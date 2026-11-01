;;; kotoni-core.el --- Core logic for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5") (json))

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'json)

(defun ktn--hiragana-to-katakana (str)
  "Convert Hiragana string STR to Katakana."
  (let ((rules '(("う゛" . "ヴ")
                 ("ゔ"   . "ヴ")
                 ("ゝ"   . "ヽ")
                 ("ゞ"   . "ヾ"))))
    ;; 例外置換ルールを順番に適用していく
    (dolist (rule rules)
      (setq str (replace-regexp-in-string (car rule) (cdr rule) str)))

    (apply #'concat
           (mapcar (lambda (c)
                     (if (and (>= c ?ぁ) (<= c ?ん))
                         (char-to-string (+ c (- ?ァ ?ぁ)))
                       (char-to-string c)))
                   str))))

;;; Dictionary Architecture

(defun ktn--parse-candidate-items (items)
  "Parse candidate items (list, vector, or hash-table) into (CANDIDATE . ANNOTATION) pairs."
  (when (hash-table-p items)
    (setq items (or (gethash "word" items)
                    (gethash "candidates" items)
                    (gethash "cands" items)
                    items)))
  (let (result)
    (dolist (item (cond
                   ((vectorp items) (append items nil))
                   ((listp items) items)
                   (t (list items))))
      (cond
       ;; skk2json で生成されたハッシュテーブル（オブジェクト）形式の候補
       ((hash-table-p item)
        (let* ((word (gethash "word" item))
               (annot (gethash "annotation" item)))
          (when word
            (push (cons word annot) result))))
       ;; 従来の文字列形式の候補
       ((stringp item)
        (let* ((clean-item (replace-regexp-in-string "\\[[^]]*\\]" "" item))
               (cand (if (string-match "^\\([^;]+\\);\\(.*\\)$" clean-item)
                         (cons (match-string 1 clean-item) (match-string 2 clean-item))
                       (cons clean-item nil))))
          (push cand result)))))
    (nreverse result)))

(defun ktn-dictionary-search (dict midasi &optional is-okuri)
  "Search MIDASI in dictionary instance DICT using pure Hash-table lookup."
  (let ((file (ktn-dictionary-path dict)))
    (when (and file (file-exists-p file))
      (if (string-suffix-p ".json" file t)
          ;; --- JSON 辞書の検索処理（ハッシュ形式に統一） ---
          (let ((data (ktn-dictionary-data dict)))
            (unless data
              (with-temp-buffer
                (insert-file-contents file)
                ;; :object-type 'hash-table でJSONオブジェクトをすべてEmacs Lispのハッシュとして展開
                (setq data (json-parse-buffer :object-type 'hash-table))
                (setf (ktn-dictionary-data dict) data)))
            (when (hash-table-p data)
              (let* ((section-key (if is-okuri "okuri_ari" "okuri_nasi"))
                     (section (gethash section-key data))
                     ;; ハッシュテーブルから見出し語を O(1) で直接取得
                     (raw-cands (cond
                                 ((hash-table-p section) (gethash midasi section))
                                 ((hash-table-p data) (gethash midasi data))
                                 (t nil))))
                (when raw-cands
                  (ktn--parse-candidate-items raw-cands)))))
        ;; --- 従来のテキスト辞書の検索処理（フォールバック） ---
        (let ((buf (ktn-dictionary-buffer dict)))
          (unless (and (bufferp buf) (buffer-live-p buf))
            (setq buf (get-buffer-create (concat " *ktn-" (file-name-nondirectory file) "*")))
            (setf (ktn-dictionary-buffer dict) buf)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert-file-contents file))
              (read-only-mode 1)))
          (when (and (bufferp buf) (buffer-live-p buf))
            (with-current-buffer buf
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward (concat "^" (regexp-quote midasi) " /") nil t)
                  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                         (body (and (string-match "/\\(.+\\)/$" line) (match-string 1 line)))
                         (clean-body (when body (replace-regexp-in-string "\\[[^]]*\\]" "" body)))
                         (filtered-cands (when clean-body (split-string clean-body "/" t))))
                    (mapcar (lambda (item)
                              (if (string-match "^\\([^;]+\\);\\(.*\\)$" item)
                                  (cons (match-string 1 item) (match-string 2 item))
                                (cons item nil)))
                            filtered-cands)))))))))))

(defun ktn-search-jisyo-combined (midasi &optional is-okuri)
  "Search MIDASI. Prioritize personal stats (frequency order), then system dictionaries."
  (let ((seen (make-hash-table :test 'equal))
        result)
    
    ;; A. 統計情報（個人辞書）からの候補取得・回数順ソート
    (let ((cands-hash (gethash midasi ktn--stats-data)))
      (when (and cands-hash (hash-table-p cands-hash))
        (let (stat-list)
          (maphash (lambda (word count)
                     (push (cons word count) stat-list))
                   cands-hash)
          (setq stat-list (sort stat-list (lambda (a b) (> (cdr a) (cdr b)))))
          (dolist (item stat-list)
            (let ((word (car item)))
              (unless (gethash word seen)
                (puthash word t seen)
                (push (list word nil 'user) result)))))))
      
    ;; B. システム辞書からの候補取得・マージ
    (dolist (dict ktn-dictionaries)
      (dolist (c (ktn-dictionary-search dict midasi is-okuri))
        (let ((word (car c))
              (annot (cdr c)))
          (unless (gethash word seen)
            (puthash word t seen)
            (push (list word annot 'system) result)))))
      
    (nreverse result)))

(defun ktn-search-jisyo-with-okuri-combined (midasi okuri-first-roma okuri-kana)
  "Search okuri-ari candidates, prioritizing personal stats."
  (let* ((search-key (concat midasi okuri-first-roma))
         (seen (make-hash-table :test 'equal))
         stat-candidates
         system-candidates)
    
    ;; 1. 統計情報（個人辞書）から送りあり用の統計を取得
    (let ((cands-hash (gethash search-key ktn--stats-data)))
      (when (and cands-hash (hash-table-p cands-hash))
        (let (lst)
          (maphash (lambda (word count)
                     (push (cons word count) lst))
                   cands-hash)
          (setq lst (sort lst (lambda (a b) (> (cdr a) (cdr b)))))
          (dolist (item lst)
            (let ((word (car item)))
              (unless (gethash word seen)
                (puthash word t seen)
                (push (list word nil 'user) stat-candidates)))))))
    
    ;; 2. システム辞書からの検索結果を取得
    (let ((raw-cands (ktn-search-jisyo-combined search-key t)))
      (dolist (cand-cell raw-cands)
        (let* ((word-orig (nth 0 cand-cell))
               (annot (nth 1 cand-cell))
               (word (concat word-orig okuri-kana))
               (source (nth 2 cand-cell)))
          (unless (gethash word seen)
            (puthash word t seen)
            (push (list word annot source) system-candidates)))))

    ;; 3. それぞれを正しい順序に戻して結合
    (append (nreverse stat-candidates) (nreverse system-candidates))))

(defun ktn--load-stats ()
  "Load ktn stats from ~/.jisyo.json safely into `ktn--stats-data`."
  (when (or (not ktn--stats-loaded-p)
            (null ktn--stats-data)
            (not (hash-table-p ktn--stats-data)))
    (let ((jisyo-path (or ktn-stats-file (expand-file-name "~/.jisyo.json"))))
      (if (and jisyo-path (file-exists-p jisyo-path))
          (condition-case err
              (progn
                (with-temp-buffer
                  (insert-file-contents jisyo-path)
                  (if (= (buffer-size) 0)
                      (setq ktn--stats-data (make-hash-table :test 'equal))
                    (setq ktn--stats-data (json-parse-buffer :object-type 'hash-table))))
                (unless (hash-table-p ktn--stats-data)
                  (setq ktn--stats-data (make-hash-table :test 'equal)))
                (setq ktn--stats-loaded-p t)
                t)
            (error
             (setq ktn--stats-data (make-hash-table :test 'equal))
             (setq ktn--stats-loaded-p t)
             t))
        (setq ktn--stats-data (make-hash-table :test 'equal))
        (setq ktn--stats-loaded-p t)
        t)))
  t)

(defun ktn--set-cursor-red ()
  (setq ktn--saved-cursor-color
        (or ktn--saved-cursor-color
            (frame-parameter nil 'cursor-color)
            (face-attribute 'cursor :background)))
  (set-cursor-color "red"))

(defun ktn--unset-cursor-red ()
  (when ktn--saved-cursor-color
    (set-cursor-color ktn--saved-cursor-color)
    (setq ktn--saved-cursor-color nil)))

;;常にフレームの最下段にサイドウィンドウとして表示する
(add-to-list 'display-buffer-alist
             (list (concat "\\`" (regexp-quote ktn-candidate-buffer-name) "\\'")
                   '(display-buffer-in-side-window)
                   '(side . bottom)
                   '(slot . 0)
                   '(window-height . fit-window-to-buffer)
                   '(window-parameters . ((no-other-window . t)
                                          (no-delete-other-windows . t)))))

(defun ktn--get-or-create-candidate-buffer ()
  "Get or create the candidate buffer."
  (get-buffer-create ktn-candidate-buffer-name))

(defun ktn--close-candidate-window ()
  "Close the window displaying the candidate buffer if it exists."
  (let ((buf (get-buffer ktn-candidate-buffer-name)))
    (when buf
      (delete-windows-on buf))))

;;;###autoload
(defun ktn-save-stats ()
  (interactive)
  "Save `ktn--stats-data` to `ktn-stats-file` in JSON format."
  (when ktn-stats-file
    (condition-case err
        (let ((json-encoding-pretty-print t))
          (with-temp-file ktn-stats-file
            (insert (json-encode ktn--stats-data))))
      (error (message "Failed to save ktn stats file: %s" err)))))

(defun ktn-setup ()
  "Initialize Ktn tables and dictionary instances safely."
  (if (not (ktn--load-stats))
      ;; 失敗時 (early return / abort)
      (progn
        (setq ktn-mode nil)
        (message "ktn-mode activation aborted due to dictionary load/parse error.")
        nil)

    ;; 成功時の処理
    ;; 1. ローマ字テーブルのセットアップ
    (unless ktn-roma-table
      (ktn-setup-roma-table))

    ;; 2. システム辞書インスタンスの設定
    (setf (ktn-dictionary-path ktn--system-jisyo-instance) ktn-jisyo-file)
    (setq ktn-dictionaries (list ktn--system-jisyo-instance))

    (let ((file (ktn-dictionary-path ktn--system-jisyo-instance)))
      (when (and file (file-exists-p file) (string-suffix-p ".json" file t))
        (with-temp-buffer
          (insert-file-contents file)
          (setf (ktn-dictionary-data ktn--system-jisyo-instance)
                (json-parse-buffer :object-type 'hash-table)))))

    ;; 3. 終了フックの登録
    (unless (memq #'ktn-save-stats kill-emacs-hook)
      (add-hook 'kill-emacs-hook #'ktn-save-stats))

    t))


(defvar ktn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'ktn-insert)
    (define-key map (kbd "C-g")       #'ktn-quit)
    (define-key map (kbd "C-j")       #'ktn-kakutei)
    (define-key map (kbd "RET")       #'ktn-kakutei-and-newline)
    (define-key map (kbd "C-m")       #'ktn-kakutei-and-newline)
    (define-key map (kbd "SPC")       #'ktn-start-henkan)
    (define-key map (kbd "C-h")       #'ktn-backspace)
    (define-key map (kbd "DEL")       #'ktn-backspace)

    (define-key map (kbd "TAB")       #'ktn-complete-headword)
    (define-key map (kbd "<tab>")     #'ktn-complete-headword)
    (define-key map (kbd "<backtab>") #'ktn-complete-headword-backward)
    (define-key map (kbd "S-TAB")     #'ktn-complete-headword-backward)

    (define-key map (kbd "<next>")    #'ktn-next-page) ;PageDown
    (define-key map (kbd "<prior>")   #'ktn-prev-page) ;PageUp
    (define-key map (kbd "q")         #'ktn-katakana-convert)
    map)
  "Keymap for `ktn-mode`.")

(defun ktn--roma-prefix-p (prefix)
  "Return non-nil if PREFIX is a prefix of any key in `ktn-roma-table`."
  (cl-some (lambda (pair)
             (string-prefix-p prefix (car pair)))
           ktn-roma-table))

(defun ktn--cleanup-headword ()
  "Clean up headword overlay, marker, and internal buffers."
  (when (overlayp (ktn--state-get :headword-overlay))
    (delete-overlay (ktn--state-get :headword-overlay))
    (ktn--state-set :headword-overlay nil))
  (when (markerp (ktn--state-get :headword-start-marker))
    (ktn--state-set :headword-start-marker nil))
  (ktn--close-candidate-window)
  (ktn--state-reset))

(defun ktn--update-inline-display ()
  "Update the inline overlay to display current input or conversion candidate."
  (if (not (memq (ktn--state-get :mode) '(headword converting)))
      (progn
        (when (overlayp (ktn--state-get :headword-overlay))
          (delete-overlay (ktn--state-get :headword-overlay))
          (ktn--state-set :headword-overlay nil))
        (ktn--hassamu-update-display))

    ;; オーバーレイがなければ作成
    (unless (and (overlayp (ktn--state-get :headword-overlay))
                 (overlay-buffer (ktn--state-get :headword-overlay)))
      (let ((pos (point)))
        (ktn--state-set :headword-overlay (make-overlay pos pos nil t t))))

    ;; カーソル位置に合わせる
    (move-overlay (ktn--state-get :headword-overlay) (point) (point))

    (cond
     ;; --- 1. ▽ 見出し語入力中モード ---
     ((eq (ktn--state-get :mode) 'headword)
      (let* ((input-str (concat (ktn--state-get :headword-buffer)
                                (ktn--state-get :roma-buffer)
                                (unless (string-empty-p (ktn--state-get :okuri-roma-first))
                                  "*")
                                (ktn--state-get :okuri-buffer)
                                (ktn--state-get :okuri-roma-buffer)))
             (display-headword (concat "▽" input-str))
             ;; 動的補完プレビューの取得
             (preview-suffix (ktn--get-current-completion-suffix)))

        (if (and preview-suffix (not (string-empty-p preview-suffix)))
            ;; プレビューがある場合：入力文字を下線付き、プレビューをプレビュー用フェイスで表示
            (progn
              (overlay-put (ktn--state-get :headword-overlay) 'before-string
                           (propertize display-headword 'face 'ktn-headword-face))
              (overlay-put (ktn--state-get :headword-overlay) 'after-string
                           (propertize preview-suffix
                                       'face 'ktn-completion-preview-face
                                       'cursor t)))
          ;; プレビューがない場合：通常の見出し語表示
          (overlay-put (ktn--state-get :headword-overlay) 'before-string
                       (propertize display-headword 'face 'ktn-headword-face))
          (overlay-put (ktn--state-get :headword-overlay) 'after-string nil))))

     ;; --- 2. ▼ 漢字変換中・候補選択モード ---
     ((eq (ktn--state-get :mode) 'converting)
      (let* ((cand (nth (ktn--state-get :candidate-index) (ktn--state-get :candidates) ))
             ;; どんなデータ構造（文字列、コンスセル、入れ子リスト）であっても漢字文字列を安全に取り出す
             (cand-str (cond
                        ((stringp cand) cand)
                        ((consp cand) (or (car cand) (format "%s" cand)))
                        ((listp cand) (format "%s" (car cand)))
                        (t (format "%s" cand))))
             (display-str (concat "▼" cand-str)))

        (overlay-put (ktn--state-get :headword-overlay) 'before-string
                     (propertize display-str 'face 'ktn-headword-face))
        ;; 変換中はプレビュー用の after-string は不要なのでクリア
        (overlay-put (ktn--state-get :headword-overlay) 'after-string nil))))))

(defun ktn--show-candidates-window ()
  "Display candidate items in a dedicated popup window at the bottom of the frame."
  (if (not (eq (ktn--state-get :mode) 'converting))
      (ktn--close-candidate-window)
    (let* ((total        (length (ktn--state-get :candidates) ))
           (page-size    (length ktn-select-keys))
           (current-page (/ (ktn--state-get :candidate-index) page-size))
           (start        (* current-page page-size))
           (end          (min total (+ start page-size)))
           parts)
      (cl-loop for i from start below end
               for cand-cell   = (nth i (ktn--state-get :candidates))
               for c-str       = (nth 0 cand-cell)
               for annot       = (nth 1 cand-cell)
               for source      = (nth 2 cand-cell)
               for source-marker = (cond
                                    ;; ((eq source 'tool) "*")
                                    ((eq source 'user) "+")
                                    (t " "))
               for display-str = (if (and annot (not (string-empty-p annot)))
                                     (concat c-str ";" annot)
                                   c-str)
               for key-char    = (nth (- i start) ktn-select-keys)
               for key-str = (propertize (format " %c " key-char) 'face 'ktn-index-face)
               for formatted = (concat source-marker " " key-str " " display-str)
               do
               (push (if (= i (ktn--state-get :candidate-index))
                         (propertize formatted 'face 'ktn-selected-face)
                       formatted)
                     parts))

      (let* ((buf (ktn--get-or-create-candidate-buffer))
             (win (display-buffer buf))

             (win-width          (if (window-live-p win)
                                     (window-width win)
                                   (frame-width)))
             (page-str           (format "(%d/%dページ)" (1+ current-page) (ceiling total (float page-size))))
             (padding            (max 0 (- win-width (string-width page-str) 2)))
             (right-aligned-page (concat (make-string padding ?\s) page-str))
             (content            (concat (mapconcat #'identity (nreverse parts) "\n")
                                         (if (> total page-size)
                                             (concat "\n" right-aligned-page)
                                           ""))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)))
        (when (window-live-p win)
          (with-selected-window win
            (fit-window-to-buffer win)))))))

(defun ktn-next-page ()
  "Jump to the next page of candidates in converting mode."
  (interactive)
  (ktn--hassamu-abort)
  (if (not (eq (ktn--state-get :mode) 'converting))
      (call-interactively #'self-insert-command)
    (let* ((total        (length (ktn--state-get :candidates) ))
           (page-size    (length ktn-select-keys))
           (current-page (/ (ktn--state-get :candidate-index) page-size))
           (max-page     (1- (ceiling total (float page-size))))
           (next-page    (if (< current-page max-page)
                             (1+ current-page)
                           0)))
      (ktn--state-set :candidate-index (* next-page page-size))
      (ktn--update-inline-display)
      (ktn--show-candidates-window)) ))

(defun ktn-prev-page ()
  "Jump to the previous page of candidates in converting mode."
  (interactive)
  (ktn--hassamu-abort)
  (if (not (eq (ktn--state-get :mode) 'converting))
      (call-interactively #'self-insert-command)
    (let* ((total        (length (ktn--state-get :candidates)))
           (page-size    (length ktn-select-keys))
           (current-page (/ (ktn--state-get :candidate-index) page-size))
           (max-page     (1- (ceiling total (float page-size))))
           (prev-page    (if (> current-page 0)
                             (1- current-page)
                           max-page)))
      (ktn--state-set :candidate-index (* prev-page page-size))
      (ktn--update-inline-display)
      (ktn--show-candidates-window)) ))

(defun ktn--process-roma-input (char-str)
  "Process CHAR-STR against `ktn-roma-buffer`."
  (let* ((new-roma (concat (ktn--state-get :roma-buffer) char-str))
         (kana (cdr (assoc new-roma ktn-roma-table))))
    (cond
     (kana (cons kana ""))

     ;; "n" の処理 (nn -> ん, na -> な etc.)
     ((and (string= (ktn--state-get :roma-buffer) "n")
           (not (member char-str '("a" "i" "u" "e" "o" "y" "n"))))
      (let ((next-kana (cdr (assoc char-str ktn-roma-table))))
        (cons (concat "ん" (or next-kana "")) (if next-kana "" char-str))))

     ;; 子音の連続による「っ」の処理
     ((and (= (length (ktn--state-get :roma-buffer)) 1)
           (string= (ktn--state-get :roma-buffer) char-str)
           (string-match-p "^[b-df-hj-np-tv-z]$" char-str))
      (cons "っ" char-str))

     ;; プレフィックスマッチ（まだ確定しないが次が続く場合）
     ((ktn--roma-prefix-p new-roma)
      (cons "" new-roma))

     ;; マッチしない場合の処理
     (t
      (if (string-empty-p (ktn--state-get :roma-buffer))
          (cons char-str "")
        (cons (ktn--state-get :roma-buffer) char-str))))))

(defun ktn--complete-search-headword (prefix)
  "Search JSON dictionaries and stats for midasi keys starting with PREFIX.
Prioritize personal stats, then system dictionaries."
  (let ((seen (make-hash-table :test 'equal))
        stats-results
        system-results)
    
    ;; 1. 個人統計データからの前方一致検索（優先）
    (maphash (lambda (midasi _)
               (when (and (string-prefix-p prefix midasi)
                          (not (gethash midasi seen)))
                 (puthash midasi t seen)
                 (push midasi stats-results)))
             ktn--stats-data)
    ;; 個人統計側は登録・使用頻度などを考慮してソート、あるいはそのまま
    (setq stats-results (sort stats-results #'string<))
    
    ;; 2. JSON システム辞書からの前方一致検索
    (dolist (dict ktn-dictionaries)
      (let ((data (ktn-dictionary-data dict)))
        (when (hash-table-p data)
          (dolist (section-key '("okuri_nasi" "okuri_ari"))
            (let ((section (gethash section-key data)))
              (when (hash-table-p section)
                (maphash (lambda (midasi _)
                           (when (and (string-prefix-p prefix midasi)
                                      (not (gethash midasi seen)))
                             (puthash midasi t seen)
                             (push midasi system-results)))
                         section)))))))
    
    (setq system-results (sort system-results #'string<))
    
    ;; 個人辞書の結果を先頭に、システム辞書の結果をその後に結合
    (append stats-results system-results)))

(defun ktn-complete-headword-backward ()
  "Cycle through headword completion candidates in reverse order using Shift-TAB."
  (interactive)
  (ktn--hassamu-abort)
  (if (not (eq (ktn--state-get :mode) 'headword))
      (call-interactively #'indent-for-tab-command)

    (unless (string-empty-p (ktn--state-get :roma-buffer))
      (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer)
                                               (ktn--state-get :roma-buffer)))
      (ktn--state-set :roma-buffer ""))
        
    ;; 補完セッションが未開始なら通常の順方向と同じように初期化して末尾から開始
    (when (or (null (ktn--state-get :completion-cands))
              (null (ktn--state-get :completion-base))
              (not (string-prefix-p (ktn--state-get :completion-base) (ktn--state-get :headword-buffer))))
      (ktn--state-set :completion-base (ktn--state-get :headword-buffer))
      (ktn--state-set :completion-index 0)
      (ktn--state-set :completion-cands (ktn--complete-search-headword (ktn--state-get :headword-buffer)))
      )
        
    (if (ktn--state-get :completion-cands)
        (let ((total (length (ktn--state-get :completion-cands))))
          ;; インデックスを逆回し（現在位置から2つ戻して調整）
          (ktn--state-set :completion-index (mod (- (ktn--state-get :completion-index) 2) total))
          (let ((completed-midasi (nth (ktn--state-get :completion-index) (ktn--state-get :completion-cands))))
            (ktn--state-set :headword-buffer completed-midasi)
            (ktn--state-set :completion-index (mod (1+ (ktn--state-get :completion-index) total)))
            (ktn--update-inline-display)
            (ktn--show-completions-window)))
      (ktn--close-candidate-window)
      (message "補完候補が見つかりません: %s" (ktn--state-get :headword-buffer))))
  )

(defun ktn--get-current-completion-suffix ()
  "現在表示されている動的補完のサフィックス文字列を返す。"
  (let ((current-input (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)
                               (unless (string-empty-p (ktn--state-get :okuri-roma-first))
                                 "*")
                               (ktn--state-get :okuri-buffer)
                               (ktn--state-get :okuri-roma-buffer))))
    (when (and (eq (ktn--state-get :mode) 'headword)
               (string-empty-p (ktn--state-get :okuri-roma-first))
               (not (string-empty-p current-input)))
      (when (or (null (ktn--state-get :completion-cands))
                (null (ktn--state-get :completion-base))
                (not (string-prefix-p (ktn--state-get :completion-base) current-input)))
        (ktn--state-set :completion-base current-input)
        (ktn--state-set :completion-index 0)
        (ktn--state-set :completion-cands (ktn--complete-search-headword current-input))
        )
      (when (ktn--state-get :completion-cands)
        (let ((first-cand (nth (ktn--state-get :completion-index) (ktn--state-get :completion-cands))))
          (when (and first-cand (string-prefix-p current-input first-cand))
            (substring first-cand (length current-input))))))))

(defun ktn-complete-headword ()
  "Complete current headword in ▽ mode using TAB.
1st TAB: Commit/cycle completion inline without showing candidates window.
2nd and subsequent TABs: Show *ktn-candidates* window."
  (interactive)
  ;; ktn-hassamu セッション中の TAB 打鍵
  (if ktn--hassamu-active-p
      (let ((target (car ktn--hassamu-current-matches))
            (start-pos (and (markerp ktn--hassamu-start-marker)
                            (marker-position ktn--hassamu-start-marker)))
            (end-pos (point)))
        (ktn--hassamu-abort)
        (when target
          (if (and start-pos end-pos (> end-pos start-pos))
              (progn
                (delete-region start-pos end-pos)
                (insert target))
            (insert target))
          (message "ktn-hassamu: '%s' を挿入しました。" target)))

    ;; 従来の ▽ モード用 TAB 補完
    (if (eq (ktn--state-get :mode) 'headword)
        (progn
          ;; 未確定のローマ字があればヘッドワードバッファに結合
          (unless (string-empty-p (ktn--state-get :roma-buffer))
            (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
            (ktn--state-set :roma-buffer ""))

          ;; 新しい補完セッションの開始判定
          (let ((is-new-session (or (null (ktn--state-get :completion-cands))
                                    (null (ktn--state-get :completion-base))
                                    (not (string-prefix-p (ktn--state-get :completion-base) (ktn--state-get :headword-buffer))))))
            (when is-new-session
              (ktn--state-set :completion-base (ktn--state-get :headword-buffer))
              (ktn--state-set :completion-index 0)
              ;; 新規セッション時は打鍵カウントをリセット
              (ktn--state-set :completion-tab-count 0)
              (ktn--state-set :completion-cands (ktn--complete-search-headword (ktn--state-get :headword-buffer)))
              )

            (if (ktn--state-get :completion-cands)
                (let ((completed-midasi (nth (ktn--state-get :completion-index) (ktn--state-get :completion-cands))))
                  ;; 打鍵カウントを進める（1発目=1, 2発目=2...）
                  (ktn--state-set :completion-tab-count (1+ (or (ktn--state-get :completion-tab-count) 0)))

                  ;; 見出し語バッファを更新
                  (ktn--state-set :headword-buffer completed-midasi)
                  (ktn--state-set :completion-index (mod (1+ (ktn--state-get :completion-index))
                                                         (length (ktn--state-get :completion-cands))))
                  (ktn--state-set :roma-buffer "")

                  ;; インライン表示の更新
                  (ktn--update-inline-display)

                  ;; ★ 肝心な制御: 2発目以降の TAB 打鍵でのみ候補ウィンドウを表示する
                  (if (>= (ktn--state-get :completion-tab-count) 2)
                      (ktn--show-completions-window)
                    (ktn--close-candidate-window)))

              (ktn--close-candidate-window)
              (message "補完候補が見つかりません: %s" (ktn--state-get :headword-buffer)))))
      (call-interactively #'indent-for-tab-command))))

(defun ktn--show-completions-window ()
  "Display completion candidates in the candidate buffer/window (max 7 items per page)."
  (if (ktn--state-get :completion-cands)
      (let* ((total        (length (ktn--state-get :completion-cands)))
             (page-size    7)
             (current-idx  (mod (1- (ktn--state-get :completion-index)) total))
             (current-page (/ current-idx page-size))
             (start        (* current-page page-size))
             (i            start)
             (end          (min total (+ start page-size)))
             (prefix       (or (ktn--state-get :completion-base) ""))
             formatted-cands)

        ;; 現在のページの7件分のみを生成
        (while (< i end)
          (let* ((cand (nth i (ktn--state-get :completion-cands)))
                 ;; ベース部分と補完部分に分割してそれぞれ face を適用
                 (base-part (if (string-prefix-p prefix cand) prefix ""))
                 (rest-part (if (string-prefix-p prefix cand)
                                (substring cand (length prefix))
                              cand))
                 (formatted-base (propertize base-part 'face 'ktn-completion-base-face))
                 (formatted-rest (propertize rest-part 'face 'ktn-completion-rest-face))
                 (cand-display (concat formatted-base formatted-rest))

                 ;; 選択状態に応じた装飾（記号とインデント）を付与
                 (item (if (= i current-idx)
                           (concat (propertize " > " 'face 'ktn-selected-face) cand-display)
                         (concat "    " cand-display))))
            (push (if (= i current-idx)
                      (propertize item 'face 'ktn-selected-face)
                    item)
                  formatted-cands)
            (setq i (1+ i))))

        (let* ((buf (ktn--get-or-create-candidate-buffer))
               (win (display-buffer buf)))
          ;; ページ表示は右寄せ
          (let* ((win-width         (if (window-live-p win)
                                       (window-width win)
                                     (frame-width)))
                 (page-str          (format "(%d/%dページ)" (1+ current-page) (ceiling total (float page-size))))
                 (padding           (max 0 (- win-width (string-width page-str) 2)))
                 (right-aligned-page (concat (make-string padding ?\s) page-str))
                 (content           (concat "【TAB補完候補】\n"
                                            (mapconcat #'identity (nreverse formatted-cands) "\n")
                                            (if (> total page-size)
                                                (concat "\n" right-aligned-page)
                                              ""))))

            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert content)))

            (when (window-live-p win)
              (with-selected-window win
                (fit-window-to-buffer win))))))
    (ktn--close-candidate-window)))

(defun ktn--reset-completion ()
  "Reset headword completion state and close completion window."
  (ktn--state-set :completion-base nil)
  (ktn--state-set :completion-tab-count 0)
  (ktn--state-set :completion-index 0)
  (ktn--state-set :completion-cands nil)
  (ktn--close-candidate-window))

(defun ktn--prompt-dict-registration ()
  "Prompt user in minibuffer to register a new word for current headword/okuri,
supporting recursive minibuffer input and sub-registrations."
  (ktn--hassamu-abort)
  (let* ((enable-recursive-minibuffers t) ; ← ミニバッファのネスト（再帰）を許可する
         (midasi (if (not (string-empty-p (ktn--state-get :okuri-roma-first)))
                     (concat (ktn--state-get :headword-buffer) (ktn--state-get :okuri-roma-first))
                   (ktn--state-get :headword-buffer)))
         (display-label (concat (ktn--state-get :headword-buffer)
                                (ktn--state-get :okuri-buffer)))
         (user-word
          (minibuffer-with-setup-hook
              (lambda ()
                (local-set-key (kbd "C-g") #'abort-recursive-edit)
                (local-set-key (kbd "C-c C-c") (lambda ()
                                                 (interactive)
                                                 (when (active-minibuffer-window)
                                                   (exit-minibuffer)))))
            (read-from-minibuffer (format "[辞書登録] '%s' に対する単語を入力 (完了は C-c C-c): "
                                         display-label)))))
    (if (and user-word (not (string-empty-p user-word)))
        (let ((pos (and (markerp (ktn--state-get :headword-start-marker))
                        (marker-position (ktn--state-get :headword-start-marker)))))
          (ktn--register-word midasi user-word)
          (ktn--cleanup-headword)
          (when pos
            (goto-char pos)
            (insert user-word))
          (message "新しい単語を辞書に登録しました: [%s] -> %s" midasi user-word))
      (ktn-quit))))

;;;###autoload
(defun ktn-quit ()
  "Cancel current input sequence or return from converting to headword mode."
  (interactive)
  (ktn--hassamu-abort)
  (cond
   ;; 1. ▼ 変換候補選択中なら ▽ 見出し語入力状態に戻る
   ((eq (ktn--state-get :mode) 'converting)
    (ktn--state-set :mode'headword)
    (ktn--state-set :candidates nil)
    (ktn--state-set :candidate-index 0)
    (message "")
    (ktn--update-inline-display))

   ;; 2. ▽ 見出し語入力中ならキャンセル
   ((eq (ktn--state-get :mode) 'headword)
    (ktn--cleanup-headword))

   ;; 3. 通常モードならバッファのローマ字バッファをクリア
   (t
    (if (string-empty-p (ktn--state-get :roma-buffer))
        (keyboard-quit)
      (ktn--state-set :roma-buffer "")
      ))))

;;;###autoload
(defun ktn-backspace ()
  "Delete a character from romaji buffer or headword buffer in ▽ mode."
  (interactive)
  (ktn--hassamu-abort)
  (ktn--reset-completion)
  (cond
   ;; ▼ 変換候補選択中なら C-g と同様に ▽ モードに戻す
   ((eq (ktn--state-get :mode) 'converting)
    (ktn--state-set :mode 'headword)
    (ktn--state-set :candidates nil)
    (ktn--state-set :candidate-index 0)
    (message "")
    (ktn--update-inline-display))

   ;; ▽ 見出し語入力中
   ((eq (ktn--state-get :mode) 'headword)
    (ktn--reset-completion)
    (cond
     ;; 1. 未確定ローマ字バッファに文字があればそこから1文字削る
     ((not (string-empty-p (ktn--state-get :okuri-roma-buffer)))
      (ktn--state-set :okuri-roma-buffer (substring (ktn--state-get :okuri-roma-buffer) 0 -1))
      (ktn--update-inline-display))

     ((not (string-empty-p (ktn--state-get :okuri-buffer)))
      (ktn--state-set :okuri-buffer (substring (ktn--state-get :okuri-buffer) 0 -1))
      (when (string-empty-p (ktn--state-get :okuri-buffer))
        (ktn--state-set :okuri-roma-first ""))
      (ktn--update-inline-display))

     ((not (string-empty-p (ktn--state-get :roma-buffer)))
      (ktn--state-set :roma-buffer (substring (ktn--state-get :roma-buffer) 0 -1))
      (ktn--update-inline-display))

     ;; 2. 見出し語バッファに文字があればそこから1文字削る
     ((not (string-empty-p (ktn--state-get :headword-buffer)))
      (ktn--state-set :headword-buffer (substring (ktn--state-get :headword-buffer) 0 -1))
      (if (string-empty-p (ktn--state-get :headword-buffer))
          (ktn--cleanup-headword)
        (ktn--update-inline-display)))

     ;; 3. どちらも空ならモードをクリーンアップ
     (t
      (ktn--cleanup-headword))))

   ;; 通常モードの場合は通常のバックスペース動作へフォールバック
   (t
    (call-interactively #'delete-backward-char))))

;;;###autoload
(defun ktn-start-henkan ()
  "Start conversion or cycle through candidates with SPC.
When candidates run out, transition to dictionary registration mode."
  (interactive)
  (ktn--hassamu-abort)
  (cond
   ;; 1. ▽ モードでの変換開始
   ((eq (ktn--state-get :mode) 'headword)
    (unless (string-empty-p (ktn--state-get :roma-buffer))
      (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
      (ktn--state-set :roma-buffer ""))
    (unless (string-empty-p (ktn--state-get :okuri-roma-buffer))
      (ktn--state-set :okuri-buffer (concat (ktn--state-get :okuri-buffer)
                                            (ktn--state-get :okuri-roma-buffer)))
      (ktn--state-set :okuri-roma-buffer ""))

    (when (string-suffix-p "n" (ktn--state-get :headword-buffer))
      (ktn--state-set :headword-buffer
            (concat (substring (ktn--state-get :headword-buffer) 0 -1) "ん")))

    (let ((cands (if (not (string-empty-p (ktn--state-get :okuri-roma-first)))
                     (ktn-search-jisyo-with-okuri-combined (ktn--state-get :headword-buffer)
                                                           (ktn--state-get :okuri-roma-first)
                                                           (ktn--state-get :okuri-buffer))
                   (ktn-search-jisyo-combined (ktn--state-get :headword-buffer)))))
      (if (not cands)
          ;; 辞書に最初からヒットしない場合 -> 辞書登録モードへ
          (ktn--prompt-dict-registration)
        ;; 候補が見つかった場合
        (ktn--state-set :henkan-count 0)
        (ktn--state-set :candidates cands)
        (ktn--state-set :candidate-index 0)
        (ktn--state-set :mode 'converting)
        (ktn--update-inline-display)
        ;; (ktn--show-candidates-window) ; 1発目で開くとウザい
        )))

   ;; 2. ▼ 変換候補選択中（SPCで次候補へ進む）
   ((eq (ktn--state-get :mode) 'converting)
    (ktn--state-set :henkan-count (1+ (ktn--state-get :henkan-count)))
    (if (>= (1+     (ktn--state-get :candidate-index))
            (length (ktn--state-get :candidates)))
        ;; 候補を全て出し切った状態でさらに SPC を押した場合 -> 辞書登録モードへ
        (ktn--prompt-dict-registration)

      (ktn--state-set :candidate-index (1+ (ktn--state-get :candidate-index)))
      (ktn--update-inline-display)
      (when (>= (ktn--state-get :henkan-count) 2)
        (ktn--show-candidates-window))))
   (t
    (insert " "))))

(defun ktn--register-word (midasi word &optional annotation)
  "Register a new WORD (with optional ANNOTATION) for MIDASI into `ktn--stats-data` and save."
  (let* ((clean-word (if annotation (concat word ";" annotation) word))
         (cands-hash (gethash midasi ktn--stats-data)))
    ;; メモリ上のハッシュに初期カウント1（または既存＋1）で追加・更新
    (unless cands-hash
      (setq cands-hash (make-hash-table :test 'equal))
      (puthash midasi cands-hash ktn--stats-data))
    (puthash clean-word
             (1+ (gethash clean-word cands-hash 0)) ; current-count
             cands-hash)

    (ktn-save-stats)          ; JSON ファイル（~/.jisyo.json）へ永続化
    (message "辞書に登録しました: [%s] -> %s" midasi clean-word)))

(defun ktn-prev-candidate ()
  "Select previous candidate in converting (▼) mode."
  (interactive)
  (ktn--hassamu-abort)
  (if (not (eq (ktn--state-get :mode) 'converting))
      (ktn-insert)

    ;; 先頭なら末尾へループさせる場合
    (ktn--state-set :candidate-index (mod (1-     (ktn--state-get :candidate-index))
                                          (length (ktn--state-get :candidates))))
    (ktn--update-inline-display)
    (ktn--show-candidates-window)))

;;;###autoload
(defun ktn-kakutei ()
  "Commit current headword or conversion result and insert into buffer."
  (interactive)

  ;; ▼ 確定の瞬間に、もし直前まで ktn-hassamu セッション中であれば一度きれいに片付ける
  (ktn--hassamu-abort)

  (when (ktn--state-get :mode)
    (let* (;; 送りありの場合は key に okuri-first-roma を含める
           (midasi (if (not (string-empty-p (ktn--state-get :okuri-roma-first)))
                       (concat (ktn--state-get :headword-buffer) (ktn--state-get :okuri-roma-first))
                     (ktn--state-get :headword-buffer)))
           (text (cond
                  ((eq (ktn--state-get :mode) 'converting)
                   (car (nth (ktn--state-get :candidate-index) (ktn--state-get :candidates))))
                  (t
                   (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)
                           (ktn--state-get :okuri-buffer)
                           (ktn--state-get :okuri-roma-buffer)))))
           (pos (and (markerp (ktn--state-get :headword-start-marker))
                     (marker-position (ktn--state-get :headword-start-marker)))))
      
      ;; ▼ 変換候補から確定された場合に統計情報を記録し、即座にファイル保存する
      (when (eq (ktn--state-get :mode) 'converting)
        (let* ((cands-hash (gethash midasi ktn--stats-data)))
          (unless cands-hash
            (setq cands-hash (make-hash-table :test 'equal))
            (puthash midasi cands-hash ktn--stats-data))
          (let ((current-count (gethash text cands-hash 0)))
            (puthash text (1+ current-count) cands-hash))
          ;; 即座に JSON ファイルへ永続化
          (ktn-save-stats)
          (message "辞書に登録・更新しました: [%s] -> %s" midasi text)))

      (ktn--cleanup-headword)
      (when pos
        (goto-char pos)
        (insert text))
      (ktn--hassamu-on-kakutei text) )))

;;;###autoload
(defun ktn-kakutei-and-newline ()
  "Commit current input and insert a newline."
  (interactive)
  (ktn--hassamu-abort)
  (when (ktn--state-get :mode)
    (ktn-kakutei))
  (newline))

;;;###autoload
(defun ktn-insert ()
  "Insert router supporting normal and headword input modes."
  (interactive)
  (barf-if-buffer-read-only)
  (ktn--hassamu-abort)
  (let* ((ch       last-command-event)
         (is-upper (and (characterp ch) (<= ?A ch) (<= ch ?Z)))
         (lower-ch (if is-upper (+ ch 32) ch))
         (is-alpha (and (characterp lower-ch) (<= ?a lower-ch) (<= lower-ch ?z))))

    (cond
     ;; ▼ 変換候補選択中（converting モード）のキー入力処理
     ;; (1). 選択キー（ktn-select-keys: 1〜7など）が押された場合: 対応する候補を選択して確定する
     ;; (2). 'x' または 'X' が押された場合: 前の候補へ戻る (ktn-prev-candidate)
     ;; (3). その他のキーが押された場合: 現在の変換を一度確定（ktn-kakutei）させた後、
     ;;    入力されたキー（ch）をイベントキュー（unread-command-events）に戻し、
     ;;    次の入力として自然に再処理させる
     ((eq (ktn--state-get :mode) 'converting)
      (let* ((page-size    (length ktn-select-keys))
             (current-page (/ (ktn--state-get :candidate-index) page-size))
             (page-start   (* current-page page-size))
             (key-idx      (cl-position ch ktn-select-keys)))
        (cond
         ;; (1). 直接選択キー
         (key-idx
          (let ((target-idx (+ page-start key-idx)))
            (when (< target-idx (length (ktn--state-get :candidates)))
              (ktn--state-set :candidate-index target-idx)
              (ktn-kakutei))))
         ;; (2). 前の候補へ戻るキー
         ((or (eq ch ?x) (eq ch ?X))
          (ktn-prev-candidate))
         ;; (3). 確定して次の入力へフォールバック
         (t
          (ktn-kakutei)
          (push ch unread-command-events)
          ))))

     ;; 1. 通常モード時に大文字が入力されたら ▽モードを開始
     ((and (null (ktn--state-get :mode)) is-upper)
      (unless (string-empty-p (ktn--state-get :roma-buffer))
        (insert (ktn--state-get :roma-buffer))
        (ktn--state-set :roma-buffer ""))

      (ktn--state-reset)
      (ktn--state-set :mode 'headword)
      (ktn--state-set :headword-start-marker (point-marker))
      (let* ((char-str (char-to-string lower-ch))
             (res (ktn--process-roma-input char-str)))
        (ktn--state-set :headword-buffer (car res))
        (ktn--state-set :roma-buffer (cdr res))
        (ktn--update-inline-display)))

     ;; 2. ▽モード時の入力
     ((eq (ktn--state-get :mode) 'headword)
      (ktn--reset-completion)
      (if is-upper
          ;; ★ここで大文字が来たら「送り仮名」の入力に切り替える
          (progn
            (unless (string-empty-p (ktn--state-get :roma-buffer))
              (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
              (ktn--state-set :roma-buffer ""))
            (let ((char-str (char-to-string lower-ch)))
              (ktn--state-set :okuri-roma-first char-str)
              (ktn--state-set :okuri-roma-buffer char-str)
              (ktn--state-set :okuri-buffer "")
              (ktn--update-inline-display)
              (let ((res (ktn--process-roma-input char-str)))
                (unless (string-empty-p (car res))
                  (ktn--state-set :okuri-buffer (car res))
                  (ktn--state-set :okuri-roma-buffer (cdr res))
                  (ktn--update-inline-display)
                  (ktn-start-henkan)))))

        ;; 小文字入力（または強制小文字化された入力）
        (if (string-empty-p (ktn--state-get :okuri-roma-first))
            (let* ((char-str (if is-alpha (char-to-string lower-ch) (char-to-string ch)))
                   (res (ktn--process-roma-input char-str)))
              (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer) (car res)))
              (ktn--state-set :roma-buffer (cdr res))
              (ktn--update-inline-display))
          (let* ((char-str (if is-alpha (char-to-string lower-ch) (char-to-string ch)))
                 (saved-roma (ktn--state-get :roma-buffer)))
            (ktn--state-set :roma-buffer (ktn--state-get :okuri-roma-buffer))
            (let ((res (ktn--process-roma-input char-str)))
              (ktn--state-set :okuri-buffer (concat (ktn--state-get :okuri-buffer) (car res)))
              (ktn--state-set :okuri-roma-buffer (cdr res))
              (ktn--state-set :roma-buffer saved-roma)
              (ktn--update-inline-display)
              (unless (string-empty-p (ktn--state-get :okuri-buffer))
                (ktn-start-henkan)))))))

     ;; 3. 通常モード時の入力
     (t
      (let* ((char-str (if is-alpha (char-to-string lower-ch) (char-to-string ch)))
             (res (ktn--process-roma-input char-str)))
        (unless (string-empty-p (car res))
          (insert (car res)))
        (ktn--state-set :roma-buffer (cdr res))
        )))))

(defun ktn-katakana-convert ()
  "Convert current headword in ▽ mode to Katakana and commit."
  (interactive)
  (ktn--hassamu-abort)
  (if (eq (ktn--state-get :mode) 'headword)
      (let ((kata-text (ktn--hiragana-to-katakana
                        (concat (ktn--state-get :headword-buffer)
                                (ktn--state-get :roma-buffer)
                                (ktn--state-get :okuri-buffer)
                                (ktn--state-get :okuri-roma-buffer))))
            (pos (and (markerp (ktn--state-get :headword-start-marker))
                      (marker-position (ktn--state-get :headword-start-marker)))))
        (ktn--cleanup-headword)
        (message "")
        (when pos
          (goto-char pos)
          (insert kata-text)))
    (ktn-insert)))

;;;###autoload
(define-minor-mode ktn-mode
  "Toggle Ktn mode."
  :lighter ktn-lighter
  :global nil
  :keymap ktn-mode-map
  (if ktn-mode
      (progn
        (ktn-setup)
        (ktn--set-cursor-red))
    (ktn--state-reset)
    (ktn--unset-cursor-red)
    (ktn--cleanup-headword)
    (ktn--hassamu-abort))
  (setq ktn-lighter (if ktn-mode ktn-indicator ""))
  (force-mode-line-update))

(provide 'kotoni-core)
;;; kotoni-core.el ends here
