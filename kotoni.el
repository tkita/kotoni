;;; kotoni.el --- Yet another SKK implementation for GNU Emacs 30 -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5") (json))

;;; Commentary:
;; Minimalist SKK clone leveraging lexical binding and cl-lib.
;; Named Kotoni (琴似), Sapporo City, Japan.
;;
;; Example configuration:
;;   (add-to-list 'load-path "/path/to/kotoni")
;;   (setq ktn-user-roma-table '(("z," . "‥") ("z." . "…")))
;;   (setq ktn-jisyo-file "/usr/share/skk/SKK-JISYO.L.json")
;;   (require 'kotoni)
;;   (global-set-key (kbd "C-x C-j") #'ktn-mode)

;;; Code:
(require 'cl-lib)
(require 'json)

(defgroup ktn nil
  "Minimal SKK clone focusing on modeline and basic insertion."
  :group 'input-method)

(defface ktn-headword-face
  '((t (:inherit underline)))
  "Face for ▽/▼ headword overlay display."
  :group 'ktn)

(defface ktn-selected-face
  '((t (:inherit highlight)))
  "Face for the currently selected candidate in the echo area list."
  :group 'ktn)

(defcustom ktn-indicator " [▼]"
  "Modeline indicator when `ktn-mode` is active."
  :type 'string)

(defcustom ktn-jisyo-file (expand-file-name "~/.emacs.d/skk-large-jisyo")
  "Path to SKK Dictionary file (Text or JSON)."
  :type 'file
  :group 'ktn)

;;; 個人辞書（統計情報）の設定とデータ構造
(defcustom ktn-stats-file (expand-file-name "~/.jisyo.json")
  "Path to JSON file storing user conversion statistics."
  :type 'file)

(defvar ktn--stats-data (make-hash-table :test 'equal)
  "Emacs internal hash table for conversion statistics.
Key: midasi (string)
Value: hash-table (Key: candidate (string), Value: count (integer))")

;;; Dictionary Instance Architecture

(cl-defstruct ktn-dictionary
  path
  buffer
  data)

(defvar ktn-dictionaries nil
  "List of `ktn-dictionary` instances used for search orchestration.")

(defvar ktn--system-jisyo-instance
  (make-ktn-dictionary :path nil :buffer nil :data nil)
  "System dictionary instance.")

(defvar ktn-select-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7)
  "Direct selection keys assigned to candidates in converting mode.")

;; 基本的なローマ字かな変換テーブル
(defvar ktn-default-roma-table
  '(("a"    . "あ")     ("i"   . "い")      ("u"    . "う")     ("e"   . "え")     ("o"    . "お")
    ("ba"   . "ば")     ("bi"  . "び")      ("bu"   . "ぶ")     ("be"  . "べ")     ("bo"   . "ぼ") 
    ("bya"  . "びゃ")   ("byi" . "びぃ")    ("byu"  . "びゅ")   ("bye" . "びぇ")   ("byo"  . "びょ")   
    ("cha"  . "ちゃ")   ("chi" . "ち")      ("chu"  . "ちゅ")   ("che" . "ちぇ")   ("cho"  . "ちょ") 
    ("cya"  . "ちゃ")   ("cyi" . "ちぃ")    ("cyu"  . "ちゅ")   ("cye" . "ちぇ")   ("cyo"  . "ちょ") 
    ("da"   . "だ")     ("di"  . "ぢ")      ("du"   . "づ")     ("de"  . "で")     ("do"   . "ど") 
    ("dha"  . "でゃ")   ("dhi" . "でぃ")    ("dhu"  . "でゅ")   ("dhe" . "でぇ")    ("dho" . "でょ")   
                                            ("dwu"  . "どぅ")  
    ("dya"  . "ぢゃ")   ("dyi" . "ぢぃ")    ("dyu"  . "ぢゅ")   ("dye" . "ぢぇ")   ("dyo"  . "ぢょ") 
    ("fa"   . "ふぁ")   ("fi"  . "ふぃ")    ("fu"   . "ふ")     ("fe"  . "ふぇ")   ("fo"   . "ふぉ") 
    ("fya"  . "ふゃ")   ("fyi" . "ふぃ")    ("fyu"  . "ふゅ")   ("fye" . "ふぇ")   ("fyo"  . "ふょ")
    ("ga"   . "が")     ("gi"  . "ぎ")      ("gu"   . "ぐ")     ("ge"  . "げ")     ("go"   . "ご")
    ("gwa"  . "ぐゎ")   ("gwi" . "ぐうぃ")  ("gwu"  . "ぐうぅ") ("gwe" . "ぐうぇ") ("gwo"  . "ぐうぉ")
    ("gya"  . "ぎゃ")   ("gyi" . "ぎぃ")    ("gyu"  . "ぎゅ")   ("gye" . "ぎぇ")   ("gyo"  . "ぎょ") 
    ("ha"   . "は")     ("hi"  . "ひ")      ("hu"   . "ふ")     ("he"  . "へ")     ("ho"   . "ほ")
    ("hya"  . "ひゃ")   ("hyi" . "ひぃ")    ("hyu"  . "ひゅ")   ("hye" . "ひぇ")   ("hyo"  . "ひょ")
    ("ja"   . "じゃ")   ("ji"  . "じ")      ("ju"   . "じゅ")   ("je"  . "じぇ")   ("jo"   . "じょ")
    ("jya"  . "じゃ")   ("jyi" . "じぃ")    ("jyu"  . "じゅ")   ("jye" . "じぇ")   ("jyo"  . "じょ")
    ("ka"   . "か")     ("ki"  . "き")      ("ku"   . "く")     ("ke"  . "け")     ("ko"   . "こ")
    ("kwa"  . "くゎ")   ("kwi" . "くうぃ")  ("kwu"  . "くうぅ") ("kwe" . "くうぇ") ("kwo"  . "くうぉ")
    ("kya"  . "きゃ")   ("kyi" . "きぃ")    ("kyu"  . "きゅ")   ("kye" . "きぇ")   ("kyo"  . "きょ")
    ("la"   . "ぁ")     ("li"  . "ぃ")      ("lu"   . "ぅ")     ("le"  . "ぇ")     ("lo"   . "ぉ")
    ("lka"  . "ヵ")                                             ("lke" . "ヶ")
                                            ("ltsu" . "っ")
                                            ("ltu"  . "っ")
    ("lwa"  . "ゎ")
    ("lya"  . "ゃ")                         ("lyu"  . "ゅ")                        ("lyo"  . "ょ")
    ("ma"   . "ま")     ("mi"  . "み")      ("mu"   . "む")     ("me"  . "め")     ("mo"   . "も")
    ("mya"  . "みゃ")   ("myi" . "みぃ")    ("myu"  . "みゅ")   ("mye" . "みぇ")   ("myo"  . "みょ")
    ("n'"   . "ん")
    ("na"   . "な")     ("ni"  . "に")      ("nu"   . "ぬ")     ("ne"  . "ね")     ("no"   . "の")
    ("nn"   . "ん")
    ("nya"  . "にゃ")   ("nyi" . "にぃ")    ("nyu"  . "にゅ")   ("nye" . "にぇ")   ("nyo"  . "にょ")
    ("pa"   . "ぱ")     ("pi"  . "ぴ")      ("pu"   . "ぷ")     ("pe"  . "ぺ")     ("po"   . "ぽ")
    ("pya"  . "ぴゃ")   ("pyi" . "ぴぃ")    ("pyu"  . "ぴゅ")   ("pye" . "ぴぇ")   ("pyo"  . "ぴょ")
    ("ra"   . "ら")     ("ri"  . "り")      ("ru"   . "る")     ("re"  . "れ")     ("ro"   . "ろ")
    ("rya"  . "りゃ")   ("ryi" . "りぃ")    ("ryu"  . "りゅ")   ("rye" . "りぇ")   ("ryo"  . "りょ")
    ("sa"   . "さ")     ("si"  . "し")      ("su"   . "す")     ("se"  . "せ")     ("so"   . "そ") 
    ("sha"  . "しゃ")   ("shi" . "し")      ("shu"  . "しゅ")   ("she" . "しぇ")   ("sho"  . "しょ")   
    ("sya"  . "しゃ")   ("syi" . "しぃ")    ("syu"  . "しゅ")   ("sye" . "しぇ")   ("syo"  . "しょ") 
    ("ta"   . "た")     ("ti"  . "ち")      ("tu"   . "つ")     ("te"  . "て")     ("to"   . "と")
    ("tha"  . "てぁ")   ("thi" . "てぃ")    ("thu"  . "てゅ")   ("the" . "てぇ")   ("tho"  . "てょ")   
    
    ("tsa"  . "つぁ")   ("tsi" . "つぃ")    ("tsu"  . "つ")     ("tse" . "つぇ")   ("tso"  . "つぉ")
                                            ("twu"  . "とぅ")
    ("tya"  . "ちゃ")   ("tyi" . "ちぃ")    ("tyu"  . "ちゅ")   ("tye" . "ちぇ")   ("tyo"  . "ちょ")
    ("va"   . "う゛ぁ") ("vi"  . "う゛ぃ")  ("vu"   . "う゛")   ("ve"  . "う゛ぇ") ("vo"   . "う゛ぉ")
    ("vya"  . "う゛ゃ") ("vyi" . "う゛ぃ")  ("vyu"  . "う゛ゅ") ("vye" . "う゛ぇ") ("vyo"  . "う゛ょ")
    ("wa"   . "わ")     ("wi"  . "うぃ")    ("wu"   . "う")     ("we"  . "うぇ")   ("wo"   . "を")
    ("wha"  . "ふぁ")   ("whi" . "ふぃ")    ("wyu"  . "ふゅ")   ("whe" . "ふぇ")   ("who"  . "ふぉ")
    ("xa"   . "ぁ")     ("xi"  . "ぃ")      ("xu"   . "ぅ")     ("xe"  . "ぇ")     ("xo"   . "ぉ")
    ("xka"  . "ヵ")                                             ("xke" . "ヶ")
    ("xtsu" . "っ")
    ("xtu"  . "っ")
    ("xwa"  . "ゎ")
    ("xwe"  . "ゑ")     ("xwi" . "ゐ")
    ("xya"  . "ゃ")                         ("xyu" . "ゅ")                         ("xyo"  . "ょ")
    ("ya"   . "や")                         ("yu"  . "ゆ")      ("ye"  . "いぇ")   ("yo"   . "よ") 
    ("za"   . "ざ")     ("zi"  . "じ")      ("zu"  . "ず")      ("ze"  . "ぜ")     ("zo"   . "ぞ")
    ("zya"  . "じゃ")   ("zyi" . "じぃ")    ("zyu" . "じゅ")    ("zye" . "じぇ")   ("zyo"  . "じょ")

    ("zh" . "←") ("zj" . "↓") ("zk"  . "↑") ("zl" . "→")
    ("zH" . "⇐") ("zJ" . "⇓") ("zK" . "⇑")  ("zL" . "⇒")
    ("z~"  . "〜")

    ("!" . "！") ("," . "、") ("-" . "ー") ("." . "。")
    (":" . "：") (";" . "；") ("?" . "？") ("[" . "「") ("]" . "」"))
  "Default Romaji-to-Kana conversion table.")

(defvar ktn-roma-table nil
  "Merged Romaji-to-Kana conversion table used for input processing.")

(defun ktn-setup-roma-table ()
  "Merge `ktn-user-roma-table` and `ktn-default-roma-table` into `ktn-roma-table`."
  (let ((table nil))
    (dolist (elt ktn-user-roma-table)
      (push elt table))
    (dolist (elt ktn-default-roma-table)
      (unless (assoc (car elt) ktn-user-roma-table)
        (push elt table)))
    (setq ktn-roma-table (nreverse table))))

(defcustom ktn-user-roma-table nil
  "User-defined Romaji-to-Kana conversion table."
  :type '(repeat (cons string string))
  :set (lambda (symbol value)
         (set-default symbol value)
         (ktn-setup-roma-table))
  :group 'ktn)

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
  (let ((result '()))
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
  "Search MIDASI in dictionary instance DICT supporting both JSON and Text formats."
  (let ((file (ktn-dictionary-path dict)))
    (when (and file (file-exists-p file))
      (if (string-suffix-p ".json" file t)
          ;; --- JSON 辞書の検索処理 ---
          (let ((data (ktn-dictionary-data dict)))
            (unless data
              (with-temp-buffer
                (insert-file-contents file)
                (setq data (json-parse-buffer :object-type 'hash-table))
                (setf (ktn-dictionary-data dict) data)))
            (let* ((section (cond
                             (is-okuri (or (gethash "okuri_ari" data)
                                           (gethash "okuri_nasi" data)
                                           data))
                             (t (or (gethash "okuri_nasi" data)
                                    data))))
                   (raw-cands (and section (gethash midasi section))))
              (when raw-cands
                (ktn--parse-candidate-items raw-cands))))
        ;; --- 従来のテキスト辞書の検索処理 ---
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
        (result '()))
    
    ;; A. 統計情報（個人辞書）からの候補取得・回数順ソート
    (let ((cands-hash (gethash midasi ktn--stats-data)))
      (when (and cands-hash (hash-table-p cands-hash))
        (let ((stat-list '()))
          (maphash (lambda (word count)
                     (push (cons word count) stat-list))
                   cands-hash)
          (setq stat-list (sort stat-list (lambda (a b) (> (cdr a) (cdr b)))))
          (dolist (item stat-list)
            (let ((word (car item)))
              (unless (gethash word seen)
                (puthash word t seen)
                (push (cons word nil) result)))))))
      
    ;; B. システム辞書からの候補取得・マージ
    (dolist (dict ktn-dictionaries)
      (let ((cands (ktn-dictionary-search dict midasi is-okuri)))
        (dolist (c cands)
          (unless (gethash (car c) seen)
            (puthash (car c) t seen)
            (push c result)))))
      
    (nreverse result)))

(defun ktn-search-jisyo-with-okuri-combined (midasi okuri-first-roma okuri-kana)
  "Search okuri-ari candidates, prioritizing personal stats."
  (let* ((search-key (concat midasi okuri-first-roma))
         (seen (make-hash-table :test 'equal))
         (stat-candidates '())
         (system-candidates '()))
    
    ;; 1. 統計情報（個人辞書）から送りあり用の統計を取得
    (let ((cands-hash (gethash search-key ktn--stats-data)))
      (when (and cands-hash (hash-table-p cands-hash))
        (let ((lst '()))
          (maphash (lambda (word count)
                     (push (cons word count) lst))
                   cands-hash)
          (setq lst (sort lst (lambda (a b) (> (cdr a) (cdr b)))))
          (dolist (item lst)
            (let ((word (car item)))
              (unless (gethash word seen)
                (puthash word t seen)
                (push (cons word nil) stat-candidates)))))))
    
    ;; 2. システム辞書からの検索結果を取得
    (let ((raw-cands (ktn-search-jisyo-combined search-key t)))
      (dolist (cand-cell raw-cands)
        (let ((word (concat (car cand-cell) okuri-kana))
              (annot (cdr cand-cell)))
          (unless (gethash word seen)
            (puthash word t seen)
            (push (cons word annot) system-candidates)))))
    
    ;; 3. それぞれを正しい順序に戻して結合
    (append (nreverse stat-candidates) (nreverse system-candidates))))

(defun ktn--load-stats ()
  "Load ktn stats from ~/.jisyo.json safely into `ktn--stats-data`.
If a JSON parse error occurs, show the error details in a dedicated popup window."
  (let ((jisyo-path (expand-file-name "~/.jisyo.json")))
    (if (file-exists-p jisyo-path)
        (condition-case err
            (progn
              (with-temp-buffer
                (insert-file-contents jisyo-path)
                (setq ktn--stats-data (json-parse-buffer :object-type 'hash-table)))
              t)
          (json-parse-error
           (let* ((error-desc (error-message-string err)) ; エラー内容の文字列を確実に取得
                  (line (nth 2 err))
                  (col (nth 3 err))
                  (err-msg (format "[Kotoni JSONパースエラー]\n\nファイル: %s\nエラー詳細: %s\n発生位置: 行 %s, 列 %s\n\n~/.jisyo.json の構文を確認してください。"
                                   jisyo-path error-desc line col)))
             (with-current-buffer (get-buffer-create "*ktn-error*")
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert err-msg)))
             (pop-to-buffer "*ktn-error*")
             (setq ktn--stats-data (make-hash-table :test 'equal))
             nil))
          (error
           (let ((err-msg (format "[Kotoni 読み込みエラー]\n\nファイル: %s\n詳細: %s" jisyo-path err)))
             (with-current-buffer (get-buffer-create "*ktn-error*")
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert err-msg)))
             (pop-to-buffer "*ktn-error*")
             (setq ktn--stats-data (make-hash-table :test 'equal))
             nil)))
      (progn
        (setq ktn--stats-data (make-hash-table :test 'equal))
        t))))

;; ウィンドウ管理

;; 隠しバッファ: バッファ名の先頭にスペースを入れると、バッファ一覧（C-x b など）に表示されない
(defconst ktn-candidate-buffer-name " *ktn-candidates*"
  "Name of the temporary buffer used for displaying conversion candidates.")

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
  ;; 1. ローマ字テーブルのセットアップ
  (unless ktn-roma-table
    (ktn-setup-roma-table))

  ;; 2. システム辞書インスタンスの設定
  (setf (ktn-dictionary-path ktn--system-jisyo-instance) ktn-jisyo-file
        (ktn-dictionary-data ktn--system-jisyo-instance) nil)
  (setq ktn-dictionaries (list ktn--system-jisyo-instance))

  ;; 3. 統計情報のロード（失敗時は ktn-mode の有効化を中止）
  (if (ktn--load-stats)
      (progn
        (add-hook 'kill-emacs-hook #'ktn-save-stats)
        t)
    (setq ktn-mode nil)
    (message "ktn-mode activation aborted due to dictionary load/parse error.")
    nil))

(defvar-local ktn-mode nil
  "Non-nil if Ktn mode is enabled in the current buffer.")

(defvar-local ktn-state nil
  "Current input state of Ktn. Either nil, 'headword (▽), or 'converting (▼).")

(defvar-local ktn-candidates nil
  "List of (CANDIDATE . ANNOTATION) candidate pairs for current conversion.")

(defvar-local ktn-candidate-index 0
  "Current index in `ktn-candidates`.")

(defvar-local ktn-roma-buffer ""
  "Buffer storing unconsumed Romaji sequence.")

(defvar-local ktn-headword-buffer ""
  "Buffer storing accumulated headword in ▽/▼ mode.")

(defvar-local ktn-headword-start-marker nil
  "Marker pointing to the start of headword input in the buffer.")

(defvar-local ktn-headword-overlay nil
  "Overlay used for ▽/▼ headword display.")

(defvar-local ktn-okuri-buffer ""
  "Buffer storing the okurigana (送り仮名) during okuri-ari conversion.")

(defvar-local ktn-okuri-roma-buffer ""
  "Buffer storing unconsumed Romaji sequence for okurigana.")

(defvar-local ktn--completion-cands nil
  "List of headword completion candidates matching current prefix.")

(defvar-local ktn--completion-index 0
  "Current index in `ktn--completion-cands`.")

(defvar-local ktn--completion-base nil
  "Base headword string before completion started.")

(defvar ktn-okuri-roma-first ""
  "Buffer storing the first romaji character of okurigana.")

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

(defvar ktn-lighter ""
  "Dynamic modeline lighter for `ktn-mode`.")

(defun ktn--roma-prefix-p (prefix)
  "Return non-nil if PREFIX is a prefix of any key in `ktn-roma-table`."
  (cl-some (lambda (pair)
             (string-prefix-p prefix (car pair)))
           ktn-roma-table))

(defun ktn--cleanup-headword ()
  "Clean up headword overlay, marker, and internal buffers."
  (when (overlayp ktn-headword-overlay)
    (delete-overlay ktn-headword-overlay)
    (setq ktn-headword-overlay nil))
  (when (markerp ktn-headword-start-marker)
    (set-marker ktn-headword-start-marker nil)
    (setq ktn-headword-start-marker nil))
  (ktn--close-candidate-window)
  (setq ktn-state nil
        ktn--completion-cands nil
        ktn--completion-index 0
        ktn--completion-base nil
        ktn-roma-buffer ""
        ktn-headword-buffer ""
        ktn-okuri-buffer ""
        ktn-okuri-roma-buffer ""
        ktn-okuri-roma-first ""
        ktn-candidates nil
        ktn-candidate-index 0))

(defun ktn--update-inline-display ()
  "Update the headword overlay at `ktn-headword-start-marker`."
  (when (markerp ktn-headword-start-marker)
    (unless (and ktn-headword-overlay (overlay-buffer ktn-headword-overlay))
      (setq ktn-headword-overlay (make-overlay ktn-headword-start-marker ktn-headword-start-marker))
      (overlay-put ktn-headword-overlay 'window (selected-window)))
    (move-overlay ktn-headword-overlay ktn-headword-start-marker ktn-headword-start-marker)
    (let* ((prefix (if (eq ktn-state 'converting) "▼" "▽"))
           (body (cond
                  ((eq ktn-state 'converting)
                   (car (nth ktn-candidate-index ktn-candidates)))
                  (t
                   (concat ktn-headword-buffer ktn-roma-buffer
                           (unless (string-empty-p ktn-okuri-roma-first) "*")
                           ktn-okuri-buffer ktn-okuri-roma-buffer)))))
      (overlay-put ktn-headword-overlay 'before-string
                   (propertize (concat prefix body)
                               'face 'ktn-headword-face)))))

(defface ktn-index-face
  '((t (:background "white" :foreground "black" :underline t)))
  "Face for candidate selection index keys."
  :group 'kotoni)

(defun ktn--show-candidates-window ()
  "Display candidate items in a dedicated popup window at the bottom of the frame."
  (if (eq ktn-state 'converting)
      (let* ((total (length ktn-candidates))
             (page-size (length ktn-select-keys))
             (current-page (/ ktn-candidate-index page-size))
             (start (* current-page page-size))
             (end (min total (+ start page-size)))
             (parts '()))
        
        ;; 候補リストの作成
        (let ((i start))
          (while (< i end)
            (let* ((cand-cell (nth i ktn-candidates))
                   (c-str (car cand-cell))
                   (annot (cdr cand-cell))
                   (key-char (nth (- i start) ktn-select-keys))
                   (display-str (if (and annot (not (string-empty-p annot)))
                                    (concat c-str ";" annot)
                                  c-str))
                   ;; 白背景・下線付きの数字インデックス
                   (key-str (propertize (format " %c " key-char) 'face 'ktn-index-face))
                   (formatted (concat key-str " " display-str)))
              (if (= i ktn-candidate-index)
                  (push (propertize formatted 'face 'ktn-selected-face) parts)
                (push formatted parts)))
            (setq i (1+ i))))
        
        ;; 1. バッファを取得し、先にウィンドウを表示・確定させる
        (let* ((buf (ktn--get-or-create-candidate-buffer))
               (win (display-buffer buf)))
          
          ;; 2. 確定したウィンドウの幅をもとに右寄せ用のパディングを計算
          (let* ((win-width (if (window-live-p win) (window-width win) (frame-width)))
                 (page-str (format "(%d/%dページ)" (1+ current-page) (ceiling total (float page-size))))
                 ;; 折り返しを防ぐため少し余裕（-2）を持たせて右端へ寄せる
                 (padding (max 0 (- win-width (string-width page-str) 2)))
                 (right-aligned-page (concat (make-string padding ?\s) page-str))
                 
                 ;; 候補一覧と右寄せページ情報を結合
                 (content (concat (mapconcat #'identity (nreverse parts) "\n")
                                  (if (> total page-size) (concat "\n" right-aligned-page) ""))))
            
            ;; 3. 正しい内容をバッファに書き込む
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert content)))
            
            ;; 4. ウィンドウの高さを内容に合わせて再調整
            (when (window-live-p win)
              (with-selected-window win
                (fit-window-to-buffer win))))))
    
    ;; 変換中でなければウィンドウを閉じる
    (ktn--close-candidate-window)))

(defun ktn-next-page ()
  "Jump to the next page of candidates in converting mode."
  (interactive)
  (if (eq ktn-state 'converting)
      (let* ((total (length ktn-candidates))
             (page-size (length ktn-select-keys))
             (current-page (/ ktn-candidate-index page-size))
             (max-page (1- (ceiling total (float page-size))))
             (next-page (if (< current-page max-page) (1+ current-page) 0)))
        (setq ktn-candidate-index (* next-page page-size))
        (ktn--update-inline-display)
        (ktn--show-candidates-window))
    (call-interactively #'self-insert-command)))

(defun ktn-prev-page ()
  "Jump to the previous page of candidates in converting mode."
  (interactive)
  (if (eq ktn-state 'converting)
      (let* ((total (length ktn-candidates))
             (page-size (length ktn-select-keys))
             (current-page (/ ktn-candidate-index page-size))
             (max-page (1- (ceiling total (float page-size))))
             (prev-page (if (> current-page 0) (1- current-page) max-page)))
        (setq ktn-candidate-index (* prev-page page-size))
        (ktn--update-inline-display)
        (ktn--show-candidates-window))
    (call-interactively #'self-insert-command)))

(defun ktn--process-roma-input (char-str)
  "Process CHAR-STR against `ktn-roma-buffer`."
  (let* ((new-roma (concat ktn-roma-buffer char-str))
         (kana (cdr (assoc new-roma ktn-roma-table))))
    (cond
     (kana (cons kana ""))

     ;; "n" の処理 (nn -> ん, na -> な etc.)
     ((and (string= ktn-roma-buffer "n")
           (not (member char-str '("a" "i" "u" "e" "o" "y" "n"))))
      (let ((next-kana (cdr (assoc char-str ktn-roma-table))))
        (cons (concat "ん" (or next-kana "")) (if next-kana "" char-str))))

     ;; 子音の連続による「っ」の処理
     ((and (= (length ktn-roma-buffer) 1)
           (string= ktn-roma-buffer char-str)
           (string-match-p "^[b-df-hj-np-tv-z]$" char-str))
      (cons "っ" char-str))

     ;; プレフィックスマッチ（まだ確定しないが次が続く場合）
     ((ktn--roma-prefix-p new-roma)
      (cons "" new-roma))

     ;; マッチしない場合の処理
     (t
      (if (string-empty-p ktn-roma-buffer)
          (cons char-str "")
        (cons ktn-roma-buffer char-str))))))

(defun ktn--complete-search-headword (prefix)
  "Search JSON dictionaries and stats for midasi keys starting with PREFIX.
Prioritize personal stats, then system dictionaries."
  (let ((seen (make-hash-table :test 'equal))
        (stats-results '())
        (system-results '()))
    
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
      (let ((file (ktn-dictionary-path dict)))
        (when (and file (file-exists-p file) (string-suffix-p ".json" file t))
          (unless (ktn-dictionary-data dict)
            (with-temp-buffer
              (insert-file-contents file)
              (setf (ktn-dictionary-data dict) (json-parse-buffer :object-type 'hash-table))))
          
          (let ((data (ktn-dictionary-data dict)))
            (when data
              (dolist (section-key '("okuri_nasi" "okuri_ari"))
                (let ((section (gethash section-key data)))
                  (when (hash-table-p section)
                    (maphash (lambda (midasi _)
                               (when (and (string-prefix-p prefix midasi)
                                          (not (gethash midasi seen)))
                                 (puthash midasi t seen)
                                 (push midasi system-results)))
                             section)))))))))
    
    (setq system-results (sort system-results #'string<))
    
    ;; 個人辞書の結果を先頭に、システム辞書の結果をその後に結合
    (append stats-results system-results)))

(defun ktn-complete-headword-backward ()
  "Cycle through headword completion candidates in reverse order using Shift-TAB."
  (interactive)
  (if (eq ktn-state 'headword)
      (progn
        (unless (string-empty-p ktn-roma-buffer)
          (setq ktn-headword-buffer (concat ktn-headword-buffer ktn-roma-buffer)
                ktn-roma-buffer ""))
        
        ;; 補完セッションが未開始なら通常の順方向と同じように初期化して末尾から開始
        (when (or (null ktn--completion-cands)
                  (null ktn--completion-base)
                  (not (string-prefix-p ktn--completion-base ktn-headword-buffer)))
          (setq ktn--completion-base ktn-headword-buffer
                ktn--completion-cands (ktn--complete-search-headword ktn-headword-buffer)
                ktn--completion-index 0))
        
        (if ktn--completion-cands
            (let ((total (length ktn--completion-cands)))
              ;; インデックスを逆回し（現在位置から2つ戻して調整）
              (setq ktn--completion-index (mod (- ktn--completion-index 2) total))
              (let ((completed-midasi (nth ktn--completion-index ktn--completion-cands)))
                (setq ktn-headword-buffer completed-midasi
                      ktn--completion-index (mod (1+ ktn--completion-index) total))
                (ktn--update-inline-display)
                (ktn--show-completions-window)))
          (ktn--close-candidate-window)
          (message "補完候補が見つかりません: %s" ktn-headword-buffer)))
    (call-interactively #'indent-for-tab-command)))

(defun ktn-complete-headword ()
  "Complete current headword in ▽ mode using TAB."
  (interactive)
  (if (eq ktn-state 'headword)
      (progn
        ;; 未確定のローマ字があれば、先にかなに変換
        (unless (string-empty-p ktn-roma-buffer)
          (setq ktn-headword-buffer (concat ktn-headword-buffer ktn-roma-buffer)
                ktn-roma-buffer ""))
        
        ;; 新しい補完セッションの開始判定
        (when (or (null ktn--completion-cands)
                  (null ktn--completion-base)
                  (not (string-prefix-p ktn--completion-base ktn-headword-buffer)))
          (setq ktn--completion-base ktn-headword-buffer
                ktn--completion-cands (ktn--complete-search-headword ktn-headword-buffer)
                ktn--completion-index 0))
        
        (if ktn--completion-cands
            (let ((completed-midasi (nth ktn--completion-index ktn--completion-cands)))
              (setq ktn-headword-buffer completed-midasi
                    ktn--completion-index (mod (1+ ktn--completion-index)
                                               (length ktn--completion-cands)))
              (ktn--update-inline-display)
              ;; 補完候補一覧ウィンドウを表示
              (ktn--show-completions-window))
          (ktn--close-candidate-window)
          (message "補完候補が見つかりません: %s" ktn-headword-buffer)))
    ;; ▽モードでなければ通常のTAB入力へ
    (call-interactively #'indent-for-tab-command)))

(defun ktn--show-completions-window ()
  "Display completion candidates in the candidate buffer/window (max 7 items per page)."
  (if ktn--completion-cands
      (let* ((total (length ktn--completion-cands))
             (page-size 7)
             (current-idx (mod (1- ktn--completion-index) total))
             (current-page (/ current-idx page-size))
             (start (* current-page page-size))
             (end (min total (+ start page-size)))
             (formatted-cands '())
             (i start))

        ;; 現在のページの7件分のみを生成
        (while (< i end)
          (let* ((cand (nth i ktn--completion-cands))
                 (item (if (= i current-idx)
                           (propertize (format " > %s" cand) 'face 'ktn-selected-face)
                         (format "   %s" cand))))
            (push item formatted-cands)
            (setq i (1+ i))))

        (let* ((buf (ktn--get-or-create-candidate-buffer))
               (win (display-buffer buf)))
          ;; 右寄せページ表示の計算
          (let* ((win-width (if (window-live-p win) (window-width win) (frame-width)))
                 (page-str (format "(%d/%dページ)" (1+ current-page) (ceiling total (float page-size))))
                 (padding (max 0 (- win-width (string-width page-str) 2)))
                 (right-aligned-page (concat (make-string padding ?\s) page-str))
                 (content (concat "【TAB補完候補】\n"
                                  (mapconcat #'identity (nreverse formatted-cands) "\n")
                                  (if (> total page-size) (concat "\n" right-aligned-page) ""))))

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
  (when ktn--completion-cands
    (setq ktn--completion-cands nil
          ktn--completion-index 0
          ktn--completion-base nil)
    (ktn--close-candidate-window)))

(defun ktn--prompt-dict-registration ()
  "Prompt user in minibuffer to register a new word for current headword/okuri."
  (let* ((midasi (if (not (string-empty-p ktn-okuri-roma-first))
                     (concat ktn-headword-buffer ktn-okuri-roma-first)
                   ktn-headword-buffer))
         (display-label (concat ktn-headword-buffer ktn-okuri-buffer))
         (user-word
          (minibuffer-with-setup-hook
              (lambda ()
                (use-local-map (let ((map (make-sparse-keymap (current-local-map))))
                                 (define-key map (kbd "C-c C-c") #'exit-minibuffer)
                                 map)))
            (read-string (format "[辞書登録] '%s' に対する単語を入力 (完了は C-c C-c): "
                                 display-label)))))
    (if (and user-word (not (string-empty-p user-word)))
        (let ((pos (and (markerp ktn-headword-start-marker)
                        (marker-position ktn-headword-start-marker))))
          ;; 1. 統計情報（個人辞書）に登録して保存
          (ktn--register-word midasi user-word)
          ;; 2. オーバーレイ等をクリーンアップ
          (ktn--cleanup-headword)
          ;; 3. 元の位置に登録した単語を挿入
          (when pos
            (goto-char pos)
            (insert user-word))
          (message "新しい単語を辞書に登録しました: [%s] -> %s" midasi user-word))
      ;; キャンセルされた場合
      (ktn-quit))))

;;;###autoload
(defun ktn-quit ()
  "Cancel current input sequence or return from converting to headword mode."
  (interactive)
  (cond
   ;; 1. ▼ 変換候補選択中なら ▽ 見出し語入力状態に戻る
   ((eq ktn-state 'converting)
    (setq ktn-state 'headword
          ktn-candidates nil
          ktn-candidate-index 0)
    (message "")
    (ktn--update-inline-display)
    ;; (message "Quit conversion, back to ▽ mode")
    )

   ;; 2. ▽ 見出し語入力中ならキャンセル
   ((eq ktn-state 'headword)
    (ktn--cleanup-headword)
    ;; (message "Quit ▽ mode")
    )

   ;; 3. 通常モードならバッファのローマ字バッファをクリア、なければC-gをパス
   (t
    (if (not (string-empty-p ktn-roma-buffer))
        (progn (setq ktn-roma-buffer "")
               ;; (message "Quit ktn input")
               )
      (keyboard-quit)))))

;;;###autoload
(defun ktn-backspace ()
  "Delete a character from romaji buffer or headword buffer in ▽ mode."
  (interactive)
  (ktn--reset-completion)
  (cond
   ;; ▼ 変換候補選択中なら C-g と同様に ▽ モードに戻す
   ((eq ktn-state 'converting)
    (setq ktn-state 'headword
          ktn-candidates nil
          ktn-candidate-index 0)
    (message "")
    (ktn--update-inline-display))

   ;; ▽ 見出し語入力中
   ((eq ktn-state 'headword)
    (ktn--reset-completion)
    (cond
     ;; 1. 未確定ローマ字バッファに文字があればそこから1文字削る
     ((not (string-empty-p ktn-okuri-roma-buffer))
      (setq ktn-okuri-roma-buffer (substring ktn-okuri-roma-buffer 0 -1))
      (ktn--update-inline-display))

     ((not (string-empty-p ktn-okuri-buffer))
      (setq ktn-okuri-buffer (substring ktn-okuri-buffer 0 -1))
      (when (string-empty-p ktn-okuri-buffer)
        (setq ktn-okuri-roma-first ""))
      (ktn--update-inline-display))

     ((not (string-empty-p ktn-roma-buffer))
      (setq ktn-roma-buffer (substring ktn-roma-buffer 0 -1))
      (ktn--update-inline-display))

     ;; 2. 見出し語バッファに文字があればそこから1文字削る
     ((not (string-empty-p ktn-headword-buffer))
      (setq ktn-headword-buffer (substring ktn-headword-buffer 0 -1))
      (if (string-empty-p ktn-headword-buffer)
          (progn
            (ktn--cleanup-headword)
            ;; (message "Quit ▽ mode")
            )
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
  (cond
   ;; 1. ▽ モードでの変換開始
   ((eq ktn-state 'headword)
    (unless (string-empty-p ktn-roma-buffer)
      (setq ktn-headword-buffer (concat ktn-headword-buffer ktn-roma-buffer)
            ktn-roma-buffer ""))
    (unless (string-empty-p ktn-okuri-roma-buffer)
      (setq ktn-okuri-buffer (concat ktn-okuri-buffer ktn-okuri-roma-buffer)
            ktn-okuri-roma-buffer ""))

    (when (string-suffix-p "n" ktn-headword-buffer)
      (setq ktn-headword-buffer
            (concat (substring ktn-headword-buffer 0 -1) "ん")))

    (let ((cands (if (not (string-empty-p ktn-okuri-roma-first))
                     (ktn-search-jisyo-with-okuri-combined ktn-headword-buffer ktn-okuri-roma-first ktn-okuri-buffer)
                   (ktn-search-jisyo-combined ktn-headword-buffer))))
      (if cands
          ;; 候補が見つかった場合
          (progn
            (setq ktn-state 'converting
                  ktn-candidates cands
                  ktn-candidate-index 0)
            (ktn--update-inline-display)
            (ktn--show-candidates-window))
        ;; 辞書に最初からヒットしない場合 -> 辞書登録モードへ
        (ktn--prompt-dict-registration))))

   ;; 2. ▼ 変換候補選択中（SPCで次候補へ進む）
   ((eq ktn-state 'converting)
    (let ((total (length ktn-candidates)))
      (if (< (1+ ktn-candidate-index) total)
          (progn
            (setq ktn-candidate-index (1+ ktn-candidate-index))
            (ktn--update-inline-display)
            (ktn--show-candidates-window))
        ;; 候補を全て出し切った状態でさらに SPC を押した場合 -> 辞書登録モードへ
        (ktn--prompt-dict-registration))))

   (t
    (insert " "))))

(defun ktn--register-word (midasi word &optional annotation)
  "Register a new WORD (with optional ANNOTATION) for MIDASI into `ktn--stats-data` and save."
  (let* ((clean-word (if annotation (concat word ";" annotation) word))
         (cands-hash (gethash midasi ktn--stats-data)))
    ;; 1. メモリ上のハッシュに初期カウント1（または既存＋1）で追加・更新
    (unless cands-hash
      (setq cands-hash (make-hash-table :test 'equal))
      (puthash midasi cands-hash ktn--stats-data))
    (let ((current-count (gethash clean-word cands-hash 0)))
      (puthash clean-word (1+ current-count) cands-hash))
    
    ;; 2. 即座に JSON ファイル（~/.jisyo.json）へ永続化
    (ktn-save-stats)
    
    (message "辞書に登録しました: [%s] -> %s" midasi clean-word)))

(defun ktn-prev-candidate ()
  "Select previous candidate in converting (▼) mode."
  (interactive)
  (if (eq ktn-state 'converting)
      (if (> ktn-candidate-index 0)
          (progn
            (setq ktn-candidate-index (1- ktn-candidate-index))
            (ktn--update-inline-display)
            (ktn--show-candidates-window))
        ;; 最初の候補で x を押した場合は ▽ モードに戻る
        (setq ktn-state 'headword
              ktn-candidates nil
              ktn-candidate-index 0)
        (ktn--update-inline-display)
        ;; (message "Quit conversion, back to ▽ mode")
        )
    (ktn-insert)))

;;;###autoload
(defun ktn-kakutei ()
  "Commit current headword or conversion result and insert into buffer."
  (interactive)
  (when ktn-state
    (let* (;; 送りありの場合は key に okuri-first-roma を含める
           (midasi (if (not (string-empty-p ktn-okuri-roma-first))
                       (concat ktn-headword-buffer ktn-okuri-roma-first)
                     ktn-headword-buffer))
           (text (cond
                  ((eq ktn-state 'converting)
                   (car (nth ktn-candidate-index ktn-candidates)))
                  (t
                   (concat ktn-headword-buffer ktn-roma-buffer ktn-okuri-buffer ktn-okuri-roma-buffer))))
           (pos (and (markerp ktn-headword-start-marker)
                     (marker-position ktn-headword-start-marker))))
      
      ;; ▼ 変換候補から確定された場合に統計情報を記録し、即座にファイル保存する
      (when (eq ktn-state 'converting)
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
        (insert text)))))

;;;###autoload
(defun ktn-kakutei-and-newline ()
  "Commit current input and insert a newline."
  (interactive)
  (when ktn-state
    (ktn-kakutei))
  (newline))

;;;###autoload
(defun ktn-insert ()
  "Insert router supporting normal and headword input modes."
  (interactive)
  (barf-if-buffer-read-only)
  (let* ((ch last-command-event)
         (is-upper (and (characterp ch) (<= ?A ch) (<= ch ?Z)))
         (lower-ch (if is-upper (+ ch 32) ch))
         (is-alpha (and (characterp lower-ch) (<= ?a lower-ch) (<= lower-ch ?z))))
    (cond
     ;; ▼ 変換候補選択中に入力が来たら、まず確定させてから次の入力として処理する
     ((eq ktn-state 'converting)
      (let* ((page-size (length ktn-select-keys))
             (current-page (/ ktn-candidate-index page-size))
             (page-start (* current-page page-size))
             (key-idx (cl-position ch ktn-select-keys)))
        (cond
         (key-idx
          (let ((target-idx (+ page-start key-idx)))
            (when (< target-idx (length ktn-candidates))
              (setq ktn-candidate-index target-idx)
              (ktn-kakutei))))
         ((or (eq ch ?x) (eq ch ?X))
          (ktn-prev-candidate))
         (t
          (ktn-kakutei)
          (ktn-insert)))))

     ;; 1. 通常モード時に大文字が入力されたら ▽モードを開始
     ((and (null ktn-state) is-upper)
      (unless (string-empty-p ktn-roma-buffer)
        (insert ktn-roma-buffer)
        (setq ktn-roma-buffer ""))
      (setq ktn-state 'headword
            ktn-headword-buffer ""
            ktn-roma-buffer ""
            ktn-okuri-buffer ""
            ktn-okuri-roma-buffer ""
            ktn-okuri-roma-first "")
      (setq ktn-headword-start-marker (point-marker))
      (let* ((char-str (char-to-string lower-ch))
             (res (ktn--process-roma-input char-str)))
        (setq ktn-headword-buffer (car res)
              ktn-roma-buffer (cdr res))
        (ktn--update-inline-display)))

     ;; 2. ▽モード時の入力
     ((eq ktn-state 'headword)
      (ktn--reset-completion)
      (if is-upper
          ;; ★ここで大文字が来たら「送り仮名」の入力に切り替える
          (progn
            ;; まず溜まっていたローマ字をかなに変換してヘッドワードへ
            (unless (string-empty-p ktn-roma-buffer)
              (setq ktn-headword-buffer (concat ktn-headword-buffer ktn-roma-buffer)
                    ktn-roma-buffer ""))
            ;; 送り英文字の記憶 (例: "k")
            (let ((char-str (char-to-string lower-ch)))
              (setq ktn-okuri-roma-first char-str
                    ktn-okuri-roma-buffer char-str
                    ktn-okuri-buffer "")
              (ktn--update-inline-display)
              ;; 母音大文字 (例: U) のように1文字でかなが完成する場合のみ自動変換
              (let ((res (ktn--process-roma-input char-str)))
                (unless (string-empty-p (car res))
                  (setq ktn-okuri-buffer (car res)
                        ktn-okuri-roma-buffer (cdr res))
                  (ktn--update-inline-display)
                  (ktn-start-henkan)))))

        ;; 小文字入力
        (if (string-empty-p ktn-okuri-roma-first)
            ;; まだ送り仮名に入っていなければヘッドワード側
            (let* ((char-str (if is-alpha (char-to-string lower-ch) (char-to-string ch)))
                   (res (ktn--process-roma-input char-str)))
              (setq ktn-headword-buffer (concat ktn-headword-buffer (car res))
                    ktn-roma-buffer (cdr res))
              (ktn--update-inline-display))
          ;; 既に送り仮名に入っていれば送り仮名側
          (let* ((char-str (if is-alpha (char-to-string lower-ch) (char-to-string ch)))
                 ;; 溜まっている送りローマ字と結合して変換を処理
                 (saved-roma ktn-roma-buffer))
            (setq ktn-roma-buffer ktn-okuri-roma-buffer)
            (let ((res (ktn--process-roma-input char-str)))
              (setq ktn-okuri-buffer (concat ktn-okuri-buffer (car res))
                    ktn-okuri-roma-buffer (cdr res)
                    ktn-roma-buffer saved-roma)
              (ktn--update-inline-display)
              ;; 送り仮名がかな (例: "く") に確定した瞬間に自動変換発火
              (unless (string-empty-p ktn-okuri-buffer)
                (ktn-start-henkan)))))))

     ;; 3. 通常モード時の入力
     (t
      (let* ((char-str (if is-alpha (char-to-string lower-ch) (char-to-string ch)))
             (res (ktn--process-roma-input char-str)))
        (unless (string-empty-p (car res))
          (insert (car res)))
        (setq ktn-roma-buffer (cdr res)))))))

(defun ktn-katakana-convert ()
  "Convert current headword in ▽ mode to Katakana and commit."
  (interactive)
  (if (eq ktn-state 'headword)
      (let ((kata-text (ktn--hiragana-to-katakana
                        (concat ktn-headword-buffer
                                ktn-roma-buffer
                                ktn-okuri-buffer
                                ktn-okuri-roma-buffer)))
            (pos (and (markerp ktn-headword-start-marker)
                      (marker-position ktn-headword-start-marker))))
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
  (when ktn-mode
    (ktn-setup))
  (ktn--cleanup-headword)
  (setq ktn-lighter (if ktn-mode ktn-indicator ""))
  ;; (message (if ktn-mode "Ktn mode ON" "Ktn mode OFF"))
  (force-mode-line-update))

(provide 'kotoni)
;;; kotoni.el ends here
