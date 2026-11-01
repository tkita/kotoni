;;; kotoni-ui.el --- UI & Interactive Command Layer for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later

;;; Commentary:
;; 第1階層：Emacsのキーバインド・バッファ表示・オーバーレイ・ウィンドウ描画を担当する最上位モジュール。

;;; Code:

(require 'cl-lib)
(require 'kotoni-data)
(require 'kotoni-fsm)

(defface ktn-headword-face '((t (:inherit underline))) "Face for ▽/▼ headword overlay." :group 'kotoni)
(defface ktn-selected-face '((t (:inherit highlight))) "Face for currently selected candidate." :group 'kotoni)
(defface ktn-completion-preview-face '((t (:foreground "#777777"))) "Face for dynamic completion preview." :group 'kotoni)
(defface ktn-completion-base-face '((t (:inherit default))) "Face for typed prefix part.")
(defface ktn-completion-rest-face '((t (:inherit font-lock-keyword-face :weight bold))) "Face for remaining part.")
(defface ktn-index-face '((t (:background "white" :foreground "black" :underline t))) "Face for candidate index keys." :group 'kotoni)

(defcustom ktn-indicator " [▼]" "Modeline indicator when `ktn-mode` is active." :type 'string :group 'kotoni)
(defcustom ktn-show-candidates-nth-henkan 3 "何回目のスペース打鍵で候補一覧を表示するか" :type 'integer :group 'kotoni)

(defvar ktn-select-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7))
(defvar-local ktn--saved-cursor-color nil)
(defconst ktn-candidate-buffer-name " *ktn-candidates*")
(defvar ktn-lighter "")

;;; 表示・描画系関数
(defun ktn--set-cursor-red ()
  (setq ktn--saved-cursor-color (or ktn--saved-cursor-color (frame-parameter nil 'cursor-color) (face-attribute 'cursor :background)))
  (set-cursor-color "red"))

(defun ktn--unset-cursor-red ()
  (when ktn--saved-cursor-color
    (set-cursor-color ktn--saved-cursor-color)
    (setq ktn--saved-cursor-color nil)))

(add-to-list 'display-buffer-alist
             (list (concat "\\`" (regexp-quote ktn-candidate-buffer-name) "\\'")
                   '(display-buffer-in-side-window)
                   '(side . bottom)
                   '(slot . 0)
                   '(window-height . fit-window-to-buffer)
                   '(window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))))

(defun ktn--get-or-create-candidate-buffer ()
  (get-buffer-create ktn-candidate-buffer-name))

(defun ktn--close-candidate-window ()
  (let ((buf (get-buffer ktn-candidate-buffer-name)))
    (when buf (delete-windows-on buf))))

(defun ktn--cleanup-headword ()
  (when (overlayp (ktn--state-get :headword-overlay))
    (delete-overlay (ktn--state-get :headword-overlay))
    (ktn--state-set :headword-overlay nil))
  (when (markerp (ktn--state-get :headword-start-marker))
    (ktn--state-set :headword-start-marker nil))
  (ktn--close-candidate-window)
  (ktn--state-reset))

(defun ktn--ensure-completion-cands (current-input)
  "Ensure completion candidates are loaded for CURRENT-INPUT."
  (when (and (not (string-empty-p current-input))
             (or (null (ktn--state-get :completion-cands))
                 (null (ktn--state-get :completion-base))
                 (not (string-prefix-p (ktn--state-get :completion-base) current-input))))
    (ktn--state-set :completion-base current-input)
    (ktn--state-set :completion-index 0)
    (ktn--state-set :completion-cands (ktn--complete-search-headword current-input))))

(defun ktn--get-current-completion-suffix ()
  (let ((current-input (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)
                               (unless (string-empty-p (ktn--state-get :okuri-roma-first)) "*")
                               (ktn--state-get :okuri-buffer) (ktn--state-get :okuri-roma-buffer))))
    (when (and (ktn--mode-p 'headword)
               (string-empty-p (ktn--state-get :okuri-roma-first))
               (not (string-empty-p current-input)))
      (ktn--ensure-completion-cands current-input)
      (when (ktn--state-get :completion-cands)
        (let ((first-cand (nth (ktn--state-get :completion-index) (ktn--state-get :completion-cands))))
          (when (and first-cand (string-prefix-p current-input first-cand))
            (substring first-cand (length current-input))))))))

(defun ktn--update-inline-display ()
  (if (not (memq (ktn--state-get :mode) '(headword converting)))
      (when (overlayp (ktn--state-get :headword-overlay))
        (delete-overlay (ktn--state-get :headword-overlay))
        (ktn--state-set :headword-overlay nil))
    (unless (and (overlayp (ktn--state-get :headword-overlay)) (overlay-buffer (ktn--state-get :headword-overlay)))
      (let ((pos (point))) (ktn--state-set :headword-overlay (make-overlay pos pos nil t t))))
    (move-overlay (ktn--state-get :headword-overlay) (point) (point))
    (cond
     ((ktn--mode-p 'headword)
      (let* ((input-str (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)
                                (unless (string-empty-p (ktn--state-get :okuri-roma-first)) "*")
                                (ktn--state-get :okuri-buffer) (ktn--state-get :okuri-roma-buffer)))
             (display-headword (concat "▽" input-str))
             (_ (ktn--ensure-completion-cands input-str)) ; ★ 起動直後から補完キャッシュを確実に生成
             (preview-suffix (ktn--get-current-completion-suffix)))
        (if (and preview-suffix (not (string-empty-p preview-suffix)))
            (progn
              (overlay-put (ktn--state-get :headword-overlay) 'before-string (propertize display-headword 'face 'ktn-headword-face))
              (overlay-put (ktn--state-get :headword-overlay) 'after-string (propertize preview-suffix 'face 'ktn-completion-preview-face 'cursor t)))
          (overlay-put (ktn--state-get :headword-overlay) 'before-string (propertize display-headword 'face 'ktn-headword-face))
          (overlay-put (ktn--state-get :headword-overlay) 'after-string nil))))
     ((ktn--mode-p 'converting)
      (let* ((cand (nth (ktn--state-get :candidate-index) (ktn--state-get :candidates)))
             (cand-str (cond ((stringp cand) cand)
                             ((consp cand) (or (car cand) (format "%s" cand)))
                             ((listp cand) (format "%s" (car cand)))
                             (t (format "%s" cand))))
             (display-str (concat "▼" cand-str)))
        (overlay-put (ktn--state-get :headword-overlay) 'before-string (propertize display-str 'face 'ktn-headword-face))
        (overlay-put (ktn--state-get :headword-overlay) 'after-string nil))))))

(defun ktn--show-candidates-window ()
  (if (not (ktn--mode-p 'converting))
      (ktn--close-candidate-window)
    (let* ((total (length (ktn--state-get :candidates)))
           (page-size (length ktn-select-keys))
           (current-page (/ (ktn--state-get :candidate-index) page-size))
           (start (* current-page page-size))
           (end (min total (+ start page-size)))
           parts)
      (cl-loop for i from start below end
               for cand-cell = (nth i (ktn--state-get :candidates))
               for c-str = (nth 0 cand-cell)
               for annot = (nth 1 cand-cell)
               for source = (nth 2 cand-cell)
               for source-marker = (if (eq source 'user) "+" " ")
               for display-str = (if (and annot (not (string-empty-p annot))) (concat c-str ";" annot) c-str)
               for key-char = (nth (- i start) ktn-select-keys)
               for key-str = (propertize (format " %c " key-char) 'face 'ktn-index-face)
               for formatted = (concat source-marker " " key-str " " display-str)
               do (push (if (= i (ktn--state-index-check)) (propertize formatted 'face 'ktn-selected-face) formatted) parts))
      (let* ((buf (ktn--get-or-create-candidate-buffer))
             (win (display-buffer buf))
             (win-width (if (window-live-p win) (window-width win) (frame-width)))
             (page-str (format "(%d/%dページ)" (1+ current-page) (ceiling total (float page-size))))
             (padding (max 0 (- win-width (string-width page-str) 2)))
             (right-aligned-page (concat (make-string padding ?\s) page-str))
             (content (concat (mapconcat #'identity (nreverse parts) "\n") (if (> total page-size) (concat "\n" right-aligned-page) ""))))
        (with-current-buffer buf (let ((inhibit-read-only t)) (erase-buffer) (insert content)))
        (when (window-live-p win) (with-selected-window win (fit-window-to-buffer win)))))))

(defun ktn--show-completion-window ()
  "ヘッドワード入力中の補完候補一覧を最大7件ずつのページ単位でサイドウィンドウに表示する[cite: 2, 4]。"
  (let ((cands (ktn--state-get :completion-cands)))
    (if (not cands)
        (ktn--close-candidate-window)
      (let* ((total (length cands))
             (page-size (length ktn-select-keys))
             (current-idx (ktn--state-get :completion-index))
             (current-page (/ current-idx page-size))
             (start (* current-page page-size))
             (end (min total (+ start page-size)))
             parts)
        (cl-loop for i from start below end
                 for cand = (nth i cands)
                 for key-char = (nth (- i start) ktn-select-keys)
                 for key-str = (propertize (format " %c " key-char) 'face 'ktn-index-face)
                 for formatted = (concat "   " key-str " " cand)
                 do (push (if (= i current-idx) (propertize formatted 'face 'ktn-selected-face) formatted) parts))
        (let* ((buf (ktn--get-or-create-candidate-buffer))
               (win (display-buffer buf))
               (win-width (if (window-live-p win) (window-width win) (frame-width)))
               (page-str (format "(%d/%dページ)" (1+ current-page) (ceiling total (float page-size))))
               (padding (max 0 (- win-width (string-width page-str) 2)))
               (right-aligned-page (concat (make-string padding ?\s) page-str))
               (content (concat (mapconcat #'identity (nreverse parts) "\n")
                                (if (> total page-size) (concat "\n" right-aligned-page) ""))))
          (with-current-buffer buf (let ((inhibit-read-only t)) (erase-buffer) (insert content)))
          (when (window-live-p win) (with-selected-window win (fit-window-to-buffer win))))))))

(defun ktn--state-index-check ()
  (ktn--state-get :candidate-index))

;;; 前後の候補へ移動する関数
;;;###autoload
(defun ktn-prev-candidate ()
  "Move to the previous conversion candidate."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((total (length (ktn--state-get :candidates)))
           (idx (ktn--state-get :candidate-index)))
      (when (> total 0)
        (ktn--state-set :candidate-index (mod (1- idx) total))
        (ktn--update-inline-display)
        (ktn--show-candidates-window)))))

;;;###autoload
(defun ktn-next-candidate ()
  "Move to the next conversion candidate."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((total (length (ktn--state-get :candidates)))
           (idx (ktn--state-get :candidate-index)))
      (when (> total 0)
        (ktn--state-set :candidate-index (mod (1+ idx) total))
        (ktn--update-inline-display)
        (ktn--show-candidates-window)))))

;;;###autoload
(defun ktn-next-page ()
  "Move to the next page of candidates."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((total (length (ktn--state-get :candidates)))
           (page-size (length ktn-select-keys))
           (idx (ktn--state-get :candidate-index))
           (new-idx (min (1- total) (+ idx page-size))))
      (ktn--state-set :candidate-index new-idx)
      (ktn--update-inline-display)
      (ktn--show-candidates-window))))

;;;###autoload
(defun ktn-prev-page ()
  "Move to the previous page of candidates."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((page-size (length ktn-select-keys))
           (idx (ktn--state-get :candidate-index))
           (new-idx (max 0 (- idx page-size))))
      (ktn--state-set :candidate-index new-idx)
      (ktn--update-inline-display)
      (ktn--show-candidates-window))))

;;;###autoload
(defun ktn-katakana-convert-or-insert ()
  "Convert current headword to Katakana if in headword mode, otherwise insert 'q'."
  (interactive)
  (if (not (ktn--mode-p 'headword))
      (insert "q")
    (let* ((raw  (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
           (kata (ktn--hiragana-to-katakana raw)))
      (ktn--cleanup-headword)
      (insert kata))))

;;; TABキーによる補完候補のサイクル補完関数
;;;###autoload
(defun ktn-complete ()
  "Cycle through completion candidates (works in both headword and converting modes)."
  (interactive)
  (cond
   ;; 0. hassamu
   ((ktn--state-get :hassamu-active-p)
    (ktn--hassamu-accept-current))

   ;; 1. 見出し語入力中の場合：既存の補完候補リストを回す
   ((and (ktn--mode-p 'headword) (string-empty-p (ktn--state-get :okuri-roma-first)))
    (let ((current-input (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer))))
      (ktn--ensure-completion-cands current-input))
    (let ((cands (ktn--state-get :completion-cands)))
      (when (and cands (> (length cands) 0))
        (let* ((idx (ktn--state-get :completion-index))
               (next-idx (mod (1+ idx) (length cands)))
               (chosen (nth next-idx cands)))
          (ktn--state-set :completion-index next-idx)
          (ktn--state-set :headword-buffer chosen)
          (ktn--state-set :roma-buffer "")
          (ktn--update-inline-display)
          (ktn--show-completion-window)
          ))))

   ;; 2. 変換中（converting）の場合：プレフィックス補完候補をベースに見出し語を切り替える
   ((ktn--mode-p 'converting)
    (let* ((current-head (ktn--state-get :headword-buffer))
           (_ (ktn--ensure-completion-cands current-head))
           (cands (ktn--state-get :completion-cands)))
      (when (and cands (> (length cands) 0))
        (let* ((idx (ktn--state-get :completion-index))
               (next-idx (mod (1+ idx) (length cands)))
               (chosen (nth next-idx cands)))
          (ktn--state-set :completion-index next-idx)
          (ktn--state-set :headword-buffer chosen)
          ;; 新しい見出し語で辞書検索をやり直し、変換モードを維持したまま候補を更新
          (let ((new-cands (ktn-search-jisyo-combined chosen)))
            (when new-cands
              (ktn--state-set :candidates new-cands)
              (ktn--state-set :candidate-index 0)
              (ktn--update-inline-display)
              (when (>= (ktn--state-get :henkan-count) 2)
                (ktn--show-candidates-window))))))))))

(defun ktn--prompt-dict-registration ()
  (let* ((enable-recursive-minibuffers t)
         (midasi (if (not (string-empty-p (ktn--state-get :okuri-roma-first)))
                     (concat (ktn--state-get :headword-buffer) (ktn--state-get :okuri-roma-first))
                   (ktn--state-get :headword-buffer)))
         (display-label (concat (ktn--state-get :headword-buffer) (ktn--state-get :okuri-buffer)))
         (user-word (minibuffer-with-setup-hook
                        (lambda ()
                          (local-set-key (kbd "C-g") #'abort-recursive-edit)
                          (local-set-key (kbd "C-c C-c") (lambda () (interactive) (when (active-minibuffer-window) (exit-minibuffer)))))
                      (read-from-minibuffer (format "[辞書登録] '%s' に対する単語を入力 (完了は C-c C-c): " display-label)))))
    (if (and user-word (not (string-empty-p user-word)))
        (let ((pos (and (markerp (ktn--state-get :headword-start-marker)) (marker-position (ktn--state-get :headword-start-marker)))))
          (ktn--register-word midasi user-word)
          (ktn--cleanup-headword)
          (when pos (goto-char pos) (insert user-word))
          (message "新しい単語を辞書に登録しました: [%s] -> %s" midasi user-word))
      (ktn-quit))))

;;;###autoload
(defun ktn-insert ()
  "Insert router powered by FSM state dispatch."
  (interactive)
  (barf-if-buffer-read-only)

  ;; +----------------+-------------------------------+----------------------------------------------------------------+
  ;; | FSM 状態       | 概要                          | トリガー / 挙動                                                |
  ;; +----------------+-------------------------------+----------------------------------------------------------------+
  ;; | (3) normal (t) | 通常モード                    | - ktn--handle-normal-mode で通常のテキスト入力処理             |
  ;; |                |                               | - 入力結果で headword 状態へ遷移した場合はインライン表示を更新 |
  ;; +----------------+-------------------------------+----------------------------------------------------------------+
  ;; | (2) headword   | 見出し語入力モード            | - ktn--handle-headword-mode でローマ字/ひらがな処理            |
  ;; |                | (インラインに "▽" 表示)      | - ktn--update-inline-display で表示を最新化                    |
  ;; +----------------+-------------------------------+----------------------------------------------------------------+
  ;; | (1) converting | 変換中モード                  | - 数字キー(1-7): 該当候補を選択して確定                        |
  ;; |                | (候補選択ウィンドウが表示中)  | - 'x' / 'X'    : 前の候補へ移動                                |
  ;; |                |                               | - その他のキー : 現在の候補で確定。FSMリセットして打鍵キーイベ |
  ;; |                |                               |                  ントをそのまま転送                            |
  ;; +----------------+-------------------------------+----------------------------------------------------------------+

  (let ((ch last-command-event))
    (cond

     ;; (1) Converting mode: Candidate selection and navigation
     ((ktn--mode-p 'converting)
      (let* ((page-size (length ktn-select-keys))
             (current-page (/ (ktn--state-get :candidate-index) page-size))
             (page-start (* current-page page-size))
             (key-idx (cl-position ch ktn-select-keys)))
        (cond
         (key-idx
          (let ((target-idx (+ page-start key-idx)))
            (when (< target-idx (length (ktn--state-get :candidates)))
              (ktn--state-set :candidate-index target-idx)
              (ktn-kakutei))))
         ((or (eq ch ?x) (eq ch ?X)) (ktn-prev-candidate))
         (t (ktn-kakutei) (ktn--state-reset) (push ch unread-command-events)))))

     ;; (2) Headword input mode: Handling romaji/hiragana input and inline display
     ((ktn--mode-p 'headword)
      (ktn--handle-headword-mode ch #'ktn-start-henkan)
      (ktn--update-inline-display))

     ;; (3) Normal mode: Standard text editing and transition triggers
     (t
      (ktn--handle-normal-mode ch)
      (when (ktn--mode-p 'headword)
        (ktn--update-inline-display))))))

;;;###autoload
(defun ktn-start-henkan ()
  "Start conversion or cycle through candidates with SPC."
  (interactive)
  (cond
   ((ktn--mode-p 'headword)
    (unless (string-empty-p (ktn--state-get :roma-buffer))
      (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
      (ktn--state-set :roma-buffer ""))
    (unless (string-empty-p (ktn--state-get :okuri-roma-buffer))
      (ktn--state-set :okuri-buffer (concat (ktn--state-get :okuri-buffer) (ktn--state-get :okuri-roma-buffer)))
      (ktn--state-set :okuri-roma-buffer ""))
    (when (string-suffix-p "n" (ktn--state-get :headword-buffer))
      (ktn--state-set :headword-buffer (concat (substring (ktn--state-get :headword-buffer) 0 -1) "ん")))
    (let ((cands (if (not (string-empty-p (ktn--state-get :okuri-roma-first)))
                     (ktn-search-jisyo-with-okuri-combined (ktn--state-get :headword-buffer)
                                                           (ktn--state-get :okuri-roma-first)
                                                           (ktn--state-get :okuri-buffer))
                   (ktn-search-jisyo-combined (ktn--state-get :headword-buffer)))))
      (if (not cands)
          (ktn--prompt-dict-registration)
        (ktn--state-set :henkan-count 0)
        (ktn--state-set :candidates cands)
        (ktn--state-set :candidate-index 0)
        (ktn--state-set :mode 'converting)
        (ktn--update-inline-display))))
   ((ktn--mode-p 'converting)
    (ktn--state-set :henkan-count (1+ (ktn--state-get :henkan-count)))
    (if (>= (1+ (ktn--state-get :candidate-index)) (length (ktn--state-get :candidates)))
        (ktn--prompt-dict-registration)
      (ktn--state-set :candidate-index (1+ (ktn--state-get :candidate-index)))
      (ktn--update-inline-display)
      (when (>= (ktn--state-get :henkan-count) 2) (ktn--show-candidates-window))))
   (t (insert " "))))

;;;###autoload
(defun ktn-kakutei ()
  "Commit current headword or conversion result and insert into buffer."
  (interactive)
  (when (ktn--state-get :mode)
    (let* ((midasi (if (not (string-empty-p (ktn--state-get :okuri-roma-first)))
                       (concat (ktn--state-get :headword-buffer) (ktn--state-get :okuri-roma-first))
                     (ktn--state-get :headword-buffer)))
           (text (cond ((ktn--mode-p 'converting) (car (nth (ktn--state-get :candidate-index) (ktn--state-get :candidates))))
                       (t (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)
                                  (ktn--state-get :okuri-buffer) (ktn--state-get :okuri-roma-buffer)))))
           (pos (and (markerp (ktn--state-get :headword-start-marker))
                     (marker-position (ktn--state-get :headword-start-marker)))))
      (when (ktn--mode-p 'converting)
        (let ((cands-hash (gethash midasi ktn--stats-data)))
          (unless cands-hash
            (setq cands-hash (make-hash-table :test 'equal))
            (puthash midasi cands-hash ktn--stats-data))
          (puthash text (1+ (gethash text cands-hash 0)) cands-hash)
          (ktn-save-stats)))
      (ktn--cleanup-headword)

      (when pos
        (goto-char pos)
        (insert text))

      (when (fboundp #'ktn--hassamu-on-kakutei)
        (ktn--hassamu-on-kakutei text))
      )))

;;;###autoload
(defun ktn-kakutei-and-newline ()
  "Commit current headword/conversion if active, otherwise insert a newline."
  (interactive)
  (if (ktn--state-get :mode)
      (progn
        (ktn-kakutei)
        (newline))
    (newline)))

;;;###autoload
(defun ktn-backspace ()
  "Delete character backward in headword mode or normal mode."
  (interactive)
  (cond
   ((ktn--mode-p 'converting)
    (ktn-quit))
   ((ktn--mode-p 'headword)
    (cond
     ;; 1. ローマ字バッファに文字がある場合はそこから1文字削る
     ((not (string-empty-p (ktn--state-get :roma-buffer)))
      (let ((roma (ktn--state-get :roma-buffer)))
        (ktn--state-set :roma-buffer (substring roma 0 -1))
        (ktn--update-inline-display)))
     ;; 2. 見出し語バッファに文字がある場合はそこから1文字削る
     ((not (string-empty-p (ktn--state-get :headword-buffer)))
      (let ((hw (ktn--state-get :headword-buffer)))
        (ktn--state-set :headword-buffer (substring hw 0 -1))
        (ktn--update-inline-display))
      (when (string-empty-p (ktn--state-get :headword-buffer))
        (ktn--cleanup-headword)))
     (t
      (ktn--cleanup-headword))))
   (t
    (backward-delete-char 1))))

;;;###autoload
(defun ktn-quit ()
  "Cancel current input sequence or return from converting to headword mode."
  (interactive)
  (cond
   ((ktn--mode-p 'converting)
    (ktn--state-set :mode 'headword)
    (ktn--state-set :candidates nil)
    (ktn--state-set :candidate-index 0)
    (message "")
    (ktn--update-inline-display))
   ((ktn--mode-p 'headword) (ktn--cleanup-headword))
   (t (if (string-empty-p (ktn--state-get :roma-buffer))
          (keyboard-quit)
        (ktn--state-set :roma-buffer "")))))

;;; Keymap & Minor-mode Definition
(defvar ktn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'ktn-insert)
    (define-key map (kbd "C-g") #'ktn-quit)
    (define-key map (kbd "C-j") #'ktn-kakutei)
    (define-key map (kbd "RET") #'ktn-kakutei-and-newline)
    (define-key map (kbd "C-m") #'ktn-kakutei-and-newline)
    (define-key map (kbd "SPC") #'ktn-start-henkan)
    (define-key map (kbd "C-h") #'ktn-backspace)
    (define-key map (kbd "q")   #'ktn-katakana-convert-or-insert)

    (define-key map (kbd "TAB")       #'ktn-complete)
    (define-key map (kbd "<tab>")     #'ktn-complete)
    (define-key map (kbd "<backtab>") #'ktn-complete-headword-backward)
    (define-key map (kbd "S-TAB")     #'ktn-complete-headword-backward)

    (define-key map (kbd "<next>")    #'ktn-next-page) ;PageDown
    (define-key map (kbd "<prior>")   #'ktn-prev-page) ;PageUp
    map)
  "Keymap for `ktn-mode`.")

(defun ktn-setup ()
  "Initialize Ktn tables and dictionary instances safely."
  (when (ktn--load-stats)
    (unless ktn-roma-table (ktn-setup-roma-table))
    (setf (ktn-dictionary-path ktn--system-jisyo-instance) ktn-jisyo-file)
    (setq ktn-dictionaries (list ktn--system-jisyo-instance))
    (unless (memq #'ktn-save-stats kill-emacs-hook)
      (add-hook 'kill-emacs-hook #'ktn-save-stats))
    t))

;;;###autoload
(define-minor-mode ktn-mode
  "Toggle Ktn mode."
  :lighter ktn-lighter
  :global nil
  :keymap ktn-mode-map
  (if ktn-mode
      (progn (ktn-setup) (ktn--set-cursor-red))
    (ktn--state-reset)
    (ktn--unset-cursor-red)
    (ktn--cleanup-headword))
  (setq ktn-lighter (if ktn-mode ktn-indicator ""))
  (force-mode-line-update))

(provide 'kotoni-ui)
;;; kotoni-ui.el ends here
