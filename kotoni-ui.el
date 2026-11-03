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

(defface ktn-headword-face
  '((t (:inherit underline)))
  "Face for ▽/▼ headword overlay."
  :group 'kotoni)

(defface ktn-selected-face
  '((t (:inherit highlight)))
  "Face for currently selected candidate."
  :group 'kotoni)

(defface ktn-completion-preview-face
  '((t (:foreground "#777777")))
  "Face for dynamic completion preview."
  :group 'kotoni)

(defface ktn-completion-base-face
  '((t (:inherit default)))
  "Face for typed prefix part."
  :group 'kotoni)

(defface ktn-completion-rest-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for remaining part."
  :group 'kotoni)

(defface ktn-index-face
  '((t (:background "white" :foreground "black" :underline t)))
  "Face for candidate index keys."
  :group 'kotoni)

(defcustom ktn-indicator " [▼]"
  "Modeline indicator when `ktn-mode` is active."
  :type 'string
  :group 'kotoni)

(defcustom ktn-show-candidates-nth-henkan 3
  "何回目の SPC 打鍵で候補一覧を表示するか"
  :type 'integer
  :group 'kotoni)

(defvar ktn-select-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7))
(defvar-local ktn--saved-cursor-color nil)
(defconst ktn-candidate-buffer-name " *ktn-candidates*")
(defvar ktn-lighter "")

;; | 10進 | 16進 | ASCII                           | Emacs Lisp での主な ?\ 表記        |
;; |------|------|---------------------------------|------------------------------------|
;; |  0   | 00   | NUL (Null文字)                  |  ?\0 (または ?\C-@)                |
;; |  1   | 01   | SOH (Start of Heading)          |  ?\C-a                             |
;; |  2   | 02   | STX (Start of Text)             |  ?\C-b                             |
;; |  3   | 03   | ETX (End of Text)               |  ?\C-c                             |
;; |  4   | 04   | EOT (End of Transmission)       |  ?\C-d                             |
;; |  5   | 05   | ENQ (Enquiry)                   |  ?\C-e                             |
;; |  6   | 06   | ACK (Acknowledge)               |  ?\C-f                             |
;; |  7   | 07   | BEL (Alert / Bell)              |  ?\a (または ?\C-g)                |
;; |  8   | 08   | BS  (Backspace)                 |  ?\b (または ?\C-h)                |
;; |  9   | 09   | HT  (Tab)                       |  ?\t (または ?\C-i, 'tab, 'TAB)    |
;; | 10   | 0a   | LF  (Line Feed / Newline)       |  ?\n (または ?\C-j)                |
;; | 11   | 0b   | VT  (Vertical Tab)              |  ?\v (または ?\C-k)                |
;; | 12   | 0c   | FF  (Form Feed)                 |  ?\f (または ?\C-l)                |
;; | 13   | 0d   | CR  (Carriage Return)           |  ?\r (または ?\C-m, 'RET, 'return) |
;; | 14   | 0e   | SO  (Shift Out)                 |  ?\C-n                             |
;; | 15   | 0f   | SI  (Shift In)                  |  ?\C-o                             |
;; | 16   | 10   | DLE (Data Link Escape)          |  ?\C-p                             |
;; | 17   | 11   | DC1 (Device Control 1 (XON))    |  ?\C-q                             |
;; | 18   | 12   | DC2 (Device Control 2)          |  ?\C-r                             |
;; | 19   | 13   | DC3 (Device Control 3 (XOFF))   |  ?\C-s                             |
;; | 20   | 14   | DC4 (Device Control 4)          |  ?\C-t                             |
;; | 21   | 15   | NAK (Negative Acknowledge)      |  ?\C-u                             |
;; | 22   | 16   | SYN (Synchronous Idle)          |  ?\C-v                             |
;; | 23   | 17   | ETB (End of Transmission Block) |  ?\C-w                             |
;; | 24   | 18   | CAN (Cancel)                    |  ?\C-x                             |
;; | 25   | 19   | EM  (End of Medium)             |  ?\C-y                             |
;; | 26   | 1a   | SUB (Substitute)                |  ?\C-z                             |
;; | 27   | 1b   | ESC (Escape)                    |  ?\e (または ?\C-[)                |
;; | 28   | 1c   | FS  (File Separator)            |  ?\C-\                             |
;; | 29   | 1d   | GS  (Group Separator)           |  ?\C-]                             |
;; | 30   | 1e   | RS  (Record Separator)          |  ?\C-^                             |
;; | 31   | 1f   | US  (Unit Separator)            |  ?\C-_                             |

(defconst ktn-dispatch-table
  '(((C-g  7)                     . ktn-quit)
    ((C-h  8)                     . ktn-backspace-handler)
    ((C-j 10)                     . ktn-kakutei)
    ((C-m 13 RET return [return]) . ktn-return-key-handler)
    ((SPC 32)                     . ktn-start-henkan)
    ((?q)                         . ktn-katakana-convert-or-insert)
    ((TAB tab [tab] ?\t)          . ktn-tab-key-handler)
    ((<prior> prior)              . ktn-prior-page) ; PgUp
    ((<next> next)                . ktn-next-page)) ; PgDn
  "キーイベントディスパッチテーブル。")

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

(defun ktn--format-completion-candidate (candidate base-string)
  "CANDIDATE のうち BASE-STRING に一致する部分を `ktn-completion-base-face` で、
残りの部分を `ktn-completion-rest-face` で装飾して返す。"
  (let* ((base     (or base-string ""))
         (base-len (length base)))
    (if (and (>  base-len 0)
             (<= base-len (length candidate))
             (string-prefix-p base base-string))
        (concat
         (propertize (substring candidate 0 base-len) 'face 'ktn-completion-base-face)
         (propertize (substring candidate base-len)   'face 'ktn-completion-rest-face))
      ;; 一致しない場合は全体を rest-face またはデフォルトにする
      (propertize candidate 'face 'ktn-completion-rest-face))))

(defun ktn--show-completion-window ()
  "見出し語入力中の補完候補一覧を最大7件ずつのページ単位でサイドウィンドウに表示する。"
  (let ((cands    (ktn--state-get :completion-cands))
        (base-str (ktn--state-get :headword-buffer))) ; 現在入力中のベース文字列を取得
    (if (not cands)
        (ktn--close-candidate-window)
      (let* ((total        (length cands))
             (page-size    (length ktn-select-keys))
             (current-idx  (ktn--state-get :completion-index))
             (current-page (/ current-idx page-size))
             (start        (* current-page page-size))
             (end          (min total (+ start page-size)))
             parts)
        (cl-loop for i from start below end
                 for cand = (nth i cands)
                 for key-char = (nth (- i start) ktn-select-keys)
                 for key-str = (propertize (format " %c " key-char) 'face 'ktn-index-face)
                 ;; 候補文字列をベース文字列に基づいてハイライト装飾する
                 for formatted-cand = (ktn--format-completion-candidate cand base-str)
                 for formatted = (concat "    " key-str " " formatted-cand)
                 do (push (if (= i current-idx)
                              (propertize formatted 'face 'ktn-selected-face)
                            formatted)
                          parts))

        (let* ((buf                (ktn--get-or-create-candidate-buffer))
               (win                (display-buffer buf))
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
              (fit-window-to-buffer win))))))))

(defun ktn--state-index-check ()
  (ktn--state-get :candidate-index))

;;; 前後の候補へ移動する関数
(defun ktn-previous-candidate ()
  "Move to the previous conversion candidate."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((total (length (ktn--state-get :candidates)))
           (idx (ktn--state-get :candidate-index)))
      (when (> total 0)
        (ktn--state-set :candidate-index (mod (1- idx) total))
        (ktn--update-inline-display)
        (ktn--show-candidates-window)))))

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

(defun ktn-prior-page ()
  "Move to the previous page of candidates."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((page-size (length ktn-select-keys))
           (idx (ktn--state-get :candidate-index))
           (new-idx (max 0 (- idx page-size))))
      (ktn--state-set :candidate-index new-idx)
      (ktn--update-inline-display)
      (ktn--show-candidates-window))))

(defun ktn-katakana-convert-or-insert ()
  (interactive)
  "Convert current headword to Katakana if in headword mode, otherwise insert 'q'."
  (if (not (ktn--mode-p 'headword))
      (insert "q")
    ;; 1. 残っているローマ字バッファを見出し語バッファへ結合
    (unless (string-empty-p (ktn--state-get :roma-buffer))
      (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
      (ktn--state-set :roma-buffer ""))
    ;; 2. 末尾の "n" を「ん」に変換
    (when (string-suffix-p "n" (ktn--state-get :headword-buffer))
      (ktn--state-set :headword-buffer (concat (substring (ktn--state-get :headword-buffer) 0 -1) "ん")))

    ;; 3. 結合・確定されたひらがなをカタカナに変換して挿入
    (let* ((raw  (ktn--state-get :headword-buffer))
           (kata (ktn--hiragana-to-katakana raw)))
      (ktn--cleanup-headword)
      (insert kata))))

;; (defun ktn--headword-completion ()
;;   (when (string-empty-p (ktn--state-get :okuri-roma-first))
;;     (ktn--ensure-completion-cands (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
;;     (let ((cands (ktn--state-get :completion-cands)))
;;       (when (and cands (> (length cands) 0))
;;         (let* ((idx (ktn--state-get :completion-index))
;;                (next-idx (mod (1+ idx) (length cands)))
;;                (chosen (nth next-idx cands)))
;;           (ktn--state-set :completion-index next-idx)
;;           (ktn--state-set :headword-buffer chosen)
;;           (ktn--state-set :roma-buffer "")
;;           (ktn--update-inline-display)
;;           (ktn--show-completion-window)
;;           )))))
(defun ktn--headword-completion ()
  "見出し語入力中のTAB補完処理。
1回目のTAB打鍵ではインライン更新のみ行い、
2回目以降のTAB打鍵で *candidates* ウィンドウを開いて候補を循環させる。"
  (when (string-empty-p (ktn--state-get :okuri-roma-first))
    (ktn--ensure-completion-cands (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
    (let ((cands (ktn--state-get :completion-cands)))
      (when (and cands (> (length cands) 0))
        (let ((tab-count (ktn--state-get :completion-tab-count 0)))
          (cond
           ;; 【1回目の TAB 打鍵】: ウィンドウを開かず、インライン更新のみ行う
           ((= tab-count 0)
            (ktn--state-set :completion-tab-count 1)
            (let* ((idx (ktn--state-get :completion-index))
                   (chosen (nth idx cands)))
              (ktn--state-set :headword-buffer chosen)
              (ktn--state-set :roma-buffer "")
              (ktn--update-inline-display)))

           ;; 【2回目以降の TAB 打鍵】: カウンターを進め、ウィンドウを開いて候補を循環させる
           (t
            (let* ((idx (ktn--state-get :completion-index))
                   (next-idx (mod (1+ idx) (length cands)))
                   (chosen (nth next-idx cands)))
              (ktn--state-set :completion-index next-idx)
              (ktn--state-set :headword-buffer chosen)
              (ktn--state-set :roma-buffer "")
              (ktn--update-inline-display)
              (ktn--show-completion-window)))))))))

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

(defun ktn--lookup-dispatch (ch)
  "キーイベント CH にマッチするディスパッチテーブルのエントリを検索。"
  (cl-find-if (lambda (entry)
                (let ((keys (car entry)))
                  (if (listp keys)
                      (memq ch keys)
                    (eq ch keys))))
              ktn-dispatch-table))

;;; 各種特殊キーのハンドラ（動的な状態分岐をカプセル化）
(defun ktn-return-key-handler ()
  "RET / C-m 打鍵時の処理。モード中なら確定+改行、通常なら改行。"
  (interactive)
  (if (ktn--state-get :mode)
      (ktn-kakutei-and-newline)
    (newline)))

(defun ktn-tab-key-handler ()
  "TAB 打鍵時の処理。状態に応じた補完やインサートに分岐。"
  (interactive)
  (cond
   ((ktn--state-get :hassamu-active-p)
    (when (fboundp #'ktn--hassamu-accept-current)
      (ktn--hassamu-accept-current)))
   ((ktn--mode-p 'headword)
    (ktn--headword-completion))
   (t
    (insert "\t"))))

(defun ktn-backspace-handler ()
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

;; | 現在の状態              | 入力 / イベント    | 次の状態       | アクション / 備考                  |
;; |-------------------------|--------------------|----------------|------------------------------------|
;; | 初期状態 (Emacs normal) | `C-x C-j`          | ktn-mode       | かな入力のため ktn-mode へ         |
;; | ktn-mode                | `C-x C-j`          | 初期状態       | ktn-mode を抜け、初期状態へ        |
;; | ktn-mode                | a-z.*              | ktn-mode       | 直接ローマ字かな変換               |
;; | ktn-mode                | A-Z                | ▽モード       | 見出し語入力を開始                 |
;; | ▽モード                | 小文字             | ▽モード       | ローマ字かな変換                   |
;; | ▽モード                | `C-g`              | ktn-mode       | ▽モードを破棄して戻る             |
;; | ▽モード                | TAB                | ktn-mode       | 見出し語を補完                     |
;; | ▽モード                | `C-j` / `RET`      | ktn-mode       | ▽モードを破棄して戻る(溜まった見出し語はバッファ挿入) |
;; | ▽モード                | `SPC`              | ▼モード       | 見出し語を確定し、漢字候補を表示   |
;; | ▼モード                | `C-g`              | ▽モード       | 漢字変換を取り消し、見出し語へ戻る |
;; | ▼モード                | `C-j` / `RET`      | ktn-mode       | 漢字候補を確定し、ktn-mode へ      |
;; | ▼モード                | `SPC` (次候補あり) | ▼モード       | 次の変換候補を表示                 |
;; | ▼モード                | `x`   (前候補あり) | ▼モード       | ひとつ前の変換候補を表示           |
;; | ▼モード                | `SPC` (候補なし)   | 辞書登録モード | 個人辞書登録へ移行                 |
;; | 辞書登録モード          | 登録完了           | ktn-mode       | 登録語を反映し、ktn-mode へ戻る    |

;; stateDiagram-v2
;;     [*] --> Normal : ktn-mode有効

;;     state Normal {
;;         [*] --> 待機中
;;         待機中 --> 待機中 : 小文字/通常入力 (a-z) <br/>[直接ローマ字かな変換・挿入]
;;     }

;;     %% Normal からの遷移
;;     Normal --> Headword : 大文字入力 (A-Z) <br/>[▽ 見出し語入力開始]

;;     state Headword {
;;         [*] --> 見出し入力
;;         見出し入力 --> 見出し入力 : ローマ字入力 / 継続
;;         見出し入力 --> 見出し入力 : TAB <br/>[補完候補の循環]
;;     }

;;     %% Headword からの遷移
;;     Headword --> Headword : 大文字入力 (A-Z) <br/>[送り仮名入力開始 / ▽再トリガー]
;;     Headword --> Converting : SPC <br/>[漢字変換開始・候補取得]
;;     Headword --> DictRegister : SPC (候補なし) <br/>[未登録語の辞書登録へ]
;;     Headword --> Normal : C-j / RET <br/>[入力を確定してバッファへ挿入]
;;     Headword --> Normal : C-g <br/>[入力を破棄・キャンセル]

;;     state Converting {
;;         [*] --> 候補選択
;;         候補選択 --> 候補選択 : SPC / x <br/>[次/前候補へ移動・ページ送り]
;;     }

;;     %% Converting からの遷移
;;     Converting --> Converting : SPC (末尾候補) <br/>[辞書登録へ移行]
;;     Converting --> Headword : 大文字入力 (A-Z) <br/>[現変換を確定 ＋ 新規 ▽ 開始]
;;     Converting --> Headword : C-g <br/>[変換キャンセルして ▽ へ戻る]
;;     Converting --> Normal : C-j / RET <br/>[選択候補を確定して通常モードへ]

;;     state DictRegister {
;;         [*] --> ミニバッファ入力
;;     }

;;     DictRegister --> Normal : 登録完了 / C-g <br/>[新単語を登録して通常モードへ]

(defun ktn-handle-input ()
  "静的ディスパッチテーブルを活用した FSM 中央ディスパッチャ。"
  (interactive)
  (barf-if-buffer-read-only)
  (let* ((ch last-command-event)
         (matched (ktn--lookup-dispatch ch)))

    (cond
     ;; 1. テーブルにヒットした場合、対応するコマンドを実行
     (matched
      (call-interactively (cdr matched)))

     ;; 2. ▽ 見出し語入力モード中の文字入力（ローマ字など）
     ((ktn--mode-p 'headword)
      (ktn--handle-headword-mode ch #'ktn-start-henkan)
      (ktn--update-inline-display))

     ;; 3. ▽ 見出し語入力モード中の文字入力（ローマ字など）
     ((ktn--mode-p 'converting)
      (ktn--handle-converting-mode ch)
      (ktn--update-inline-display))

     ;; 4. 通常モードの文字入力など
     (t
      (ktn--handle-normal-mode ch)
      (when (ktn--mode-p 'headword)
        (ktn--update-inline-display))))))


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
    (if (>= (1+ (ktn--state-get :candidate-index))
            (length (ktn--state-get :candidates)))
        (ktn--prompt-dict-registration)
      (ktn--state-set :candidate-index (1+ (ktn--state-get :candidate-index)))
      (ktn--update-inline-display)
      (when (>= (ktn--state-get :henkan-count)
                (1- ktn-show-candidates-nth-henkan))
        (ktn--show-candidates-window))))
   (t (insert " "))))

(defun ktn-kakutei ()
  "Commit current headword or conversion result and insert into buffer."
  (interactive)
  (when (ktn--state-get :mode) ; nil 以外の場合
    (let* ((midasi (if (not (string-empty-p (ktn--state-get :okuri-roma-first)))
                       (concat (ktn--state-get :headword-buffer) (ktn--state-get :okuri-roma-first))
                     (ktn--state-get :headword-buffer)))

           (text (cond ((ktn--mode-p 'converting)
                        (car (nth (ktn--state-get :candidate-index) (ktn--state-get :candidates))))
                       (t
                        (concat (ktn--state-get :headword-buffer)
                                (ktn--state-get :roma-buffer)
                                (ktn--state-get :okuri-buffer)
                                (ktn--state-get :okuri-roma-buffer)))))
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

(defun ktn-kakutei-and-newline ()
  "Commit current headword/conversion if active, otherwise insert a newline."
  (interactive)
  (if (ktn--state-get :mode)
      (progn
        (ktn-kakutei)
        (newline))
    (newline)))


(defun ktn-quit ()
  "Cancel current input sequence or return from converting to headword mode."
  (interactive)
  (cond
   ((ktn--mode-p 'headword)
    (ktn--cleanup-headword))

   ((ktn--mode-p 'converting)
    (ktn--state-set :mode            'headword)
    (ktn--state-set :candidates      nil)
    (ktn--state-set :candidate-index 0)

    (message "")
    (ktn--close-candidate-window)
    (ktn--update-inline-display))

   (t (if (string-empty-p (ktn--state-get :roma-buffer))
          (keyboard-quit)
        (ktn--state-set :roma-buffer "")))
   ))

;;; Keymap & Minor-mode Definition
(defvar ktn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'ktn-handle-input)

    (dolist (key '("C-g" "C-h" "C-j" "RET" "C-m" "SPC" "q"
                   "TAB" "<tab>" "<backtab>" "S-TAB"
                   "<next>" "<prior>"))
      (define-key map (kbd key) #'ktn-handle-input))
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
