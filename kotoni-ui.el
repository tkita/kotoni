;;; kotoni-ui.el --- UI & Interactive Command Layer for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.2
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later

;;; Commentary:
;; 第1階層：Emacsのキーバインド・バッファ表示・オーバーレイ・ウィンドウ描画を担当する最上位モジュール。
;; ktn辞書（Trie-JSON）の高速な補完・変換結果をインタラクティブに表示・制御する。

;;; Code:

(require 'cl-lib)
(require 'ccc)             ; buffer local cursor color control library
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

(defcustom ktn-kakutei-hook nil
  "Hook run when a headword or conversion result is committed (kakutei).
Each function is called with one argument: BASE-TEXT."
  :type 'hook
  :group 'kotoni)

(defcustom ktn-show-candidates-nth-henkan 3
  "何回目の SPC 打鍵で候補一覧を表示するか"
  :type 'integer
  :group 'kotoni)

(defcustom ktn-newline-on-enter t
  "非nilの場合、変換確定時の RET キー打鍵による改行を抑制する。"
  :type 'boolean
  :group 'kotoni)

(defvar ktn-select-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7))

(defconst ktn-candidate-buffer-name " *ktn-candidates*")

(defvar ktn-lighter "")

(defconst ktn-dispatch-table
  '((( C-g  7                     ) . ktn-quit)
    (( C-h  8                     ) . ktn-backspace-handler)
    (( C-j 10                     ) . ktn-kakutei)
    (( C-m 13 RET return [return] ) . ktn-return-key-handler)
    (( SPC 32                     ) . ktn-start-henkan)
    (( M-SPC                      ) . ktn-completion-and-henkan) ; ktn--normalize-input() で正規化
    (( TAB tab [tab] ?\t          ) . ktn-tab-key-handler)
    (( <backtab> backtab S-TAB    ) . ktn-backtab-key-handler)
    (( <prior>   prior            ) . ktn-prior-page) ; PgUp
    (( <next>    next             ) . ktn-next-page)) ; PgDn
  "キーイベントディスパッチテーブル。")

;;; 表示・描画系関数
(defun ktn--set-cursor-red ()
  "Set the current buffer's cursor color to red via ccc.el."
  (when (fboundp #'ccc-set-buffer-local-cursor-color)
    (ccc-set-buffer-local-cursor-color "red")))

(defun ktn--unset-cursor-red ()
  "Restore the normal cursor color for the current buffer via ccc.el."
  (when (fboundp #'ccc-set-cursor-color-buffer-local)
    (ccc-set-cursor-color-buffer-local nil)))

(defun ktn--get-or-create-candidate-buffer ()
  (get-buffer-create ktn-candidate-buffer-name))

(defun ktn--close-candidate-window ()
  (when-let* ((buf (get-buffer ktn-candidate-buffer-name)))
    (delete-windows-on buf)))

(defun ktn--cleanup-headword ()
  (when ktn--current-headword
    (when (overlayp (ktn-headword-headword-overlay ktn--current-headword))
      (delete-overlay (ktn-headword-headword-overlay ktn--current-headword))
      (setf (ktn-headword-headword-overlay ktn--current-headword) nil))
    (when (markerp (ktn-headword-start-marker ktn--current-headword))
      (setf (ktn-headword-start-marker ktn--current-headword) nil)))
  (ktn--close-candidate-window)
  (setq ktn--normal-roma-buffer "")
  (ktn--state-reset))

(defun ktn--render-inline-content ()
  "▽モードおよび▼モードにおける表示内容を構築し、オーバーレイに適用する。"
  (cond
   ;; --- ▽ 見出し語入力モード ---
   ((ktn--mode-p 'headword)
    (let* ((hw ktn--current-headword)
           (input-str (concat (ktn-headword-buffer hw) (ktn-headword-roma-buffer hw)
                              (unless (string-empty-p (ktn-headword-okuri-roma-first hw)) "*")
                              (ktn-headword-okuri-buffer hw) (ktn-headword-okuri-roma-buffer hw)))
           (display-headword (concat "▽" input-str))
           (_ (unless (> (ktn-headword-completion-tab-count hw) 0)
                (ktn--ensure-completion-cands hw input-str)))
           (preview-suffix (ktn--get-current-completion-suffix)))
      (if (and preview-suffix
               (not (string-empty-p preview-suffix)))
          (progn
            (overlay-put (ktn-headword-headword-overlay hw) 'before-string
                         (propertize display-headword 'face 'ktn-headword-face))
            (overlay-put (ktn-headword-headword-overlay hw) 'after-string
                         (propertize preview-suffix 'face 'ktn-completion-preview-face 'cursor t)))
        (overlay-put (ktn-headword-headword-overlay hw) 'before-string
                     (propertize display-headword 'face 'ktn-headword-face))
        (overlay-put (ktn-headword-headword-overlay hw) 'after-string nil))))

   ;; --- ▼ 変換モード ---
   ((ktn--mode-p 'converting)
    (let* ((hw ktn--current-headword)
           (cand (nth (ktn-headword-candidate-index hw) (ktn-headword-candidates hw)))
           (cand-str (cond ((stringp cand) cand)
                          ((consp cand) (or (car cand) (format "%s" cand)))
                          ((listp cand) (format "%s" (car cand)))
                          (t (format "%s" cand))))
           (display-str (concat "▼" cand-str)))
      (overlay-put (ktn-headword-headword-overlay hw) 'before-string
                   (propertize display-str 'face 'ktn-headword-face))
      (overlay-put (ktn-headword-headword-overlay hw) 'after-string nil)))))

(defun ktn--manage-headword-overlay ()
  "現在のモードに応じてインラインオーバーレイの生成・削除・位置調整を行う。"
  (if (not (memq (and ktn--current-headword (ktn-headword-mode ktn--current-headword)) '(headword converting)))
      ;; 非アクティブ時はオーバーレイを削除
      (when (and ktn--current-headword
                 (overlayp (ktn-headword-headword-overlay ktn--current-headword)))
        (delete-overlay (ktn-headword-headword-overlay ktn--current-headword))
        (setf (ktn-headword-headword-overlay ktn--current-headword) nil))

    ;; アクティブ時はオーバーレイを現在位置に確保して追従させる
    (unless (and ktn--current-headword 
                 (overlayp (ktn-headword-headword-overlay ktn--current-headword)) 
                 (overlay-buffer (ktn-headword-headword-overlay ktn--current-headword)))
      (let ((pos (point)))
        (setf (ktn-headword-headword-overlay ktn--current-headword) (make-overlay pos pos nil t t))))
    (move-overlay (ktn-headword-headword-overlay ktn--current-headword) (point) (point))))

(defun ktn--update-inline-display ()
  "インライン表示の更新を行うメイン関数。"
  (ktn--manage-headword-overlay)
  (when (memq (and ktn--current-headword (ktn-headword-mode ktn--current-headword)) '(headword converting))
    (ktn--render-inline-content)))

(defun ktn--render-candidate-window (parts total current-page)
  "候補ウィンドウの共通描画・レイアウト・ページャー生成処理。"
  (let* ((buf                  (ktn--get-or-create-candidate-buffer))
         (win                  (display-buffer buf))
         (win-width            (if (window-live-p win)
                                   (window-width win)
                                 (frame-width)))
         (page-size            (length ktn-select-keys))
         (total-pages          (ceiling total (float page-size)))
         (page-str             (mapconcat (lambda (i)
                                            (if (= i current-page) "■" "□"))
                                          (number-sequence 0 (1- total-pages))
                                          ""))
         (max-avail-width      (- win-width 4))
         (display-page-str     (if (> (string-width page-str) max-avail-width)
                                   (format "(%d/%d)" (1+ current-page) total-pages)
                                 page-str))
         (padding              (max 0 (- win-width (string-width display-page-str) 2)))
         (right-aligned-page   (concat (make-string padding ?\s) display-page-str))
         (content              (concat (mapconcat #'identity (nreverse parts) "\n")
                                       (if (> total page-size)
                                           (concat "\n" right-aligned-page)
                                         ""))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)))
    (when (window-live-p win)
      (with-selected-window win
        (fit-window-to-buffer win)))))

(defun ktn--show-candidates-window ()
  "変換候補ウィンドウを表示する。"
  (if (not (ktn--mode-p 'converting))
      (ktn--close-candidate-window)
    (let* ((hw         ktn--current-headword)
           (candidates (ktn-headword-candidates hw))
           (total      (length candidates))
           (page-size  (length ktn-select-keys))
           (current-idx (ktn-headword-candidate-index hw))
           (current-page (/ current-idx page-size))
           (start      (* current-page page-size))
           (end        (min total (+ start page-size)))
           parts)
      (cl-loop for i from start below end
               for cand-cell = (nth i candidates)
               for c-str = (nth 0 cand-cell)
               for annot = (nth 1 cand-cell)
               for source = (nth 2 cand-cell)
               for source-marker = (if (eq source 'user) "+" " ")
               for display-str = (if (and annot (not (string-empty-p annot)))
                                     (concat c-str ";" annot)
                                   c-str)
               for key-char = (nth (- i start) ktn-select-keys)
               for key-str = (propertize (format " %c " key-char) 'face 'ktn-index-face)
               for formatted = (concat source-marker " " key-str " " display-str)
               do (push (if (= i current-idx)
                            (propertize formatted 'face 'ktn-selected-face)
                          formatted) parts))
      (ktn--render-candidate-window parts total current-page))))

;;; 前後の候補へ移動する関数
(defun ktn-previous-candidate ()
  "Move to the previous conversion candidate."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((hw ktn--current-headword)
           (total (length (ktn-headword-candidates hw)))
           (idx (ktn-headword-candidate-index hw)))
      (when (> total 0)
        (setf (ktn-headword-candidate-index hw) (mod (1- idx) total))
        (ktn--update-inline-display)
        (ktn--show-candidates-window)))))

(defun ktn-next-candidate ()
  "Move to the next conversion candidate."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((hw ktn--current-headword)
           (total (length (ktn-headword-candidates hw)))
           (idx (ktn-headword-candidate-index hw)))
      (when (> total 0)
        (setf (ktn-headword-candidate-index hw) (mod (1+ idx) total))
        (ktn--update-inline-display)
        (ktn--show-candidates-window)))))

(defun ktn-next-page ()
  "Move to the next page of candidates."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((hw ktn--current-headword)
           (total (length (ktn-headword-candidates hw)))
           (page-size (length ktn-select-keys))
           (idx (ktn-headword-candidate-index hw))
           (new-idx (min (1- total) (+ idx page-size))))
      (setf (ktn-headword-candidate-index hw) new-idx)
      (ktn--update-inline-display)
      (ktn--show-candidates-window))))

(defun ktn-prior-page ()
  "Move to the previous page of candidates."
  (interactive)
  (when (ktn--mode-p 'converting)
    (let* ((hw ktn--current-headword)
           (page-size (length ktn-select-keys))
           (idx (ktn-headword-candidate-index hw))
           (new-idx (max 0 (- idx page-size))))
      (setf (ktn-headword-candidate-index hw) new-idx)
      (ktn--update-inline-display)
      (ktn--show-candidates-window))))

;;;
;;; 見出し語の補完
;;;

(defun ktn--format-completion-candidate (candidate base-string)
  "CANDIDATE のうち BASE-STRING に一致する部分を `ktn-completion-base-face` で、
残りの部分を `ktn-completion-rest-face` で装飾して返す。"
  (let* ((base (or base-string ""))
         (base-len (length base)))
    (if (and (> base-len 0)
             (<= base-len (length candidate))
             (string-prefix-p base candidate))
        (concat
         (propertize (substring candidate 0 base-len) 'face 'ktn-completion-base-face)
         (propertize (substring candidate base-len)   'face 'ktn-completion-rest-face))
      (propertize candidate 'face 'ktn-completion-rest-face))))

(defun ktn--show-completion-window ()
  "見出し語の TAB 補完候補ウィンドウを表示する。"
  (let* ((hw ktn--current-headword)
         (cands    (and hw (ktn-headword-completion-cands hw)))
         (base-str (and hw (ktn-headword-completion-base hw))))
    (if (not cands)
        (ktn--close-candidate-window)
      (let* ((total        (length cands))
             (page-size    (length ktn-select-keys))
             (current-idx  (ktn-headword-completion-index hw))
             (current-page (/ current-idx page-size))
             (start        (* current-page page-size))
             (end          (min total (+ start page-size)))
             parts)
        (cl-loop for i from start below end
                 for cand = (nth i cands)
                 for key-char = (nth (- i start) ktn-select-keys)
                 for key-str = (propertize (format " %c " key-char) 'face 'ktn-index-face)
                 for formatted-cand = (ktn--format-completion-candidate cand base-str)
                 for formatted = (concat "    " key-str " " formatted-cand)
                 do (push (if (= i current-idx)
                              (propertize formatted 'face 'ktn-selected-face)
                            formatted)
                          parts))
        (ktn--render-candidate-window parts total current-page)))))

(defun ktn--get-current-completion-suffix ()
  (when-let* ((hw ktn--current-headword)
              (current-input (concat (ktn-headword-buffer hw) (ktn-headword-roma-buffer hw)
                                     (unless (string-empty-p (ktn-headword-okuri-roma-first hw))
                                       "*")
                                     (ktn-headword-okuri-buffer hw)
                                     (ktn-headword-okuri-roma-buffer hw)))
              ((ktn--mode-p 'headword))
              ((string-empty-p (ktn-headword-okuri-roma-first hw)))
              ((not (string-empty-p current-input))))
    (unless (> (ktn-headword-completion-tab-count hw) 0)
      (ktn--ensure-completion-cands hw current-input))
    (when-let* ((cands (ktn-headword-completion-cands hw))
                (first-cand (nth (ktn-headword-completion-index hw) cands))
                ((string-prefix-p current-input first-cand)))
      (substring first-cand (length current-input)))))

(defun ktn--ensure-completion-cands (hw current-input)
  "CURRENT-INPUT に対する補完候補をロード・維持する。"
  (let ((base  (ktn-headword-completion-base hw))
        (cands (ktn-headword-completion-cands hw)))
    (cond
     ((string-empty-p current-input)
      (setf (ktn-headword-completion-base hw) nil
            (ktn-headword-completion-cands hw) nil
            (ktn-headword-completion-index hw) 0))

     ((and base cands (string-prefix-p base current-input))
      (let ((filtered (cl-remove-if-not
                       (lambda (cand) (string-prefix-p current-input cand))
                       cands)))
        (setf (ktn-headword-completion-base hw) current-input
              (ktn-headword-completion-cands hw) filtered
              (ktn-headword-completion-index hw) 0)))

     ((or (null base)
          (null cands)
          (not (string-prefix-p base current-input)))
      (setf (ktn-headword-completion-base hw) current-input
            (ktn-headword-completion-index hw) 0
            (ktn-headword-completion-cands hw) (ktn--complete-search-headword current-input))))))

(defun ktn--headword-completion (&optional delta)
  "見出し語入力中の補完処理。"
  (interactive)
  (let ((d (or delta 1)))
    (when (and ktn--current-headword
               (string-empty-p (ktn-headword-okuri-roma-first ktn--current-headword)))
      (let* ((hw ktn--current-headword)
             (raw-input (concat (ktn-headword-buffer hw) (ktn-headword-roma-buffer hw))))
        (unless (ktn-headword-completion-base hw)
          (setf (ktn-headword-completion-base hw) raw-input
                (ktn-headword-completion-cands hw) (ktn--complete-search-headword raw-input)
                (ktn-headword-completion-index hw) 0))

        (let ((cands (ktn-headword-completion-cands hw)))
          (when (and cands (> (length cands) 0))
            (let* ((total     (length cands))
                   (tab-count (ktn-headword-completion-tab-count hw))
                   (idx       (ktn-headword-completion-index hw))
                   (new-idx (if (and (= tab-count 0)
                                     (= d 1))
                                idx
                              (mod (+ idx d) total)))
                   (chosen (nth new-idx cands)))
              (setf (ktn-headword-completion-tab-count hw) (1+ tab-count)
                    (ktn-headword-completion-index hw) new-idx
                    (ktn-headword-buffer hw) chosen
                    (ktn-headword-roma-buffer hw) "")
              (ktn--update-inline-display)
              (when (> (ktn-headword-completion-tab-count hw) 1)
                (ktn--show-completion-window)))))))))

;;;
;;; 辞書登録・アクション系
;;;

(defun ktn--prompt-dict-registration ()
  (let* ((hw ktn--current-headword)
         (enable-recursive-minibuffers t)
         (midasi (if (not (string-empty-p (ktn-headword-okuri-roma-first hw)))
                     (concat (ktn-headword-buffer hw) (ktn-headword-okuri-roma-first hw))
                   (ktn-headword-buffer hw)))
         (display-label (concat (ktn-headword-buffer hw) (ktn-headword-okuri-buffer hw)))
         (user-word (minibuffer-with-setup-hook
                        #'(lambda ()
                            (local-set-key (kbd "C-g") #'abort-recursive-edit)
                            (local-set-key (kbd "C-c C-c") #'(lambda ()
                                                               (interactive)
                                                               (when (active-minibuffer-window)
                                                                 (exit-minibuffer)))))
                      (read-from-minibuffer (format "[辞書登録] '%s' に対する単語を入力 (完了は C-c C-c): " display-label)))))
    (if (and user-word
             (not (string-empty-p user-word)))
        (let ((pos (and (markerp (ktn-headword-start-marker hw))
                        (marker-position (ktn-headword-start-marker hw)))))
          (ktn--register-word midasi user-word)
          (ktn--cleanup-headword)
          (when pos
            (goto-char pos)
            (insert user-word))
          (message "新しい単語を辞書に登録しました: [%s] -> %s" midasi user-word))
      (ktn-quit))))

(defun ktn-return-key-handler ()
  "RET / C-m 打鍵時の処理"
  (interactive)
  (if (not ktn--current-headword)
      (newline)
    (ktn-kakutei)
    (and ktn-newline-on-enter
         (newline))))

(defun ktn-tab-key-handler ()
  "TAB 打鍵時の処理。"
  (interactive)
  (cond
   ((and ktn--current-headword
         (ktn-headword-hassamu-active-p ktn--current-headword))
    (when (fboundp #'ktn--hassamu-accept-current)
      (ktn--hassamu-accept-current)))

   ((ktn--mode-p 'headword)
    (ktn--headword-completion 1))

   (t
    (insert "\t"))))

(defun ktn-backtab-key-handler ()
  "Shift-TAB / backtab 打鍵時の処理。"
  (interactive)
  (cond
   ((ktn--mode-p 'converting)
    (ktn-previous-candidate))
   ((ktn--mode-p 'headword)
    (ktn--headword-completion -1))
   (t
    (insert "\t"))))

(defun ktn-backspace-handler ()
  "Delete character backward in headword mode or normal mode."
  (interactive)
  (cond
   ((ktn--mode-p 'converting)
    (ktn-quit))
   ((ktn--mode-p 'headword)
    (let ((hw ktn--current-headword))
      (when (or (> (ktn-headword-completion-tab-count hw) 0)
                (get-buffer ktn-candidate-buffer-name))
        (ktn--close-candidate-window)
        (setf (ktn-headword-completion-tab-count hw) 0
              (ktn-headword-completion-index hw) 0))

      (cond
       ((not (string-empty-p (ktn-headword-roma-buffer hw)))
        (let ((roma (ktn-headword-roma-buffer hw)))
          (setf (ktn-headword-roma-buffer hw) (substring roma 0 -1))
          (ktn--update-inline-display)))
       ((not (string-empty-p (ktn-headword-buffer hw)))
        (let ((h (ktn-headword-buffer hw)))
          (setf (ktn-headword-buffer hw) (substring h 0 -1))
          (ktn--update-inline-display))
        (when (string-empty-p (ktn-headword-buffer hw))
          (ktn--cleanup-headword)))
       (t
        (ktn--cleanup-headword)))))
   (t
    (delete-char -1))))

(defun ktn--normalize-input (ch keys)
  "入力イベント CH とキーシーケンス KEYS を前裁きし、抽象化されたキーを返す。"
  (let ((key-desc (and keys (key-description keys))))
    (cond
     ;; --- M-SPC / ESC SPC の揺らぎをすべて 'M-SPC に正規化 ---
     ((or (and key-desc (or (equal key-desc "M-SPC")
                            (equal key-desc "ESC SPC")
                            (string-prefix-p "ESC" key-desc)))
          (and (numberp ch) (= ch 32) keys (seq-contains-p keys 27))
          (and (integerp ch) (= (logand ch #x3fff) 32) (not (= ch 32)))
          (equal ch 134217760)) ; テスト等で渡る特定の値
      'M-SPC)
     
     ;; --- 通常の数値やシンボルはそのまま返す ---
     (t ch))))

(defun ktn--lookup-dispatch (ch &optional keys)
  "前裁きされたキーを基に `ktn-dispatch-table' を検索する（本体）。"
  (let ((norm-key (ktn--normalize-input ch keys)))
    (cl-find-if
     (lambda (entry)
       (let ((table-keys (car entry)))
         (cl-some
          (lambda (k)
            (cond
             ;; 1. 数値同士の比較
             ((and (numberp k) (numberp norm-key) (= k norm-key))     t)
             ;; 2. シンボルやその他の完全一致 (equal)
             ((equal k norm-key)                                      t)
             ;; 3. キーシーケンス（keys）との比較
             ((and keys (equal k keys))                               t)
             ;; 4. キーディスクリプション文字列の一致
             ((and keys (stringp k) (equal k (key-description keys))) t)
             (t nil)))
          (if (listp table-keys) table-keys (list table-keys)))))
     ktn-dispatch-table)))

(defun ktn-handle-input ()
  "静的ディスパッチテーブルを活用した FSM 中央ディスパッチャ。"
  (interactive)
  (barf-if-buffer-read-only)
  (let* ((ch      last-command-event)
         (keys (this-command-keys)) ; 実際に押されたキーのシーケンス（文字列またはベクタ）を取得
         (matched (ktn--lookup-dispatch ch keys)))
    ;; (message "DEBUG: last-command-event = %S, this-command-keys = %s, matched = %S" ch keys matched) ; デバッグ用
    (cond
     (matched                   (call-interactively (cdr matched)))
     ((ktn--mode-p 'headword)   (ktn--handle-headword-mode ch #'ktn-start-henkan) (ktn--update-inline-display))
     ((ktn--mode-p 'converting) (ktn--handle-converting-mode ch)                  (ktn--update-inline-display))

     (t
      (ktn--handle-normal-mode ch)
      (when (ktn--mode-p 'headword)
        (ktn--update-inline-display))))))

(defun ktn-completion-and-henkan ()
  (interactive)
  (ktn--headword-completion 1)
  (call-interactively #'ktn-start-henkan))

(defun ktn-start-henkan ()
  "Start conversion or cycle through candidates with SPC."
  (interactive)
  (cond
   ((ktn--mode-p 'headword)
    (let ((hw ktn--current-headword))
      (unless (string-empty-p (ktn-headword-roma-buffer hw))
        (setf (ktn-headword-buffer hw) (concat (ktn-headword-buffer hw) (ktn-headword-roma-buffer hw))
              (ktn-headword-roma-buffer hw) ""))
      (unless (string-empty-p (ktn-headword-okuri-roma-buffer hw))
        (setf (ktn-headword-okuri-buffer hw) (concat (ktn-headword-okuri-buffer hw) (ktn-headword-okuri-roma-buffer hw))
              (ktn-headword-okuri-roma-buffer hw) ""))
      (when (string-suffix-p "n" (ktn-headword-buffer hw))
        (setf (ktn-headword-buffer hw) (concat (substring (ktn-headword-buffer hw) 0 -1) "ん")))
      (let ((cands (if (not (string-empty-p (ktn-headword-okuri-roma-first hw)))
                       (ktn-search-jisyo-with-okuri-combined (ktn-headword-buffer hw)
                                                             (ktn-headword-okuri-roma-first hw)
                                                             (ktn-headword-okuri-buffer hw))
                     (ktn-search-jisyo-combined (ktn-headword-buffer hw)))))
        (if (not cands)
            (ktn--prompt-dict-registration)
          (setf (ktn-headword-henkan-count hw) 0
                (ktn-headword-candidates hw) cands
                (ktn-headword-candidate-index hw) 0
                (ktn-headword-mode hw) 'converting)
          (ktn--update-inline-display)))))
   ((ktn--mode-p 'converting)
    (let ((hw ktn--current-headword))
      (setf (ktn-headword-henkan-count hw) (1+ (ktn-headword-henkan-count hw)))
      (if (>= (1+ (ktn-headword-candidate-index hw))
              (length (ktn-headword-candidates hw)))
          (ktn--prompt-dict-registration)
        (setf (ktn-headword-candidate-index hw) (1+ (ktn-headword-candidate-index hw)))
        (ktn--update-inline-display)
        (when (>= (ktn-headword-henkan-count hw)
                  (1- ktn-show-candidates-nth-henkan))
          (ktn--show-candidates-window)))))
   (t (insert " "))))

(defun ktn-kakutei ()
  "Commit current headword or conversion result and insert into buffer."
  (interactive)
  (when ktn--current-headword
    (ktn--kakutei-internal)))

(defun ktn--kakutei-internal ()
  (let* ((hw ktn--current-headword)
         (midasi (if (not (string-empty-p (ktn-headword-okuri-roma-first hw)))
                     (concat (ktn-headword-buffer hw) (ktn-headword-okuri-roma-first hw))
                   (ktn-headword-buffer hw)))
         (base-text (if (ktn--mode-p 'converting)
                        (car (nth (ktn-headword-candidate-index hw) (ktn-headword-candidates hw)))
                      (concat (ktn-headword-buffer hw)
                              (ktn-headword-roma-buffer hw)
                              (ktn-headword-okuri-buffer hw))))
         (p-proc (ktn-headword-post-processor hw))
         (text (if (functionp p-proc)
                   (funcall p-proc base-text)
                 base-text))
         (pos (and (markerp (ktn-headword-start-marker hw))
                   (marker-position (ktn-headword-start-marker hw)))))

    (ktn--add-recent-history midasi)

    (when (ktn--mode-p 'converting)
      (let ((cands-hash (gethash midasi ktn--stats-data)))
        (unless cands-hash
          (setq cands-hash (make-hash-table :test 'equal))
          (puthash midasi cands-hash ktn--stats-data))
        (puthash base-text (1+ (gethash base-text cands-hash 0)) cands-hash)
        (ktn-save-stats)))

    (setf (ktn-headword-post-processor hw) nil)
    (ktn--cleanup-headword)
    (when pos
      (goto-char pos)
      (insert text))

    (run-hook-with-args 'ktn-kakutei-hook base-text)))

(defun ktn-quit ()
  "Cancel current input sequence or return from converting to headword mode."
  (interactive)
  (cond
   ((ktn--mode-p 'headword)
    (ktn--cleanup-headword))
   ((ktn--mode-p 'converting)
    (let ((hw ktn--current-headword))
      (setf (ktn-headword-mode hw) 'headword
            (ktn-headword-candidates hw) nil
            (ktn-headword-candidate-index hw) 0)
      (message "")
      (ktn--close-candidate-window)
      (ktn--update-inline-display)))
   (t 
    (keyboard-quit))))

(defun ktn-setup ()
  "Initialize Ktn tables and dictionary instances safely."
  (when (fboundp #'ccc-setup)
    (ccc-setup))

  (when (ktn--load-stats)
    (ktn--setup-internal)))

(defun ktn--setup-internal ()
  (unless (and (boundp 'ktn-roma-exact-hash)
               ktn-roma-exact-hash)
    (ktn-setup-roma-table))

  (unless ktn-dictionaries
    (setq ktn-dictionaries (list (make-ktn-dictionary :path ktn-jisyo-file :data nil))))
    
  (dolist (dict ktn-dictionaries)
    (unless (ktn-dictionary-data dict)
      (ktn--load-dictionary-data dict)))

  (unless (memq #'ktn-save-stats kill-emacs-hook)
    (add-hook 'kill-emacs-hook #'ktn-save-stats))

  (add-to-list 'display-buffer-alist
               (list (concat "\\`" (regexp-quote ktn-candidate-buffer-name) "\\'")
                     '(display-buffer-in-side-window)
                     '(side . bottom)
                     '(slot . 0)
                     '(window-height . fit-window-to-buffer)
                     '(window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))))
  t)

(provide 'kotoni-ui)
;;; kotoni-ui.el ends here
