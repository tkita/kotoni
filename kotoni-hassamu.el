;;; kotoni-hassamu.el --- hassamu support for Kotoni -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.2
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5") (json))

;;; Commentary:
;; 1. 対象の文書ファイルを開き、`M-x ktn-hassamu` を実行して、バッファ内の漢字をスキャン・記憶させます。
;;    なお、この管理はバッファローカルですので、文書ファイルを閉じれば破棄されます。
;; 2. 変換時に、バッファ内の文脈に応じた語句候補がインラインおよび候補ウィンドウに提示されます。
;;    (ktn-headword オブジェクトの hassamu フィールド群を使用)

;;; Code:

(require 'cl-lib)
(require 'kotoni-fsm)

(defvar-local ktn--hassamu-candidates nil
  "ktn-hassamu で抽出・保持している漢字文字列のリスト（バッファローカル）。")

;;;###autoload
(defun ktn-hassamu ()
  "カレントバッファ全体 (point-min から point-max) から漢字文字列を抽出し、バッファローカルなリストに保持する。
漢字文字列とは、漢字で始まり、漢字以外の文字（改行を除く）が来るまでの間の文字列で、少なくとも漢字3文字以上とする。"
  (interactive)
  (let (candidates)
    (save-excursion
      (goto-char (point-min))
      ;; 改行を一時的に削除して結合（改行を跨ぐ連続に対応）
      (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
             (clean-content (replace-regexp-in-string "[\r\n]+" "" content)))
        (with-temp-buffer
          (insert clean-content)
          (goto-char (point-min))
          ;; 漢字3文字以上で始まり、その直後に「漢字以外の文字」が来るまでの文字列を抽出する
          (while (re-search-forward "\\([一-龥]\\{3,\\}\\)[^一-龥]" nil t)
            (let ((match-str (match-string 1)))
              (when match-str
                (push match-str candidates)))))))

    (setq ktn--hassamu-candidates (delete-dups (nreverse candidates)))
    (when (called-interactively-p 'interactive)
      (message "ktn-hassamu: %d 件の漢字文字列を記憶しました。"
               (length ktn--hassamu-candidates)))
    ktn--hassamu-candidates))

(defun ktn--hassamu-accept-current ()
  "ktn-hassamu の現在プレビューされている候補を確定してバッファに挿入し、セッションを終了する。"
  (when ktn--current-headword
    (let* ((hw             ktn--current-headword)
           (target         (car (ktn-headword-hassamu-current-matches hw)))
           (start-marker   (ktn-headword-hassamu-start-marker hw))
           (confirmed-text (ktn-headword-confirmed-text hw)))
      (when target
        (when (markerp start-marker)
          (goto-char (marker-position start-marker)))

        (let ((diff (if (and confirmed-text
                             (string-prefix-p confirmed-text target))
                        (substring target (length confirmed-text))
                      target)))
          (insert diff)))))

  (ktn--hassamu-abort))

(defun ktn--hassamu-update-display ()
  "ktn-hassamu のアクティブ時に、最初の候補をインラインプレビューとして表示する。"
  (when ktn--current-headword
    (let ((hw ktn--current-headword))
      ;; 既存のオーバーレイがあれば削除
      (when (overlayp (ktn-headword-headword-overlay hw))
        (delete-overlay (ktn-headword-headword-overlay hw))
        (setf (ktn-headword-headword-overlay hw) nil))

      ;; アクティブ状態とマッチ候補を取得して判定
      (when (and (ktn-headword-hassamu-active-p hw)
                 (ktn-headword-hassamu-current-matches hw))
        (let* ((target (car (ktn-headword-hassamu-current-matches hw)))
               (pos (or (and (markerp (ktn-headword-hassamu-start-marker hw))
                             (marker-position (ktn-headword-hassamu-start-marker hw)))
                        (point))))

          (setf (ktn-headword-headword-overlay hw) (make-overlay pos pos nil t t))

          (move-overlay (ktn-headword-headword-overlay hw) pos pos)
          (overlay-put  (ktn-headword-headword-overlay hw) 'before-string nil)

          (overlay-put  (ktn-headword-headword-overlay hw) 'after-string
                        (propertize (concat "→ " target)
                                    'face 'ktn-completion-preview-face)))))))

(defun ktn--hassamu-on-kakutei (confirmed-text)
  "確定文字列 CONFIRMED-TEXT をキーに `ktn--hassamu-candidates' を前方一致検索し、マッチすればセッションを開始する。"
  (if (not ktn--hassamu-candidates)
      (ktn--hassamu-abort)
    (let ((matches (cl-remove-if-not (lambda (item)
                                       (string-prefix-p confirmed-text item))
                                    ktn--hassamu-candidates)))
      (if (not matches)
          (ktn--hassamu-abort)
        (when ktn--current-headword
          (let ((hw ktn--current-headword))
            (setf (ktn-headword-hassamu-active-p hw)        t
                  (ktn-headword-hassamu-current-matches hw) matches
                  (ktn-headword-confirmed-text hw)          confirmed-text
                  (ktn-headword-hassamu-start-marker hw)    (set-marker (or (ktn-headword-hassamu-start-marker hw)
                                                                            (make-marker))
                                                                        (point)))
            (ktn--hassamu-update-display)))))))

(defun ktn--hassamu-abort ()
  "ktn-hassamu のセッションを強制終了し、プレビューとマーカーを片付ける。"
  (when ktn--current-headword
    (let ((hw ktn--current-headword))
      (setf (ktn-headword-hassamu-active-p hw)        nil
            (ktn-headword-hassamu-current-matches hw) nil
            (ktn-headword-confirmed-text hw)          nil)
      (let ((old-marker (ktn-headword-hassamu-start-marker hw)))
        (when (markerp old-marker)
          (set-marker old-marker nil))
        (setf (ktn-headword-hassamu-start-marker hw) nil))
      (ktn--hassamu-update-display))))

(add-hook 'ktn-kakutei-hook #'ktn--hassamu-on-kakutei)
(provide 'kotoni-hassamu)
;;; kotoni-hassamu.el ends here
