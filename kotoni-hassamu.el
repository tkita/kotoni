;;; kotoni-hassamu.el --- hassamu support for Kotoni -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5") (json))

;;; Commentary:
;; 1. 対象の文書ファイルを開き、`M-x ktn-hassamu` を実行して、バッファ内の漢字をスキャン・記憶させます。
;;    なお、この管理はバッファローカルですので、文書ファイルを閉じれば破棄されます。
;; 2. 変換時に、バッファ内の文脈に応じた語句候補がインラインおよび候補ウィンドウに提示されます。

;;; Code:

(require 'cl-lib)
(require 'kotoni-core)

(defvar-local ktn--hassamu-candidates nil
  "ktn-hassamu で抽出・保持している漢字文字列のリスト（バッファローカル）。")

(defvar-local ktn--hassamu-current-matches nil
  "現在前方一致で絞り込まれている候補のリスト。")

(defvar-local ktn--hassamu-active-p nil
  "現在 ktn-hassamu の補完セッション中かどうかを示すフラグ。")

(defvar-local ktn--hassamu-start-marker nil
  "ktn-hassamu のプレビューが始まったバッファ位置を指すマーカー。")

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
          ;; 構造: 漢字3文字以上の連続 ＋ (漢字以外の文字 または 文末)
          (while (re-search-forward "\\([一-龥]\\{3,\\}\\)[^一-龥]" nil t)
            (let ((match-str (match-string 1)))
              (when match-str
                (push match-str candidates)))))))

    (setq ktn--hassamu-candidates (delete-dups (nreverse candidates))
          ktn--hassamu-active-p nil
          ktn--hassamu-current-matches nil)
    (when (markerp ktn--hassamu-start-marker)
      (set-marker ktn--hassamu-start-marker nil))
    (message "ktn-hassamu: %d件の漢字文字列を記憶しました。" (length ktn--hassamu-candidates))))

(defun ktn--hassamu-update-display ()
  "ktn-hassamu のアクティブ時に、最初の候補をインラインプレビューとして表示する。"
  (when (overlayp ktn-headword-overlay)
    (delete-overlay ktn-headword-overlay)
    (setq ktn-headword-overlay nil))

  (when (and ktn--hassamu-active-p ktn--hassamu-current-matches)
    (let ((target (car ktn--hassamu-current-matches))
          (pos (point)))

      (setq ktn-headword-overlay (make-overlay pos pos nil t t))

      (move-overlay ktn-headword-overlay pos pos)
      (overlay-put  ktn-headword-overlay 'before-string nil)

      (overlay-put  ktn-headword-overlay 'after-string
                    (propertize (concat "→ " target)
                                'face 'ktn-completion-preview-face)))))

(defun ktn--hassamu-on-kakutei (confirmed-text)
  "確定文字列 CONFIRMED-TEXT をキーにリストを前方一致検索し、マッチすればセッションを開始する。"
  (if ktn--hassamu-candidates
      (let ((matches (cl-remove-if-not (lambda (item)
                                         (string-prefix-p confirmed-text item))
                                       ktn--hassamu-candidates)))
        (if matches
            (progn
              (setq ktn--hassamu-active-p t
                    ktn--hassamu-current-matches matches)
              (setq ktn--hassamu-start-marker
                    (set-marker (or ktn--hassamu-start-marker (make-marker))
                                (- (point) (length confirmed-text))))
              (ktn--hassamu-update-display))
          (setq ktn--hassamu-active-p nil
                ktn--hassamu-current-matches nil)
          (when (markerp ktn--hassamu-start-marker)
            (set-marker ktn--hassamu-start-marker nil))
          (ktn--hassamu-update-display)))
    (setq ktn--hassamu-active-p nil
          ktn--hassamu-current-matches nil)
    (ktn--hassamu-update-display)))

(defun ktn--hassamu-abort ()
  "ktn-hassamu のセッションを強制終了し、プレビューとマーカーを片付ける。"
  (setq ktn--hassamu-active-p nil
        ktn--hassamu-current-matches nil)
  (when (markerp ktn--hassamu-start-marker)
    (set-marker ktn--hassamu-start-marker nil))
  (ktn--hassamu-update-display))

(provide 'kotoni-hassamu)
;;; kotoni-hassamu.el ends here
