;;; kotoni-auto.el --- Automatic deactivation of ktn-mode outside comments/strings -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5") (json))

;;; Commentary:
;;   カーソルがコメント・文字列の外に出たら自動で ktn-mode off
;;   (require 'kotoni-auto)
;;   (add-hook 'prog-mode-hook #'kotoni-auto-mode)

(defun ktn--in-comment-or-string-p ()
  "現在カーソル位置がコメント内または文字列リテラル内かどうか"
  (let ((pps (syntax-ppss)))
    (or (nth 3 pps) ; non-nil if inside a string.
        (nth 4 pps) ; nil if outside a comment, t if inside a non-nestable comment,
        ))) 

(defun kotoni-auto--check-context ()
  "ポイント位置の文脈に応じて、コメントや文字列リテラルの「外」に出たら `ktn-mode` を自動オフにする。"
  (when (and (derived-mode-p 'prog-mode)
             (not (derived-mode-p 'lisp-interaction-mode))
             ktn-mode
             (not (ktn--in-comment-or-string-p)))
    (ktn-mode -1)
    (message "[△] コメント/文字列の領域外。ktn-mode 自動オフ")))

;;;###autoload
(define-minor-mode kotoni-auto-mode
  "Toggle automatic context-based deactivation of `ktn-mode`.
When enabled, `ktn-mode` is automatically turned off when the point 
leaves comments or string literals, but it never turns on automatically."
  :lighter " [△]"
  :global nil
  (if kotoni-auto-mode
      ;; 有効化時: バッファローカルな post-command-hook に登録 (第4引数 t)
      (add-hook 'post-command-hook #'kotoni-auto--check-context nil t)
    ;; 無効化時: バッファローカルなフックから解除
    (remove-hook 'post-command-hook #'kotoni-auto--check-context t)))

(provide 'kotoni-auto)
;;; kotoni-auto.el ends here
