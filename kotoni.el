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

(require 'kotoni-common)
(require 'kotoni-core)
(require 'kotoni-hassamu)

(provide 'kotoni)
;;; kotoni.el ends here
