;;; skk2json.el --- SKK JISYO to JSON converter -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Your Name <you@example.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ( ... )

;;; Commentary:
;; M-x eval-buffer
;; M-x skk2json

;;; Code:
(require 'cl-lib)
(require 'json)

(defun skk2json (input-file output-file)
  "Convert SKK-JISYO file INPUT-FILE to JSON formatted dictionary at OUTPUT-FILE."
  (interactive "fSKK Dictionary File: \nFOutput JSON File: ")
  (unless (file-exists-p input-file)
    (error "Input file does not exist: %s" input-file))
  
  (message "Converting %s to JSON..." input-file)
  (let ((okuri-ari-alist '())
        (okuri-nasi-alist '())
        (current-section 'okuri-ari))
    
    (with-temp-buffer
      ;; Emacs がファイル先頭の -*- coding: ... -*- を自動判定して読み込みます
      (insert-file-contents input-file)
      (goto-char (point-min))
      
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (cond
           ((or (string-empty-p line) (string-prefix-p ";" line))
            (when (string-prefix-p ";; okuri-nasi entries." line)
              (setq current-section 'okuri-nasi)))
           
           ((string-match "^\\([^ \t]+\\)[ \t]+/\\(.+\\)/$" line)
            (let* ((midasi (match-string 1 line))
                   (raw-body (match-string 2 line))
                   (clean-body (replace-regexp-in-string "\\[[^]]*\\]" "" raw-body))
                   (items (split-string clean-body "/" t))
                   (cands '()))
              
              (dolist (item items)
                (let ((cand-obj
                       (if (string-match "^\\([^;]+\\);\\(.*\\)$" item)
                           ;; 注釈がある場合は alist（連想配列）として構築
                           `(("word" . ,(match-string 1 item))
                             ("annotation" . ,(match-string 2 item)))
                         ;; 注釈がない場合は word のみ
                         `(("word" . ,item)))))
                  (push cand-obj cands)))
              
              (when cands
                (let ((entry (cons midasi (nreverse cands))))
                  (if (eq current-section 'okuri-ari)
                      (push entry okuri-ari-alist)
                    (push entry okuri-nasi-alist))))))))
        (forward-line 1)))
    
    (let* ((root-obj `(("meta" . (("version" . "1.0")
                                  ("generator" . "skk2json.el")))
                       ("okuri_ari" . ,(nreverse okuri-ari-alist))
                       ("okuri_nasi" . ,(nreverse okuri-nasi-alist))))
           (json-encoding-pretty-print t))
      (with-temp-file output-file
        (insert (json-encode root-obj)))
      (message "Successfully converted SKK dictionary to %s!" output-file))))

(provide 'skk2json)
;;; skk2json.el ends here
