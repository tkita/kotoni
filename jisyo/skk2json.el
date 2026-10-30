;;; skk2json.el --- SKK JISYO to JSON converter -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (json))
;; License: GPL-3.0-or-later

;;; Commentary:
;; Convert a SKK JISYO dictionary file into JSON.
;; M-x eval-buffer
;; M-x skk2json

;;; Code:
(require 'cl-lib)
(require 'json)

(defun skk2json (input-file output-file)
  "Convert SKK-JISYO file INPUT-FILE to JSON formatted dictionary (Hash/Object format) at OUTPUT-FILE."
  (interactive "fSKK Dictionary File: \nFOutput JSON File: ")
  (unless (file-exists-p input-file)
    (error "Input file does not exist: %s" input-file))

  (message "Converting %s to JSON..." input-file)
  (let ((okuri-ari-hash (make-hash-table :test 'equal))
        (okuri-nasi-hash (make-hash-table :test 'equal))
        (current-section 'okuri-ari)
        (total-lines 0)
        (processed-lines 0)
        (last-percent -1))

    (with-temp-buffer
      (insert-file-contents input-file)
      (goto-char (point-min))
      (setq total-lines (count-lines (point-min) (point-max)))
      (goto-char (point-min))

      (let ((continue t))
        (while (and continue (not (eobp)))
          (let ((line (string-trim (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
            (setq processed-lines (1+ processed-lines))

            ;; 進捗率の計算と表示（総行数を超えないように min で保護）
            (when (> total-lines 0)
              (let* ((safe-processed (min processed-lines total-lines))
                     (percent (/ (* safe-processed 100) total-lines)))
                (when (and (/= percent last-percent) (or (= percent 100) (= (% percent 5) 0)))
                  (setq last-percent percent)
                  (message "Converting... %d%% (%d / %d lines)" percent safe-processed total-lines))))

            (cond
             ((or (string-empty-p line)
                  (string-prefix-p ";" line)
                  (string-prefix-p ">" line)
                  (string-prefix-p "#" line))
              (when (string-prefix-p ";; okuri-nasi entries." line)
                (setq current-section 'okuri-nasi)))

             ((string-match "^\\([^ \t]+\\)[ \t]+/\\(.+\\)/$" line)
              (let* ((midasi (match-string 1 line))
                     (raw-body (match-string 2 line))
                     (clean-body (replace-regexp-in-string "\\[[^]]*\\]" "" raw-body))
                     (items (split-string clean-body "/" t))
                     cands)

                (dolist (item items)
                  (let ((cand-obj
                         (if (string-match "^\\([^;]+\\);\\(.*\\)$" item)
                             `(("word" . ,(match-string 1 item))
                               ("annotation" . ,(match-string 2 item)))
                           `(("word" . ,item)))))
                    (push cand-obj cands)))

                (when cands
                  (let ((target-hash (if (eq current-section 'okuri-ari)
                                         okuri-ari-hash
                                       okuri-nasi-hash)))
                    (puthash midasi (nreverse cands) target-hash)))))))

          ;; 次の行へ移動。これ以上進めない場合はループを終了する
          (let ((old-point (point)))
            (if (= (forward-line 1) 0)
                (when (= (point) old-point)
                  (setq continue nil))
              (setq continue nil)))))

    (let* ((root-obj `(("meta" . (("version" . "1.0")
                                  ("generator" . "skk2json.el")))
                       ("okuri_ari" . ,okuri-ari-hash)
                       ("okuri_nasi" . ,okuri-nasi-hash)))
           (json-encoding-pretty-print t))
      (with-temp-file output-file
        (insert (json-encode root-obj))))
      (message "Successfully converted SKK dictionary to Hash-format JSON: %s!" output-file))))

(provide 'skk2json)
;;; skk2json.el ends here
