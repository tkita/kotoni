;;; skk2json.el --- SKK JISYO to JSON converter -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (json))
;; License: GPL-3.0-or-later

;;; Commentary:
;; Convert a SKK JISYO dictionary file into JSON.
;; == *scratch* ==
;; (let* ((root (expand-file-name "~/jisyo"))
;;        (files '("SKK-JISYO.L"
;;                 "SKK-JISYO.geo"
;;                 "SKK-JISYO.jinmei"
;;                 "SKK-JISYO.law"
;;                 "SKK-JISYO.propernoun"
;;                 "SKK-JISYO.station"))
;;        (skk-files (mapcar (lambda (file)
;;                             (expand-file-name file root)) files)))
;;   (skk2json-merge-skk-dicts-to-json skk-files (expand-file-name "jisyo.json" root)))
;; == *scratch* ==

;;; Code:
(require 'cl-lib)
(require 'json)

(defun skk2json--skip-entry-p (line headword)
  "見出し語 HEADWORD の先頭1文字が ASCII 文字（英字・数字・記号など）であるか判定する。"
  (cond
   ;; 2. 行頭が ';' OR '>' OR '#' で始まる行
   ((or (string-prefix-p ";" line)
        (string-prefix-p ">" line)
        (string-prefix-p "#" line))
    t)
   ;; 1. 見出し語のすべてが ASCII で構成されている場合
   ((string-match-p "\\`[[:ascii:]]+\\'" headword)
    t)
   ;; 3. 見出し語の末尾が '>' である場合
   ((string-suffix-p ">" headword)
    t)
   ;;
   ((string-match-p "#" headword)
    t)
   ;;
   (t
    nil)))

(defun skk2json--union-candidates (existing new-cands)
  "既存の候補リストと新しい候補リストを結合し、順序を維持したまま重複を排除する。"
  (let ((seen   (make-hash-table :test 'equal))
        (result nil))
    (dolist (c (append existing new-cands))
      (unless (gethash c seen)
        (puthash c t seen)
        (push c result)))
    (nreverse result)))

(defun skk2json--count-total-lines (file-paths)
  "指定された複数のSKK辞書ファイルの総行数をあらかじめカウントする。"
  (let ((total 0))
    (dolist (path file-paths total)
      (when (file-exists-p path)
        (with-temp-buffer
          ;; (let ((coding-system-for-read 'euc-jp))
          (insert-file-contents path)
          (setq total (+ total (count-lines (point-min) (point-max))))
          )))))

(defun skk2json--parse-skk-dict-with-progress (filepath nasi-table ari-table state)
  "SKK辞書ファイル FILEPATH を読み込みながら、進捗を計算・表示してマージする。"
  (when (file-exists-p filepath)
    (with-temp-buffer
      (let ( ;(coding-system-for-read 'euc-jp)
            (current-type 'nasi))
        (insert-file-contents filepath)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position)
                                                                   (line-end-position)))))
            
            ;; 処理行数をインクリメント
            (plist-put state :processed (1+ (plist-get state :processed)))
            
            ;; 進捗率の計算と表示（総行数を超えないように min で保護）
            (let* ((total-lines     (plist-get state :total))
                   (processed-lines (plist-get state :processed)))
              (when (> total-lines 0)
                (let* ((safe-processed (min processed-lines total-lines))
                       (percent        (/ (* safe-processed 100) total-lines))
                       (last-percent   (plist-get state :last-percent)))
                  (when (and (/= percent last-percent) (or (= percent 100) (= (% percent 5) 0)))
                    (plist-put state :last-percent percent)
                    (message "Converting... %d%% (%d / %d lines)" percent safe-processed total-lines)))))

            (cond
             ;; セクション切替の検知
             ((string-match-p "\\`;; okuri-ari entries" line)
              (setq current-type 'ari))
             ((string-match-p "\\`;; okuri-nasi entries" line)
              (setq current-type 'nasi))
             
             ;; コメント行や空行はスキップ
             ((or (string-empty-p line)
                  (string-prefix-p ";" line)))
             
             (t
              ;; エントリ行のパース
              (when (string-match "\\`\\([^ ]+\\) /\\(.+\\)/\\'" line)
                (let* ((headword        (match-string 1 line))
                       (candidates-part (match-string 2 line))
                       (raw-cands       (split-string candidates-part "/" t))
                       (candidates      (mapcar #'skk2json--format-candidate raw-cands))
                       (target-table    (if (eq current-type 'ari)
                                            ari-table
                                          nasi-table)))
                  
                  ;; 記号・英字・数字など ASCII 見出しの場合はスキップ
                  (unless (skk2json--skip-entry-p line headword)
                    (let ((existing (gethash headword target-table)))
                      (if existing
                          (puthash headword (skk2json--union-candidates existing candidates) target-table)
                        (puthash headword candidates target-table)))))))))
          (forward-line 1))))))

(defun skk2json--format-candidate (cand-str)
  "候補文字列 CAND-STR をパースし、常に word / annotation を持つオブジェクト形式に整形する。"
  (let ((trimmed (string-trim cand-str)))
    (if (string-match "\\`\\([^;]+\\);\\s-*\\(.+\\)\\'" trimmed)
        `(("word" . ,(string-trim (match-string 1 trimmed)))
          ("annotation" . ,(string-trim (match-string 2 trimmed))))
      `(("word" . ,trimmed)))))

(defun skk2json-merge-skk-dicts-to-json (file-paths output-file)
  "複数のSKK辞書ファイルを読み込み、ASCII（記号・英字・数字）をスキップしながらJSONに出力する。"
  (let* ((total-lines     (skk2json--count-total-lines file-paths)) ;複数のSKK辞書ファイルの総行数
         (state           (list :total total-lines :processed 0 :last-percent -1))
         (okuri-nasi-hash (make-hash-table :test 'equal))
         (okuri-ari-hash  (make-hash-table :test 'equal)))
    
    (message "Start parsing %d files (%d lines)..." (length file-paths) total-lines)
    
    ;; 各ファイルを順次読み込んで分類・統合
    (dolist (path file-paths)
      (skk2json--parse-skk-dict-with-progress path okuri-nasi-hash okuri-ari-hash state))

    ;; オリジナルの出力構造（meta, okuri_ari, okuri_nasi）
    (let* ((generated-time (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
           (root-obj `(("meta" . (("version" . "1.0")
                                  ("generator" . "skk2json.el")
                                  ("generated_at" . ,generated-time)))
                       ("okuri_ari" . ,okuri-ari-hash)
                       ("okuri_nasi" . ,okuri-nasi-hash)))
           (json-encoding-pretty-print t))
      (with-temp-file output-file
        (let ((coding-system-for-write 'utf-8))
          (insert (json-encode root-obj)))))
    
    (message "Successfully saved merged dictionary to: %s" output-file)))

(provide 'skk2json)
;;; skk2json.el ends here
