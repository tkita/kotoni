;;; skk2json.el--- SKK JISYO to Trie JSON converter -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.2
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (json))

;;; Commentary:
;; Convert SKK JISYO dictionary files into Trie-indexed JSON.
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
;;   (skk2json-merge-skk-dicts-to-json skk-files (expand-file-name "SKK-JISYO.trie.json" root)))
;; == *scratch* ==

;; ## 辞書 JSON の構成 ##
;; [ ktn辞書 (Root JSON Object) ]
;;  ├── "meta" (メタデータ)
;;  │    ├── version: "2.0"
;;  │    ├── generator: "skk2json.el"
;;  │    └── trie_indexed: true
;;  │
;;  ├── "okuri_ari" (送りありエントリセクション)
;;  │    └── キー: "あいs"
;;  │         └── 配列: [ { "word": "愛", "annotation": "あいする" } ]
;;  │
;;  ├── "okuri_nasi" (送りなしエントリセクション)
;;  │    ├── キー: "あいす"
;;  │         └── 配列: [ { "word": "アイス" } ]
;;  │    └── キー: "あいすくりーむ"
;;  │         └── 配列: [ { "word": "アイスクリーム" } ]
;;  │
;;  └── "trie_index" (O(1) 高速プレフィックスインデックス)
;;       ├── キー: "あ"
;;       │    └── 配列 (長語優先ソート済み):
;;       │         ├── [0] headword: "あいすくりーむ" -> candidates: [...]
;;       │         └── [1] headword: "あいす"         -> candidates: [...]
;;       │
;;       ├── キー: "あい"
;;       │    └── 配列 (長語優先ソート済み):
;;       │         ├── [0] headword: "あいすくりーむ" -> candidates: [...]
;;       │         └── [1] headword: "あいす"         -> candidates: [...]
;;       │
;;       └── キー: "あいす"
;;            └── 配列 (長語優先ソート済み):
;;                 ├── [0] headword: "あいすくりーむ" -> candidates: [...]
;;                 └── [1] headword: "あいす"         -> candidates: [...]

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun skk2json--skip-entry-p (line headword)
  "見出し語 HEADWORD の先頭1文字が ASCII 文字（英字・数字・記号など）であるか判定する。"
  (cond
   ;; コメント行・メタ行（;, >, # で始まる行）をスキップ
   ((or (string-prefix-p ";" line)
        (string-prefix-p ">" line)
        (string-prefix-p "#" line))
    t)

   ;; 先頭が ASCII 文字で始まる見出し語をスキップ（「ASCII+ひらがな」もスキップ）
   ((string-match-p "\\`[[:ascii:]]" headword)
    t)

   ;; 見出し語の末尾が '>' であるものをスキップ
   ((string-suffix-p ">" headword)
    t)

   ;; 見出し語に '#' が含まれるものをスキップ（数値変換用エントリなど）
   ((string-match-p "#" headword)
    t)

   ;; 結果、ひらがな単体や「ひらがな+ASCII」（送りありエントリ）は保持
   (t
    nil)))

(defun skk2json--union-candidates (existing new-cands)
  "既存の候補リストと新しい候補リストを結合し、順序を維持したまま重複を排除する。"
  (let ((seen (make-hash-table :test 'equal))
        result)
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
          (insert-file-contents path)
          (setq total (+ total (count-lines (point-min) (point-max)))))))))

(defun skk2json--parse-skk-dict-with-progress (filepath nasi-table ari-table state)
  "SKK辞書ファイル FILEPATH を読み込みながら、進捗を計算・表示してマージする。"
  (when (file-exists-p filepath)
    (with-temp-buffer
      (let ((current-type 'nasi))
        (insert-file-contents filepath)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position)
                                                                   (line-end-position)))))
            
            ;; 処理行数をインクリメント
            (plist-put state :processed (1+ (plist-get state :processed)))
            
            ;; 進捗率の計算と表示
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

(defun skk2json--build-trie-index (okuri-nasi-hash)
  "送りなし見出し語と漢字候補を統合したプレフィックスインデックス（Trie木）を生成する。
処理中の進捗をメッセージに表示する。"
  (message "Building unified Trie index (headwords + candidates)...")
  (let* ((trie-hash (make-hash-table :test 'equal))
         (total-keys (hash-table-count okuri-nasi-hash))
         (processed 0)
         (last-percent -1))

    ;; 1. 各見出し語の全プレフィックスに対し、見出し語と候補のペアを登録
    (maphash
     (lambda (headword candidates)
       ;; 進捗カウントとログ表示
       (setq processed (1+ processed))
       (when (> total-keys 0)
         (let* ((percent (/ (* processed 100) total-keys))
                (safe-percent (min percent 100)))
           (when (and (/= safe-percent last-percent) (or (= safe-percent 100) (= (% safe-percent 5) 0)))
             (setq last-percent safe-percent)
             (message "Building Trie... %d%% (%d / %d headwords)" safe-percent processed total-keys))))

       ;; プレフィックスごとのインデックス展開
       (let ((len (length headword)))
         (cl-loop for i from 1 to len do
                  (let* ((prefix (substring headword 0 i))
                         (entry `(("headword" . ,headword)
                                  ("candidates" . ,candidates)))
                         (existing (gethash prefix trie-hash)))
                    (unless (cl-find headword existing
                                     :test (lambda (hw item)
                                             (string= hw (cdr (assoc "headword" item)))))
                      (puthash prefix (cons entry existing) trie-hash))))))
     okuri-nasi-hash)

    ;; 2. 各プレフィックス内の候補群を「見出し語の長さの降順（長語優先）」にソート
    (message "Sorting Trie index candidates...")
    (let ((sorted-count 0)
          (total-trie-keys (hash-table-count trie-hash)))
      (maphash
       (lambda (prefix entry-list)
         (setq sorted-count (1+ sorted-count))
         ;; ソート処理の進捗も必要に応じて（数が多い場合は軽微なためメッセージは省略可）
         (let ((sorted (sort entry-list
                             (lambda (a b)
                               (let ((hw-a (cdr (assoc "headword" a)))
                                     (hw-b (cdr (assoc "headword" b))))
                                 (if (= (length hw-a) (length hw-b))
                                     (string< hw-a hw-b)
                                   (> (length hw-a) (length hw-b))))))))
           (puthash prefix sorted trie-hash)))
       trie-hash))
    
    (message "Trie index build completed.")
    trie-hash))

(defun skk2json-merge-skk-dicts-to-json (file-paths output-file)
  "複数のSKK辞書ファイルを読み込み、Trie構造インデックス付きのJSONを出力する。"
  (let* ((total-lines     (skk2json--count-total-lines file-paths))
         (state           (list :total total-lines :processed 0 :last-percent -1))
         (okuri-nasi-hash (make-hash-table :test 'equal))
         (okuri-ari-hash  (make-hash-table :test 'equal)))
    
    (message "Start parsing %d files (%d lines)..." (length file-paths) total-lines)
    
    ;; 各ファイルを順次読み込んで分類・統合
    (dolist (path file-paths)
      (skk2json--parse-skk-dict-with-progress path okuri-nasi-hash okuri-ari-hash state))

    ;; Trie インデックスの構築
    (let ((trie-hash (skk2json--build-trie-index okuri-nasi-hash)))
      
      (message "Encoding JSON and writing to disk...")
      (let* ((generated-time (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
             (root-obj `(("meta" . (("version" . "2.0")
                                    ("generator" . "skk2json.el")
                                    ("trie_indexed" . t)
                                    ("generated_at" . ,generated-time)))
                         ("okuri_ari" . ,okuri-ari-hash)
                         ("okuri_nasi" . ,okuri-nasi-hash)
                         ("trie_index" . ,trie-hash)))
             (json-encoding-pretty-print t))
        (with-temp-file output-file
          (let ((coding-system-for-write 'utf-8))
            (insert (json-encode root-obj)))))
      
      (message "Successfully saved merged trie-JSON to: %s" output-file))))

(provide 'skk2json)
;;; skk2json.el ends here
