;;; kotoni-soen.el --- contextual candidate ranking for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.2
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later

;;; Commentary:

;; o システム辞書に基づく漢字候補の検索は、常に静的順序
;; (ktn--search-jisyo-common "しょうごう")
;; => (("称号" "title" system) ("商号" "trade name" system) ("照合" "match" system) ...)
;;
;; o kotoni-soen では、辞書順を優先順位の初期値として使いつつ、
;;   バッファの内容や文脈で順序を補正する
;; (ktn-soen-search "しょうごう")
;; => (("照合" "match" system) ("商号" "trade name" system) ("稱號" "「号」旧字" system) ...)
;;
;;   処理フロー:
;;   ユーザー
;;     │
;;     └─ (ktn-soen-search "しょうごう")
;;            │
;;            ├─ ktn--search-jisyo-static
;;            │     │
;;            │     └─ ktn-search-jisyo-combined
;;            │            │
;;            │            └─ ktn--search-jisyo-common
;;            │                   │
;;            │                   └─ 静的候補を返す
;;            │
;;            └─ ktn--boost-candidates
;;                   │
;;                   ├─ ktn--ensure-document-context
;;                   │     │
;;                   │     ├─ ktn--sentence-at-point
;;                   │     │     └─ 現在バッファの文を取得
;;                   │     │
;;                   │     └─ ktn--build-document-context
;;                   │           │
;;                   │           ├─ ktn--tokenize-text
;;                   │           │     └─ 文中の語を分割
;;                   │           └─ hash-table に単語頻度・bigram を格納
;;                   │
;;                   └─ 各候補に対して
;;                         ├─ ktn--candidate-context-score
;;                         │    │
;;                         │    ├─ ktn--document-context の :terms を読む
;;                         │    ├─ ktn--document-context の :bigrams を読む
;;                         │    └─ スコア計算
;;                         │
;;                         └─ sort により再並び替え
;;
;; 補足:
;; - ktn--document-context はバッファローカル
;; - バッファ終了時に ktn--drop-document-context で破棄
;; - 検索戦略の切り替えは ktn-jisyo-search-switch で行う
;;
;; (let ((test-search-jisyo #'(lambda (midasi &optional enable-context)
;;                              (let ((ktn-search-jisyo-enable-context enable-context))
;;                                (mapconcat #'car (ktn--search-jisyo midasi) " "))))
;;       (midasi "しょうごう")
;;       result-off result-on)
;;   (with-temp-buffer
;;     (insert "照合の作業が必要です。しょうごうを確認する。\n")
;;     (goto-char (point-min))
;;     (setq result-off (format "context: nil, %s" (funcall test-search-jisyo midasi  )))
;;     (setq result-on  (format "context: t,   %s" (funcall test-search-jisyo midasi t))))
;;   (insert result-off "\n" result-on))
;;
;; => context: nil, 称号 商号 照合 正業 稱號
;; => context: t,   照合 商号 称号 稱號 正業

;;; Code:

(require 'cl-lib)

(declare-function ktn-search-jisyo-combined "kotoni-data" (midasi))
(declare-function ktn--search-jisyo-common "kotoni-data" (midasi))

(defcustom ktn-search-jisyo-enable-context t
  "Enable jisyo-search with document-context-based re-ranking."
  :type 'boolean
  :group 'kotoni)

(defvar-local ktn--document-context nil
  "Buffer-local document context for contextual candidate ranking.

The value is a plist with:
  :terms    => hash-table word -> frequency
  :bigrams  => hash-table \"left|right\" -> frequency
  :tokens   => vector/list of tokens in current sentence
  :sentence => current sentence text
")

(defun ktn--drop-document-context ()
  "Drop buffer-local context when buffer is killed."
  (setq-local ktn--document-context nil))

(add-hook 'kill-buffer-hook #'ktn--drop-document-context)

(defun ktn--tokenize-text (text)
  "Split text into token-like pieces.

This is intentionally simple and robust enough for general Japanese text:
- Japanese kanji/kana
- ASCII letters and digits
- It ignores punctuation and whitespace.
"
  (let ((re "\\(?:[[:alpha:][:digit:]]+\\|[^[:ascii:]]+\\)")
        tokens)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let ((tok (match-string 0)))
          (unless (or (string-empty-p tok)
                      (string-match-p "\\`[[:space:][:punct:]]+\\'" tok))
            (push tok tokens))))
      (nreverse tokens))))

(defun ktn--sentence-at-point ()
  "Return sentence text around point."
  (save-excursion
    (let* ((beg (progn (backward-sentence 1) (point)))
           (end (progn (forward-sentence 1) (point))))
      (buffer-substring-no-properties beg end))))

(defun ktn--build-document-context (sentence)
  "Build a hash-based context model for a sentence.
This is kept buffer-local to avoid cross-buffer contamination."
  (let ((terms (make-hash-table :test 'equal))
        (bigrams (make-hash-table :test 'equal))
        (tokens (ktn--tokenize-text sentence))
        prev)
    (dolist (tok tokens)
      (unless (string-empty-p tok)
        (puthash tok (1+ (gethash tok terms 0)) terms)
        (when prev
          (let ((key (concat prev "|" tok)))
            (puthash key (1+ (gethash key bigrams 0)) bigrams)))
        (setq prev tok)))
    (list :terms terms
          :bigrams bigrams
          :tokens tokens
          :sentence sentence)))

(defun ktn--ensure-document-context ()
  "Ensure buffer-local context is current for the current sentence."
  (let* ((sentence (ktn--sentence-at-point))
         (ctx ktn--document-context))
    (unless (and ctx
                 (plist-get ctx :sentence)
                 (string= (plist-get ctx :sentence) sentence))
      (setq-local ktn--document-context (ktn--build-document-context sentence)))
    ktn--document-context))

(defun ktn--candidate-context-score (candidate ctx)
  "Score a single candidate using the current sentence context."
  (let* ((terms    (plist-get ctx :terms))
         (bigrams  (plist-get ctx :bigrams))
         (tokens   (plist-get ctx :tokens))
         (word     (car candidate))
         (annot    (nth 1 candidate))
         (source   (or (nth 2 candidate) 'system))
         (score    0))

    ;; 1. exact repetition in the current sentence gives strong bonus
    (when (member word tokens)
      (cl-incf score 60))

    ;; 2. lexical frequency in current sentence
    (cl-incf score (* 15 (gethash word terms 0)))

    ;; 3. local co-occurrence with nearby tokens
    (dolist (tok tokens)
      (unless (string= tok word)
        (let ((key-left  (concat tok "|" word))
              (key-right (concat word "|" tok)))
          (cl-incf score (* 30 (gethash key-left  bigrams 0)))
          (cl-incf score (* 30 (gethash key-right bigrams 0))))))

    ;; 4. user/history weight
    (when (eq source 'user)
      (cl-incf score 20))

    ;; 5. annotation-based small penalty for archaic/domain-specific labels
    (when (and annot
               (string-match-p "\\(旧字\\|仏教\\|古語\\|古典\\)" annot))
      (cl-decf score 10))

    ;; 6. candidate length penalty for very long or uncommon forms
    (when (> (length word) 8)
      (cl-decf score 5))

    score))

(defun ktn--boost-candidates (candidates)
  "Re-rank candidates based on buffer-local context.
CANDIDATES shape: (WORD ANNOTATION SOURCE)
Example: (\"照合\" \"match\" system)"
  (when (null candidates)
    (return-from ktn--boost-candidates nil))

  (let* ((ctx (ktn--ensure-document-context))
         scored)
    (dolist (cand candidates)
      (let* ((word  (nth 0 cand))
             (annot (nth 1 cand))
             (src   (or (nth 2 cand) 'system))
             (score (ktn--candidate-context-score cand ctx)))
        (push (list word annot src score) scored)))

    (let* ((sorted (sort scored
                        (lambda (a b)
                          (> (nth 3 a) (nth 3 b)))))
           (out nil))
      (dolist (item sorted)
        (push (list (nth 0 item) (nth 1 item) (nth 2 item)) out))
      (nreverse out))))

(defun ktn--search-jisyo (midasi)
  "Dispatch according to `ktn-jisyo-search-switch'."
  (let ((cands (ktn-search-jisyo-combined midasi)))
    (if ktn-search-jisyo-enable-context
        (ktn--boost-candidates cands)
      cands)))

(provide 'kotoni-soen)
;;; kotoni-soen.el ends here
