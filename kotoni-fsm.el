;;; kotoni-fsm.el --- FSM State Transition Layer for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later

;;; Commentary:
;; 第2階層：有限ステートマシンの状態管理（normal / headword / converting）とロジック計算を担当するモジュール。

;;; Code:

(require 'cl-lib)
(require 'kotoni-data)

;;; FSM 内部ステート変数とアクセサ
(defvar-local ktn--state nil)

(defun ktn--state-get (key &optional default)
  (let ((val (plist-get ktn--state key)))
    (cond
     (val val)
     (default default)
     ((memq key '(:headword-buffer
                  :roma-buffer
                  :okuri-buffer
                  :okuri-roma-buffer
                  :okuri-roma-first
                  :completion-base))
      "")
     (t nil))))

(defun ktn--state-set (key value)
  (setq ktn--state (plist-put ktn--state key value))
  value)

(defun ktn--mode-p (mode)
  (eq (ktn--state-get :mode) mode))

(defun ktn--state-reset ()
  (setq ktn--state
        (list :mode                  nil
              :headword-buffer       ""
              :roma-buffer           ""
              :okuri-buffer          ""
              :okuri-roma-buffer     ""
              :okuri-roma-first      ""
              :headword-start-marker nil
              :candidates            nil
              :candidate-index       0
              :henkan-count          0
              :completion-cands      nil
              :completion-index      0
              :completion-base       nil
              :completion-tab-count  0
              :headword-overlay      nil

              :hassamu-active-p        nil
              :hassamu-current-matches nil
              :confirmed-text          nil
              :hassamu-start-marker    nil
              )))

;;; ユーティリティ変換処理
(defun ktn--hiragana-to-katakana (str)
  (let ((rules '(("う゛" . "ヴ") ("ゔ" . "ヴ") ("ゝ" . "ヽ") ("ゞ" . "ヾ"))))
    (dolist (rule rules)
      (setq str (replace-regexp-in-string (car rule) (cdr rule) str)))
    (apply #'concat
           (mapcar (lambda (c)
                     (if (and (>= c ?ぁ) (<= c ?ん))
                         (char-to-string (+ c (- ?ァ ?ぁ)))
                       (char-to-string c)))
                   str))))

(defun ktn--safe-char-to-string (ch)
  (cond
   ((stringp ch) (string-to-multibyte ch))
   ((characterp ch) (char-to-string ch))
   ((integerp ch) (char-to-string ch))
   (t "")))

(defun ktn--roma-prefix-p (prefix)
  (cl-some (lambda (pair) (string-prefix-p prefix (car pair)))
           ktn-roma-table))

(defun ktn--process-roma-input (char-str)
  (let* ((old-roma (ktn--state-get :roma-buffer))
         (safe-char-str (ktn--safe-char-to-string char-str))
         (new-roma (concat old-roma safe-char-str))
         (kana (cdr (assoc new-roma ktn-roma-table))))
    (cond
     (kana (cons kana ""))
     ((and (string= old-roma "n")
           (not (member safe-char-str '("a" "i" "u" "e" "o" "y" "n"))))
      (let ((next-kana (cdr (assoc safe-char-str ktn-roma-table))))
        (cons (concat "ん" (or next-kana "")) (if next-kana "" safe-char-str))))
     ((and (= (length old-roma) 1)
           (string= old-roma safe-char-str)
           (string-match-p "^[b-df-hj-np-tv-z]$" safe-char-str))
      (cons "っ" safe-char-str))
     ((ktn--roma-prefix-p new-roma)
      (cons "" new-roma))
     (t
      (if (string-empty-p old-roma)
          (cons safe-char-str "")
        (cons old-roma safe-char-str))))))

(defun ktn--reset-completion ()
  (ktn--state-set :completion-base nil)
  (ktn--state-set :completion-tab-count 0)
  (ktn--state-set :completion-index 0)
  (ktn--state-set :completion-cands nil))


;;; モード別入力ディスパッチロジック (FSM Core)

(defun ktn--handle-normal-mode (ch)
  (let* ((is-upper (and (characterp ch) (<= ?A ch) (<= ch ?Z)))
         (lower-ch (if is-upper (+ ch 32) ch))
         (is-alpha (and (characterp lower-ch) (<= ?a lower-ch) (<= lower-ch ?z))))
    (cond
     ;; 大文字入力 -> ▽見出し語モードへ遷移
     (is-upper
      (unless (string-empty-p (ktn--state-get :roma-buffer))
        (insert (ktn--state-get :roma-buffer))
        (ktn--state-set :roma-buffer ""))
      (ktn--state-reset)
      (ktn--state-set :mode 'headword)
      (ktn--state-set :headword-start-marker (point-marker))
      (let* ((char-str (ktn--safe-char-to-string lower-ch))
             (res (ktn--process-roma-input char-str)))
        (ktn--state-set :headword-buffer (car res))
        (ktn--state-set :roma-buffer (cdr res))))

      ;; 通常の文字入力
     (t
      (let* ((char-str (ktn--safe-char-to-string (if is-alpha lower-ch ch)))
             (res (ktn--process-roma-input char-str)))
        (unless (string-empty-p (car res))
          (insert (car res)))
        (ktn--state-set :roma-buffer (cdr res)))))))

(defun ktn--handle-headword-mode (ch &optional start-henkan-fn)
  "見出し語入力モード（headword）中のキー入力 CH を処理する。"
  (let* ((ch-int (cond
                  ((integerp ch) ch)
                  ((characterp ch) ch)
                  ((stringp ch) (string-to-char ch))
                  (t 0)))
         (is-upper (and (<= ?A ch-int) (<= ch-int ?Z)))
         (lower-ch (if is-upper (+ ch-int 32) ch-int))
         (is-alpha (and (<= ?a lower-ch) (<= lower-ch ?z))))
    
    (ktn--reset-completion)

    (if is-upper
        ;; --- 大文字入力（送り仮名の入力開始トリガー） ---
        (progn
          ;; 1. 残っているローマ字バッファを見出し語バッファへ結合
          (unless (string-empty-p (ktn--state-get :roma-buffer))
            (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer) (ktn--state-get :roma-buffer)))
            (ktn--state-set :roma-buffer ""))
          ;; 2. 末尾の "n" を「ん」に確実な変換
          (when (string-suffix-p "n" (ktn--state-get :headword-buffer))
            (ktn--state-set :headword-buffer (concat (substring (ktn--state-get :headword-buffer) 0 -1) "ん")))

          ;; 3. 送り仮名側の初期設定（必ず安全な文字列に変換）
          (let ((char-str (ktn--safe-char-to-string lower-ch)))
            (ktn--state-set :okuri-roma-first char-str)
            (ktn--state-set :okuri-roma-buffer char-str)
            (ktn--state-set :okuri-buffer "")
            (let ((res (ktn--process-roma-input char-str)))
              (unless (string-empty-p (car res))
                (ktn--state-set :okuri-buffer (car res))
                (ktn--state-set :okuri-roma-buffer (cdr res))
                (when start-henkan-fn
                  (funcall start-henkan-fn))))))

      ;; --- 小文字入力（通常の見出し語または送りローマ字の継続） ---
      (if (string-empty-p (ktn--state-get :okuri-roma-first))
          ;; 見出し語の継続入力
          (let* ((char-str (ktn--safe-char-to-string (if is-alpha lower-ch ch-int)))
                 (res (ktn--process-roma-input char-str)))
            (ktn--state-set :headword-buffer (concat (ktn--state-get :headword-buffer) (car res)))
            (ktn--state-set :roma-buffer (cdr res)))

        ;; 送りローマ字の継続入力
        (let* ((char-str (ktn--safe-char-to-string (if is-alpha lower-ch ch-int)))
               (saved-roma (ktn--state-get :roma-buffer)))
          (ktn--state-set :roma-buffer (ktn--state-get :okuri-roma-buffer))
          (let ((res (ktn--process-roma-input char-str)))
            (ktn--state-set :okuri-buffer (concat (ktn--state-get :okuri-buffer) (car res)))
            (ktn--state-set :okuri-roma-buffer (cdr res))
            (ktn--state-set :roma-buffer saved-roma)
            (unless (string-empty-p (ktn--state-get :okuri-buffer))
              (when start-henkan-fn
                (funcall start-henkan-fn)))))))))

(defun ktn--start-headword-mode ()
  "新しい見出し語入力モード（▽）を開始するための共通初期化処理。"
  (ktn--state-reset)
  (ktn--state-set :mode 'headword)
  (ktn--state-set :headword-start-marker (point-marker)))

(defun ktn-select-nth-candidate (n)
  "N番目（0始まり）の変換候補を選択して確定する。"
  (interactive "p")
  ;; 1. 候補リストの範囲内かチェック
  (let* ((candidates (ktn--state-get :candidates))
         (total (if candidates (length candidates) 0)))
    (when (and (>= n 0) (< n total))
      ;; 2. 現在のインデックスを更新、または直接その候補で確定する
      (ktn--state-set :candidate-index n)
      (ktn-kakutei))))

(defun ktn--handle-converting-mode (ch)
  "変換モード（converting）中のキー入力 CH を処理する。"
  (let* ((ch-int (cond
                  ((integerp   ch) ch)
                  ((characterp ch) ch)
                  ((stringp    ch) (string-to-char ch))
                  (t 0)))
         (is-upper  (and (<= ?A ch-int) (<= ch-int ?Z)))
         (lower-ch  (if is-upper (+ ch-int 32) ch-int))
         (is-alpha  (and (<= ?a lower-ch) (<= lower-ch ?z)))
         (is-number (and (<= ?1 ch-int) (<= ch-int ?9))))

    (cond
     ;; 1. 変換中に大文字が入力された場合：現在の変換を確定し、新しい見出し語モードを開始
     (is-upper
      (ktn-kakutei)
      (ktn--start-headword-mode)
      (let ((char-str (ktn--safe-char-to-string lower-ch)))
        (ktn--state-set :roma-buffer char-str)
        (ktn--update-inline-display)))

     ;; 2. 'x' が入力された場合は前候補選択（ktn-previous-candidate）を実行
     ((or (= ch-int ?x) (eq ch 'x))
      (when (fboundp 'ktn-previous-candidate)
        (ktn-previous-candidate)))

     ;; 3. 数字キー（'1' 〜 '9'）が入力された場合：対応する候補を選択・確定
     (is-number
      (let ((index (- ch-int ?1)))
        (ktn-select-nth-candidate index))
      (ktn-kakutei))

     ;; 4. 【修正】変換中に小文字アルファベットが入力された場合：
     ;;    現在の変換を確定し、打たれた文字をそのまま通常入力（normal）として処理する
     (is-alpha
      (let ((char-str (ktn--safe-char-to-string lower-ch)))
        (ktn-kakutei)
        ;; 確定後の通常モード（normal）でその文字を改めて処理・挿入する
        (let* ((res (ktn--process-roma-input char-str)))
          (unless (string-empty-p (car res))
            (insert (car res)))
          (ktn--state-set :roma-buffer (cdr res)))))

     ;; 4. その他の変換中操作（必要に応じてスペースや次候補など）
     (t
      ;; 必要に応じたデフォルトの変換中キー処理
      ))))

(provide 'kotoni-fsm)
;;; kotoni-fsm.el ends here
