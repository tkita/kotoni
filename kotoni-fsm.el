;;; kotoni-fsm.el --- FSM State Transition Layer for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'kotoni-data)

;;; データオブジェクト定義

(defvar-local ktn--normal-roma-buffer ""
  "通常モード時の一時ローマ字バッファ")

(cl-defstruct (ktn-headword (:constructor ktn-headword--create))
  "見出し語入力・変換状態を保持するデータオブジェクト"
  (mode                  'headword)     ; 'headword | 'converting
  (buffer                "")            ; 見出し語テキスト
  (roma-buffer           "")            ; 入力中ローマ字
  (okuri-buffer          "")            ; 送り仮名テキスト
  (okuri-roma-buffer     "")            ; 送り仮名入力中ローマ字
  (okuri-roma-first      "")            ; 送り開始大文字
  (start-marker          nil)           ; 開始位置マーカー
  (candidates            nil)           ; 変換候補リスト
  (candidate-index       0)             ; 選択中インデックス
  (henkan-count          0)             ; 変換打鍵回数
  (completion-cands      nil)           ; 補完候補
  (completion-index      0)             ; 補完インデックス
  (completion-base       nil)           ; 補完基準テキスト
  (completion-tab-count  0)             ; TAB連打カウント
  (headword-overlay      nil)           ; オーバーレイ参照
  (pending-punct         "")            ; 句読点
  
  ;; hassamu 連携用フィールド
  (hassamu-active-p        nil)
  (hassamu-current-matches nil)
  (confirmed-text          nil)
  (hassamu-start-marker    nil))

(defvar-local ktn--current-headword nil
  "現在アクティブな `ktn-headword` オブジェクトインスタンス。")

(defun ktn--mode-p (mode)
  (and ktn--current-headword
       (eq (ktn-headword-mode ktn--current-headword) mode)))

(defun ktn--state-reset ()
  (setq ktn--current-headword nil)
  (setq ktn--normal-roma-buffer ""))

;;; ヘルパー関数

(defun ktn--to-char-int (ch)
  "入力文字 CH を安全に整数文字コードに変換する。"
  (cl-typecase ch
    ((or integer character) ch)
    (string (string-to-char ch))
    (t 0)))

(defun ktn--upper-case-p (ch-int)
  (and (<= ?A ch-int) (<= ch-int ?Z)))

(defun ktn--lower-case-p (ch-int)
  (and (<= ?a ch-int) (<= ch-int ?z)))

(defun ktn--number-key-p (ch-int)
  (and (<= ?1 ch-int) (<= ch-int ?9)))


(defun ktn--safe-char-to-string (ch)
  (cl-typecase ch
    (string (string-to-multibyte ch))
    ((or character integer) (char-to-string ch))
    (t "")))

(defun ktn--roma-prefix-p (prefix)
  (cl-some (lambda (pair)
             (string-prefix-p prefix (car pair)))
           ktn-roma-table))

(defun ktn--process-roma-input (old-roma char-str)
  "OLD-ROMA と入力文字 CHAR-STR からローマ字・かな変換を計算する。"
  (let* ((new-roma (concat old-roma char-str))
         (kana     (cdr (assoc new-roma ktn-roma-table))))
    (cond
     (kana
      (cons kana ""))

     ((and (string= old-roma "n")
           (not (member char-str '("a" "i" "u" "e" "o" "y" "n"))))
      (let ((next-kana (cdr (assoc char-str ktn-roma-table))))
        (cons (concat "ん" (or next-kana "")) (if next-kana "" char-str))))

     ((and (= (length old-roma) 1)
           (string= old-roma char-str)
           (string-match-p "^[b-df-hj-np-tv-z]$" char-str))
      (cons "っ" char-str))

     ((ktn--roma-prefix-p new-roma)
      (cons "" new-roma))

     (t
      (if (string-empty-p old-roma)
          (cons char-str "")
        (cons old-roma char-str))))))

(defun ktn--reset-completion (hw)
  (setf (ktn-headword-completion-base      hw) nil
        (ktn-headword-completion-tab-count hw) 0
        (ktn-headword-completion-index     hw) 0
        (ktn-headword-completion-cands     hw) nil))

;;; 各モードの入力ハンドラ

(defun ktn--handle-normal-mode (ch)
  "通常モード時の入力処理"
  (let* ((ch-int   (ktn--to-char-int ch))
         (is-upper (ktn--upper-case-p ch-int))
         (lower-ch (if is-upper (+ ch-int 32) ch-int))
         (is-alpha (ktn--lower-case-p lower-ch)))
    (cond
     ;; 大文字入力: 見出し語モード（▽）へ遷移
     (is-upper
      (setq ktn--normal-roma-buffer "")
      (ktn--start-headword-mode)
      (let* ((char-str (ktn--safe-char-to-string lower-ch))
             (res      (ktn--process-roma-input "" char-str)))
        (setf (ktn-headword-buffer ktn--current-headword) (car res)
              (ktn-headword-roma-buffer ktn--current-headword) (cdr res))))

     ;; 通常の文字入力
     (t
      (let* ((char-str (ktn--safe-char-to-string (if is-alpha lower-ch ch-int)))
             (res      (ktn--process-roma-input ktn--normal-roma-buffer char-str)))
        (setq ktn--normal-roma-buffer (cdr res))
        (unless (string-empty-p (car res))
          (insert (car res))))))))

(defun ktn--handle-headword-mode (ch &optional start-henkan-fn)
  "▽時の入力処理"
  (unless ktn--current-headword
    (ktn--start-headword-mode))
  
  (let* ((hw       ktn--current-headword)
         (ch-int   (ktn--to-char-int ch))
         (is-punct (member ch-int '(?, ?.)))
         (is-upper (ktn--upper-case-p ch-int))
         (lower-ch (if is-upper (+ ch-int 32)
                     ch-int))
         (is-alpha (ktn--lower-case-p lower-ch)))

    (ktn--reset-completion hw)

    (cond
     ;; 1. 句読点（, または .）が押された場合：ローマ字テーブルを参照して変換・ペンディング
     (is-punct
      (unless (string-empty-p (ktn-headword-roma-buffer hw))
        (let ((res (ktn--process-roma-input (ktn-headword-roma-buffer hw) "")))
          (setf (ktn-headword-buffer hw) (concat (ktn-headword-buffer hw) (car res))
                (ktn-headword-roma-buffer hw) "")))

      ;; 入力された文字（, や .）を文字列化し、ktn-roma-table（process-roma-input）経由で変換
      (let* ((char-str (char-to-string ch-int))
             (res (ktn--process-roma-input "" char-str))
             ;; 変換結果（car）があればそれを、なければそのままの文字を使う
             (punct-val (if (not (string-empty-p (car res)))
                            (car res)
                          char-str)))
        (setf (ktn-headword-pending-punct hw) punct-val))

      (when start-henkan-fn
        (funcall start-henkan-fn)))

     ;; 2. 大文字入力
     (is-upper
      (unless (string-empty-p (ktn-headword-roma-buffer hw))
        (let ((res (ktn--process-roma-input (ktn-headword-roma-buffer hw) "")))
          (setf (ktn-headword-buffer hw)
                (concat (ktn-headword-buffer hw) (car res))
                (ktn-headword-roma-buffer hw) (cdr res))))

      (when (string-suffix-p "n" (ktn-headword-buffer hw))
        (setf (ktn-headword-buffer hw)
              (concat (substring (ktn-headword-buffer hw) 0 -1) "ん")))

      (let* ((char-str (ktn--safe-char-to-string lower-ch))
             (res (ktn--process-roma-input "" char-str)))
        (setf (ktn-headword-okuri-roma-first hw) char-str
              (ktn-headword-okuri-buffer hw) (car res)
              (ktn-headword-okuri-roma-buffer hw) (cdr res))))

     ;; 3. 送り仮名入力中（ktn-headword-okuri-roma-first が空でない場合）
     ((not (string-empty-p (ktn-headword-okuri-roma-first hw)))
      (let* ((char-str (ktn--safe-char-to-string (if is-alpha lower-ch
                                                   ch-int)))
             (res (ktn--process-roma-input (ktn-headword-okuri-roma-buffer hw) char-str)))
        (setf (ktn-headword-okuri-buffer hw) (concat (ktn-headword-okuri-buffer hw) (car res))
              (ktn-headword-okuri-roma-buffer hw) (cdr res)))

      ;; 送り仮名のローマ字入力が完了し、ひらがなとして成立した瞬間に自動変換を開始
      (when (and (string-empty-p (ktn-headword-okuri-roma-buffer hw))
                 (not (string-empty-p (ktn-headword-okuri-buffer hw))))
        (when start-henkan-fn
          (funcall start-henkan-fn))))

     ;; 4. 通常の見出し語入力の継続
     (t
      (let* ((char-str (ktn--safe-char-to-string (if is-alpha lower-ch
                                                   ch-int)))
             (res (ktn--process-roma-input (ktn-headword-roma-buffer hw) char-str)))
        (setf (ktn-headword-buffer hw) (concat (ktn-headword-buffer hw) (car res))
              (ktn-headword-roma-buffer hw) (cdr res))))
     )))

(defun ktn--handle-converting-mode (ch)
  "▼時の入力処理。"
  (when ktn--current-headword
    (let* ((hw        ktn--current-headword)
           (ch-int    (ktn--to-char-int ch))
           (is-upper  (ktn--upper-case-p ch-int))
           (lower-ch  (if is-upper (+ ch-int 32) ch-int))
           (is-alpha  (ktn--lower-case-p lower-ch))
           (is-number (ktn--number-key-p ch-int)))

      (cond
       ;; 大文字: 現在の変換を確定し、新たな Headword を開始
       (is-upper
        (ktn-kakutei)
        (ktn--start-headword-mode)
        (let* ((char-str (ktn--safe-char-to-string lower-ch))
               (res (ktn--process-roma-input "" char-str)))
          (setf (ktn-headword-buffer ktn--current-headword) (car res)
                (ktn-headword-roma-buffer ktn--current-headword) (cdr res))
          (ktn--update-inline-display)))

       ;; 'x': 前候補選択
       ((or (= ch-int ?x)
            (eq ch 'x))
        (when (fboundp 'ktn-previous-candidate)
          (ktn-previous-candidate)))

       ;; 数字キー: 該当候補の選択・確定
       (is-number
        (let ((index (- ch-int ?1)))
          (ktn-select-nth-candidate index))
        (ktn-kakutei))

       ;; 小文字アルファベット: 確定して通常文字として挿入
       (is-alpha
        (ktn-kakutei)
        (let ((last-command-event lower-ch))
          (ktn--handle-normal-mode last-command-event)))

       (t nil)))))

(defun ktn--hiragana-to-katakana (str)
  "ひらがな文字列 STR をカタカナに変換する。"
  (concat
   (cl-loop for c across str
            collect (if (and (>= c #x3041)  ; ?ぁ
                             (<= c #x309F)) ; ?ゟ ※「より」という2文字を1文字にまとめた合字
                        (+ c #x60)
                      c))))

;;; ライフサイクル管理関数

(defun ktn--start-headword-mode ()
  "新しい `ktn-headword` オブジェクトを生成して初期化する。"
  (setq ktn--current-headword (ktn-headword--create :mode 'headword
                                                    :start-marker (point-marker))))

(defun ktn-select-nth-candidate (n)
  (when ktn--current-headword
    (let* ((candidates (ktn-headword-candidates ktn--current-headword))
           (total (if candidates (length candidates) 0)))
      (when (and (>= n 0) (< n total))
        (setf (ktn-headword-candidate-index ktn--current-headword) n)
        (ktn-kakutei)))))

(provide 'kotoni-fsm)
;;; kotoni-fsm.el ends here
