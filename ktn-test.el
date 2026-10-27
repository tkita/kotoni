;;; ktn-test.el --- E2E tests for Kotoni (JSON Dictionary Support) -*- lexical-binding: t; -*-

;;; Commentary:
;; DDKKK/TEST/test-utils.el を参考にさせていただきました。
;; 

(require 'ert)
(require 'json)
(require 'kotoni)

;;;
;;; Test Utilities (JSON Dictionary Version)
;;;

(defmacro ktn-define-e2e-test (name &rest args)
  "Kotoni用のE2Eテストを定義する（JSON辞書対応版）。"
  (declare (indent defun))
  (let* ((docstring (car args))
         (have-skip-when (assoc 'skip-when args))
         (skip-when (cdr (assoc 'skip-when args)))
         (okuri-ari-entries (cdr (assoc 'okuri-ari-entries args)))
         (okuri-nasi-entries (cdr (assoc 'okuri-nasi-entries args)))
         (actions (cdr (assoc 'actions args)))
         (expected-buffer-string (cdr (assoc 'expected-buffer-string args)))
         body)
    (when (consp expected-buffer-string)
      (setq expected-buffer-string (car expected-buffer-string)))
    (when (and (consp skip-when)
               (not (functionp (car skip-when)))
               (not (special-form-p (car skip-when)))
               (not (macrop (car skip-when))))
      (setq skip-when (car skip-when)))
    ;; ★ リストが評価されないようクォート (') を付与する
    (setq body `(ktn-e2e-test
                 ',okuri-ari-entries
                 ',okuri-nasi-entries
                 ,@actions
                 ,(when expected-buffer-string
                    `(lambda ()
                       (should (equal
                                (buffer-substring-no-properties (point-min)
                                                                (point-max))
                                ,expected-buffer-string))))))
    `(ert-deftest ,name ()
       ,@(when (stringp docstring)
           (list docstring))
       ,@(when have-skip-when
           (list `(skip-unless (not ,skip-when))))
       ,body)))

(defun ktn-e2e-test (okuri-ari-entries okuri-nasi-entries &rest actions)
  "テスト用の実ファイル JSON 辞書 (ktn-test-jisyo.json) を用いて ACTIONS を実行する。"
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (let* ((ktn-jisyo-file (expand-file-name "ktn-test-jisyo.json"))
           (ktn-stats-file (make-temp-file "ktn-stats" nil ".json" "{}"))
           (ktn-dictionaries nil)
           (ktn--stats-data (make-hash-table :test 'equal))
           (_ (setf (ktn-dictionary-data ktn--system-jisyo-instance) nil
                    (ktn-dictionary-buffer ktn--system-jisyo-instance) nil)))
      (unwind-protect
          (progn
            (ktn-setup)
            (dolist (action actions)
              (cond
               ((stringp action) (execute-kbd-macro (read-kbd-macro action)))
               ((functionp action) (funcall action))
               (t (eval action))))
            (buffer-substring-no-properties (point-min) (point-max)))
        (when (file-exists-p ktn-stats-file)
          (delete-file ktn-stats-file))))))

(defun ktn-compose-test-json-dictionary (okuri-ari-entries okuri-nasi-entries)
  "OKURI-ARI-ENTRIES と OKURI-NASI-ENTRIES から JSON 形式のテスト辞書文字列を生成する。"
  (let ((root (make-hash-table :test 'equal))
        (ari-ht (make-hash-table :test 'equal))
        (nasi-ht (make-hash-table :test 'equal)))
    (when okuri-ari-entries
      (dolist (entry okuri-ari-entries)
        ;; ベクタ形式で格納
        (puthash (car entry) (vconcat (cdr entry)) ari-ht))
      (puthash "okuri_ari" ari-ht root))
    (when okuri-nasi-entries
      (dolist (entry okuri-nasi-entries)
        ;; ベクタ形式で格納
        (puthash (car entry) (vconcat (cdr entry)) nasi-ht))
      (puthash "okuri_nasi" nasi-ht root))
    (let ((json-encoding-pretty-print nil))
      (json-encode root))))

(defun ktn-with-temporary-file (prefix content body)
  "一時ファイルを作成して BODY を呼び出す。"
  (let (file-name)
    (unwind-protect
        (progn
          (setq file-name
                (make-temp-file
                 (expand-file-name
                  prefix
                  (or small-temporary-file-directory
                      temporary-file-directory))))
          (with-temp-buffer
            (insert content)
            (write-region (point-min) (point-max) file-name))
          (funcall body file-name))
      (when file-name
        (delete-file file-name)))))

(defun ktn-with-temporary-files (prefixes-and-contents body)
  "一時ファイルを複数作成して BODY を呼び出す。"
  (if prefixes-and-contents
      (ktn-with-temporary-file
       (caar prefixes-and-contents)
       (cdar prefixes-and-contents)
       (lambda (file-name)
         (ktn-with-temporary-files
          (cdr prefixes-and-contents)
          (lambda (&rest file-names)
            (apply body (cons file-name file-names))))))
    (funcall body)))


;;;
;;; Test Cases (JSON Dictionary Format)
;;;

(ktn-define-e2e-test basic-henkan/test1
  "送り有りと送り無しでJSON辞書から変換できる。"
  (okuri-ari-entries ("かんj" . ["感"]))
  (okuri-nasi-entries ("かんじ" . ["漢字" "幹事"]))
  (actions #'ktn-mode
           "K a n J i C-j"
           "K a n j i SPC C-j")
  (expected-buffer-string "感じ漢字"))

(ktn-define-e2e-test ktn-previous-candidate/test1
  "JSON辞書を用いたxでの前候補への戻りテスト。"
  (okuri-nasi-entries ("かんじ" . ["漢字" "幹事"]))
  (actions #'ktn-mode
           "K a n j i SPC x C-j")
  (expected-buffer-string "幹事"))

;; (ktn-define-e2e-test ktn-kutouten-henkan/test1
;;   "JSON辞書を用いたでの句読点で変換開始。"
;;   (okuri-nasi-entries ("かんじ" . ["漢字" "幹事"]))
;;   (actions #'ktn-mode
;;            "K a n j i . C-j")
;;   (expected-buffer-string "漢字。"))

(ktn-define-e2e-test ktn-mode-off/test1
  "もう一度呼ぶとKTNモードを切れる。"
  (actions (lambda () (ktn-mode 1))
           (lambda () (ktn-mode -1))
           "a")
  (expected-buffer-string "a"))

(provide 'ktn-test)

;;; ktn-test.el ends here
