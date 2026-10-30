;;; ktn-test.el --- E2E tests for Kotoni (JSON Dictionary Support) -*- lexical-binding: t; -*-

;;; Commentary:
;; DDKKK/TEST/test-utils.el を参考にさせていただきました。

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
    (setq body `(ktn-e2e-test
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

(defun ktn-e2e-test (&rest actions)
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

;;;
;;; Test Cases (JSON Dictionary Format)
;;;

(ktn-define-e2e-test basic-henkan/test1
  "送り有りと送り無しでJSON辞書から変換できる。"
  (actions #'ktn-mode
           "K a n J i C-j"
           "K a n j i SPC C-j")
  (expected-buffer-string "感じ漢字"))

(ktn-define-e2e-test ktn-previous-candidate/test1
  "JSON辞書を用いたxでの前候補への戻りテスト。"
  (actions #'ktn-mode
           "K a n j i SPC x C-j")
  (expected-buffer-string "幹事"))

;; (ktn-define-e2e-test ktn-kutouten-henkan/test1
;;   "JSON辞書を用いたでの句読点で変換開始。"
;;   (actions #'ktn-mode
;;            "K a n j i . C-j")
;;   (expected-buffer-string "漢字。"))

(ktn-define-e2e-test recursive-dict-registration/test1
  "未知の複合語登録時に再帰的な辞書登録を行える。"
  (actions #'ktn-mode
           ;; 1. 「さいきてき」を入力して変換（SPC）→ 辞書にないため「さいきてき」の登録モードへ
           "S a i k i t e k i SPC"
           ;; 2. さらに「さいき」を入力して変換（SPC）→ 「さいき」もないため再帰的な登録モードへ
           "S a i k i SPC"
           ;; 3. 「さい」と「き」をそれぞれ変換して「再帰」を作り、C-c C-c で「さいき」の登録を完了させる
           "S a i SPC K i SPC C-c C-c"
           ;; 4. 親の「さいきてき」の登録モードに戻り、「さいき」を変換（再帰）させ、
           ;;    「てき」は既存の辞書（的）から引いて「さいきてき」を完成させる
           "T e k i SPC"
           ;; 5. 最終的な確定操作
           "C-j C-c C-c")
  (expected-buffer-string "再帰的"))

(ktn-define-e2e-test ktn-mode-off/test1
  "もう一度呼ぶとKTNモードを切れる。"
  (actions (lambda () (ktn-mode 1))
           (lambda () (ktn-mode -1))
           "a")
  (expected-buffer-string "a"))

(provide 'ktn-test)

;;; ktn-test.el ends here
