;;; ktn-test.el --- E2E tests for Kotoni (JSON Dictionary Support) -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later

;;; Commentary:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'kotoni)

(defmacro ktn--silence (&rest body)
  "テスト時に message / minibuffer のノイズを抑制する。"
  `(let ((inhibit-message t)
         (message-log-max nil)
         (minibuffer-message-timeout 0))
     ,@body))

(defun ktn--setup-test-encoding ()
  "テスト環境での文字化けを避ける。"
  (prefer-coding-system 'utf-8)
  (set-language-environment "Japanese")
  (setq locale-coding-system 'utf-8-unix
        default-file-name-coding-system 'utf-8-unix
        file-name-coding-system 'utf-8-unix
        network-coding-system 'utf-8-unix))

(defun ktn--test-file-dir ()
  (or
   (and load-file-name (file-name-directory load-file-name))
   (and buffer-file-name (file-name-directory buffer-file-name))
   default-directory
   (expand-file-name "./")))

(defun ktn--run-action-sequence (s)
  "例: \"K a n j i SPC C-j\" を KTN FSM に直接 feed する。"
  (cl-loop for tok in (split-string s "[[:space:]]+" t)
           do
           (let ((keys (read-kbd-macro tok)))
             (unless (vectorp keys)
               (error "Bad key sequence for token: %S" tok))
             (cl-loop for event across keys
                      do
                      (let ((last-command-event event))
                        (ktn-handle-input))))))

(defun ktn--test-setup ()
  (ktn--setup-test-encoding)
  (setq ktn-jisyo-file
        (expand-file-name "ktn-test-jisyo.json"
                          (ktn--test-file-dir)))
  (setq ktn-stats-file (make-temp-file "ktn-stats" nil ".json"))
  (setq ktn-dictionaries nil)
  (setq ktn--stats-data (make-hash-table :test 'equal))
  (ktn-setup)
  (when (fboundp 'ktn-mode)
    (ktn--silence
      (ktn-mode 1))))

(defun ktn-e2e-test (&rest actions)
  (let ((orig-jisyo ktn-jisyo-file)
        (orig-stats ktn-stats-file)
        (orig-dicts ktn-dictionaries)
        (orig-stats-data ktn--stats-data))
    (unwind-protect
        (with-temp-buffer
          (let ((inhibit-read-only t))
            (erase-buffer))
          (ktn--test-setup)
          (dolist (action actions)
            (cond
             ((stringp action)
              (ktn--run-action-sequence action))
             ((functionp action)
              (funcall action))
             (t
              (eval action))))
          (buffer-substring-no-properties (point-min) (point-max)))
      (setq ktn-jisyo-file   orig-jisyo
            ktn-stats-file   orig-stats
            ktn-dictionaries orig-dicts
            ktn--stats-data  orig-stats-data)
      (when (and ktn-stats-file (file-exists-p ktn-stats-file))
        (delete-file ktn-stats-file)))))

(defmacro ktn-define-e2e-tests (&rest tests)
  "複数のE2Eテストケースを一括定義するマクロ。"
  `(progn
     ,@(mapcar (lambda (test)
                 (let ((name (intern (nth 0 test)))
                       (input (nth 1 test))
                       (expected (nth 2 test)))
                   `(ert-deftest ,name ()
                      (let ((result (ktn-e2e-test ,input)))
                        (should (string-match-p ,expected result))))))
               tests)))

;; E2E テストケース
;;   * 名称 "状態（モード）/機能"
(ktn-define-e2e-tests
 ("0/normal/roma-input"             "k a n j i"                            "かんじ")
 ("1/conversion/basic"              "K a n j i SPC C-j"                    "漢字")
 ("1/conversion/cancel"             "K a n j i C-g s a"                    "さ")
 ("1/conversion/katakana"           "K a n j i q"                          "カンジ")
 ("1/conversion/sequence"           "K a n J i C-j K a n j i SPC C-j"      "感じ漢字")
 ("1/conversion/select-key"         "K a n j i SPC 1"                      "漢字")
 ("2/headword/backspace"            "K a k i C-h n j i SPC C-j"            "漢字")
 ("2/headword/completion"           "K a TAB C-j"                          "かんじ")
 ("3/conversion/okuri"              "S i T t a C-j"                        "知った")
 ("3/conversion/backspace"          "K a n j i SPC C-h q"                  "カンジ")
 ("3/conversion/kakutei-newline"    "K a n j i SPC RET s a"                "漢字\nさ")
 ("3/conversion/punctuation-comma"  "K a n j i , C-j"                      "漢字、")
 ("3/conversion/punctuation-period" "K a n j i . C-j"                      "漢字。")
 ("3/conversion/punctuation-quest"  "K a n j i ? C-j"                      "漢字？")
 ("3/conversion/bracket"            "K a n j i [ C-j"                      "「漢字」")
 ("3/conversion/page-navigation"    "K a n j i SPC SPC <next> <prior> C-j" "漢字")
 ("3/conversion/previous-candidate" "K a n j i SPC x C-j"                  "幹事")
 ("3/conversion/continuous1"        "K a n j i SPC s a i C-j"              "漢字さい")
 ("3/conversion/continuous2"        "K a n j i SPC S a i C-j"              "漢字さい")
 ("3/conversion/continuous3"        "K a n j i SPC ?"                      "漢字？")
 )

(ert-deftest dictionary/recursive-registration ()
  (let ((result
         (let ((inhibit-message t)
               (message-log-max nil))
           (cl-letf (((symbol-function 'read-from-minibuffer)
                      (lambda (&rest _)
                        ""))
                     ((symbol-function 'minibufferp)
                      (lambda () nil)))
             (ktn-e2e-test
              "S a i k i t e k i SPC S a i k i SPC S a i SPC K i SPC C-c C-c T e k i SPC C-j C-c C-c")))))
    (should (string-match-p "再帰的" result))))

(ert-deftest headword/hassamu ()
  (with-temp-buffer
    (let* ((inhibit-message t)
           (target "情報処理技術者"))
      (insert (format "%s、。" target))
      (let ((cands (ktn-hassamu)))
        (should (listp cands))
        (should (member target cands)))

      (setq ktn--current-headword
            (ktn-headword--create
             :mode 'headword
             :hassamu-active-p t
             :hassamu-current-matches (list target)
             :hassamu-start-marker (point-marker)))

      (let ((last-command-event ?\t))
        (ktn-tab-key-handler))

      (should (string-match-p target (buffer-string))))))

(ert-deftest mode/toggle-off ()
  (with-temp-buffer
    (let* ((key (kbd "C-x C-j"))
           (old (global-key-binding key)))
      (unwind-protect
          (progn
            (global-set-key key #'ktn-mode)
            (ktn--silence
              (ktn-mode 1))
            (should (bound-and-true-p ktn-mode))
            (let ((cmd (global-key-binding key)))
              (should cmd)
              (call-interactively cmd))
            (should-not (bound-and-true-p ktn-mode)))
        (if old
            (global-set-key key old)
          (global-unset-key key))))))

(provide 'ktn-test)

;;; ktn-test.el ends here
