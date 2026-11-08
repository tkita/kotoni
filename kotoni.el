;;; kotoni.el --- Yet another SKK-like implementation for GNU Emacs 30 -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5") (json))

;;; Commentary:
;; Minimalist SKK clone leveraging lexical binding and cl-lib.
;; Named Kotoni (琴似), Sapporo City, Japan.
;;
;; Example configuration:
;;   (add-to-list 'load-path "/path/to/kotoni")
;;   (setq ktn-user-roma-table '(("z," . "‥") ("z." . "…")))
;;   (setq ktn-jisyo-file "/usr/share/skk/SKK-JISYO.trie.json")
;;   (require 'kotoni)
;;   (global-set-key (kbd "C-x C-j") #'ktn-mode)

;; アーキテクチャ概要:
;;
;; [ kotoni-data.el ]: 第3階層: データ・辞書ストレージ層
;;   | - system-jisyo (skk-large-jisyo など) 検索
;;   | - user-stats (JSON統計データ保存・読み込み)
;;   | - ローマ字-かな変換テーブル定義
;; ^
;; ^ 辞書検索・補完クエリ・統計セーブ
;; ^
;; [ kotoni-fsm.el ]: 第2階層: FSM / 状態遷移ロジック層
;;   | - 状態管理 (normal | headword | converting)
;;   | - ローマ字テーブル・仮名変換・バッファ演算処理
;; ^
;; ^ FSMイベント・状態コマンドをディスパッチ
;; ^
;; [ kotoni-ui.el ]: 第1階層: UI & インタフェース層
;;   | - define-minor-mode / ktn-mode-map
;;   | - ktn-insert (インプットルーター)
;;   | - オーバーレイ描画・候補一覧ウィンドウ・メッセージ
;; ^
;; ^
;; ^
;; [ Keyboard / Emacs Buffer ]

;;; Code:

(require 'kotoni-data)
(require 'kotoni-fsm)
(require 'kotoni-ui)
(require 'kotoni-hassamu)


;;; Keymap & Minor-mode Definition

(defvar ktn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'ktn-handle-input)

    (dolist (key '("C-g"    "C-h"   "C-j" "RET" "C-m" "SPC" "q" "M-SPC"
                   "TAB"    "<tab>" "<backtab>" "S-TAB"
                   "<next>" "<prior>"))
      (define-key map (kbd key) #'ktn-handle-input))
    map)
  "Keymap for `ktn-mode`.")

;;;###autoload
(define-minor-mode ktn-mode
  "Toggle Ktn mode."
  :lighter ktn-lighter
  :global nil
  :keymap ktn-mode-map
  (if ktn-mode
      (progn (ktn-setup)
             (ktn--set-cursor-red))
    (ktn--state-reset)
    (ktn--unset-cursor-red)
    (ktn--cleanup-headword))
  (setq ktn-lighter (if ktn-mode
                        ktn-indicator
                      ""))
  (force-mode-line-update))

;;;###autoload
(defalias 'kotoni-mode 'ktn-mode)

(provide 'kotoni)
;;; kotoni.el ends here
