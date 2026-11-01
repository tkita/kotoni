;;; kotoni-common.el --- Shared definitions for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1") (cl-lib "0.5") (json))

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defgroup kotoni nil
  "Minimal SKK clone focusing on modeline and basic insertion."
  :group 'input-method)

(defface ktn-headword-face
  '((t (:inherit underline)))
  "Face for ▽/▼ headword overlay display."
  :group 'kotoni)

(defface ktn-selected-face
  '((t (:inherit highlight)))
  "Face for the currently selected candidate in the echo area list."
  :group 'kotoni)

(defcustom ktn-indicator " [▼]"
  "Modeline indicator when `ktn-mode` is active."
  :type 'string
  :group 'kotoni)

(defcustom ktn-jisyo-file (expand-file-name "~/.emacs.d/skk-large-jisyo")
  "Path to SKK Dictionary file (Text or JSON)."
  :type 'file
  :group 'kotoni)

(defcustom ktn-show-candidates-nth-henkan 3
  "何回目のスペース打鍵で候補一覧バッファを表示するか"
  :type 'integer
  :group 'kotoni)

;;; 個人辞書（統計情報）の設定とデータ構造
(defvar ktn--stats-loaded-p nil
  "Non-nil when personal statistics have been loaded into memory.")

(defcustom ktn-stats-file (expand-file-name "~/.jisyo.json")
  "Path to JSON file storing user conversion statistics."
  :type 'file
  :group 'kotoni)

(defvar ktn--stats-data (make-hash-table :test 'equal)
  "Emacs internal hash table for conversion statistics.
Key: midasi (string)
Value: hash-table (Key: candidate (string), Value: count (integer))")

;;; Dictionary Instance Architecture

(cl-defstruct ktn-dictionary
  path buffer data)

(defvar ktn-dictionaries nil
  "List of `ktn-dictionary` instances used for search orchestration.")

(defvar ktn--system-jisyo-instance
  (make-ktn-dictionary :path nil :buffer nil :data nil)
  "System dictionary instance.")

(defvar ktn-select-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7)
  "Direct selection keys assigned to candidates in converting mode.")

;; 基本的なローマ字かな変換テーブル
(defvar ktn-default-roma-table
  '(("a"    . "あ")     ("i"   . "い")      ("u"    . "う")     ("e"   . "え")     ("o"    . "お")
    ("ba"   . "ば")     ("bi"  . "び")      ("bu"   . "ぶ")     ("be"  . "べ")     ("bo"   . "ぼ") 
    ("bya"  . "びゃ")   ("byi" . "びぃ")    ("byu"  . "びゅ")   ("bye" . "びぇ")   ("byo"  . "びょ")   
    ("cha"  . "ちゃ")   ("chi" . "ち")      ("chu"  . "ちゅ")   ("che" . "ちぇ")   ("cho"  . "ちょ") 
    ("cya"  . "ちゃ")   ("cyi" . "ちぃ")    ("cyu"  . "ちゅ")   ("cye" . "ちぇ")   ("cyo"  . "ちょ") 
    ("da"   . "だ")     ("di"  . "ぢ")      ("du"   . "づ")     ("de"  . "で")     ("do"   . "ど") 
    ("dha"  . "でゃ")   ("dhi" . "でぃ")    ("dhu"  . "でゅ")   ("dhe" . "でぇ")    ("dho" . "でょ")   
                                            ("dwu"  . "どぅ")  
    ("dya"  . "ぢゃ")   ("dyi" . "ぢぃ")    ("dyu"  . "ぢゅ")   ("dye" . "ぢぇ")   ("dyo"  . "ぢょ") 
    ("fa"   . "ふぁ")   ("fi"  . "ふぃ")    ("fu"   . "ふ")     ("fe"  . "ふぇ")   ("fo"   . "ふぉ") 
    ("fya"  . "ふゃ")   ("fyi" . "ふぃ")    ("fyu"  . "ふゅ")   ("fye" . "ふぇ")   ("fyo"  . "ふょ")
    ("ga"   . "が")     ("gi"  . "ぎ")      ("gu"   . "ぐ")     ("ge"  . "げ")     ("go"   . "ご")
    ("gwa"  . "ぐゎ")   ("gwi" . "ぐうぃ")  ("gwu"  . "ぐうぅ") ("gwe" . "ぐうぇ") ("gwo"  . "ぐうぉ")
    ("gya"  . "ぎゃ")   ("gyi" . "ぎぃ")    ("gyu"  . "ぎゅ")   ("gye" . "ぎぇ")   ("gyo"  . "ぎょ") 
    ("ha"   . "は")     ("hi"  . "ひ")      ("hu"   . "ふ")     ("he"  . "へ")     ("ho"   . "ほ")
    ("hya"  . "ひゃ")   ("hyi" . "ひぃ")    ("hyu"  . "ひゅ")   ("hye" . "ひぇ")   ("hyo"  . "ひょ")
    ("ja"   . "じゃ")   ("ji"  . "じ")      ("ju"   . "じゅ")   ("je"  . "じぇ")   ("jo"   . "じょ")
    ("jya"  . "じゃ")   ("jyi" . "じぃ")    ("jyu"  . "じゅ")   ("jye" . "じぇ")   ("jyo"  . "じょ")
    ("ka"   . "か")     ("ki"  . "き")      ("ku"   . "く")     ("ke"  . "け")     ("ko"   . "こ")
    ("kwa"  . "くゎ")   ("kwi" . "くうぃ")  ("kwu"  . "くうぅ") ("kwe" . "くうぇ") ("kwo"  . "くうぉ")
    ("kya"  . "きゃ")   ("kyi" . "きぃ")    ("kyu"  . "きゅ")   ("kye" . "きぇ")   ("kyo"  . "きょ")
    ("la"   . "ぁ")     ("li"  . "ぃ")      ("lu"   . "ぅ")     ("le"  . "ぇ")     ("lo"   . "ぉ")
    ("lka"  . "ヵ")                                             ("lke" . "ヶ")
                                            ("ltsu" . "っ")
                                            ("ltu"  . "っ")
    ("lwa"  . "ゎ")
    ("lya"  . "ゃ")                         ("lyu"  . "ゅ")                        ("lyo"  . "ょ")
    ("ma"   . "ま")     ("mi"  . "み")      ("mu"   . "む")     ("me"  . "め")     ("mo"   . "も")
    ("mya"  . "みゃ")   ("myi" . "みぃ")    ("myu"  . "みゅ")   ("mye" . "みぇ")   ("myo"  . "みょ")
    ("n'"   . "ん")
    ("na"   . "な")     ("ni"  . "に")      ("nu"   . "ぬ")     ("ne"  . "ね")     ("no"   . "の")
    ("nn"   . "ん")
    ("nya"  . "にゃ")   ("nyi" . "にぃ")    ("nyu"  . "にゅ")   ("nye" . "にぇ")   ("nyo"  . "にょ")
    ("pa"   . "ぱ")     ("pi"  . "ぴ")      ("pu"   . "ぷ")     ("pe"  . "ぺ")     ("po"   . "ぽ")
    ("pya"  . "ぴゃ")   ("pyi" . "ぴぃ")    ("pyu"  . "ぴゅ")   ("pye" . "ぴぇ")   ("pyo"  . "ぴょ")
    ("ra"   . "ら")     ("ri"  . "り")      ("ru"   . "る")     ("re"  . "れ")     ("ro"   . "ろ")
    ("rya"  . "りゃ")   ("ryi" . "りぃ")    ("ryu"  . "りゅ")   ("rye" . "りぇ")   ("ryo"  . "りょ")
    ("sa"   . "さ")     ("si"  . "し")      ("su"   . "す")     ("se"  . "せ")     ("so"   . "そ") 
    ("sha"  . "しゃ")   ("shi" . "し")      ("shu"  . "しゅ")   ("she" . "しぇ")   ("sho"  . "しょ")   
    ("sya"  . "しゃ")   ("syi" . "しぃ")    ("syu"  . "しゅ")   ("sye" . "しぇ")   ("syo"  . "しょ") 
    ("ta"   . "た")     ("ti"  . "ち")      ("tu"   . "つ")     ("te"  . "て")     ("to"   . "と")
    ("tha"  . "てぁ")   ("thi" . "てぃ")    ("thu"  . "てゅ")   ("the" . "てぇ")   ("tho"  . "てょ")   
    
    ("tsa"  . "つぁ")   ("tsi" . "つぃ")    ("tsu"  . "つ")     ("tse" . "つぇ")   ("tso"  . "つぉ")
                                            ("twu"  . "とぅ")
    ("tya"  . "ちゃ")   ("tyi" . "ちぃ")    ("tyu"  . "ちゅ")   ("tye" . "ちぇ")   ("tyo"  . "ちょ")
    ("va"   . "う゛ぁ") ("vi"  . "う゛ぃ")  ("vu"   . "う゛")   ("ve"  . "う゛ぇ") ("vo"   . "う゛ぉ")
    ("vya"  . "う゛ゃ") ("vyi" . "う゛ぃ")  ("vyu"  . "う゛ゅ") ("vye" . "う゛ぇ") ("vyo"  . "う゛ょ")
    ("wa"   . "わ")     ("wi"  . "うぃ")    ("wu"   . "う")     ("we"  . "うぇ")   ("wo"   . "を")
    ("wha"  . "ふぁ")   ("whi" . "ふぃ")    ("wyu"  . "ふゅ")   ("whe" . "ふぇ")   ("who"  . "ふぉ")
    ("xa"   . "ぁ")     ("xi"  . "ぃ")      ("xu"   . "ぅ")     ("xe"  . "ぇ")     ("xo"   . "ぉ")
    ("xka"  . "ヵ")                                             ("xke" . "ヶ")
    ("xtsu" . "っ")
    ("xtu"  . "っ")
    ("xwa"  . "ゎ")
    ("xwe"  . "ゑ")     ("xwi" . "ゐ")
    ("xya"  . "ゃ")                         ("xyu" . "ゅ")                         ("xyo"  . "ょ")
    ("ya"   . "や")                         ("yu"  . "ゆ")      ("ye"  . "いぇ")   ("yo"   . "よ") 
    ("za"   . "ざ")     ("zi"  . "じ")      ("zu"  . "ず")      ("ze"  . "ぜ")     ("zo"   . "ぞ")
    ("zya"  . "じゃ")   ("zyi" . "じぃ")    ("zyu" . "じゅ")    ("zye" . "じぇ")   ("zyo"  . "じょ")

    ("zh" . "←") ("zj" . "↓") ("zk"  . "↑") ("zl" . "→")
    ("zH" . "⇐") ("zJ" . "⇓") ("zK" . "⇑")  ("zL" . "⇒")
    ("z~"  . "〜")

    ("!" . "！") ("," . "、") ("-" . "ー") ("." . "。")
    (":" . "：") (";" . "；") ("?" . "？") ("[" . "「") ("]" . "」"))
  "Default Romaji-to-Kana conversion table.")

(defvar ktn-roma-table nil
  "Merged Romaji-to-Kana conversion table used for input processing.")

(defun ktn-setup-roma-table ()
  "Merge `ktn-user-roma-table` and `ktn-default-roma-table` into `ktn-roma-table`."
  (let ((table nil))
    (dolist (elt ktn-user-roma-table)
      (push elt table))
    (cl-loop for elt in ktn-default-roma-table
             unless (assoc (car elt) ktn-user-roma-table)
             do (push elt table))
    (setq ktn-roma-table (nreverse table))))

(defcustom ktn-user-roma-table nil
  "User-defined Romaji-to-Kana conversion table."
  :type '(repeat (cons string string))
  :set (lambda (symbol value)
         (set-default symbol value)
         (ktn-setup-roma-table))
  :group 'kotoni)

(defvar-local ktn--saved-cursor-color nil)

(defconst ktn-candidate-buffer-name " *ktn-candidates*" ; 隠しバッファ: バッファ名の先頭にスペース
  "Name of the temporary buffer used for displaying conversion candidates.")

(defvar ktn-lighter ""
  "Dynamic modeline lighter for `ktn-mode`.")

(defface ktn-completion-preview-face
  '((t (:foreground "#777777")))
  "Face for dynamic completion inline preview (pale/dim color)."
  :group 'kotoni)

(defface ktn-completion-base-face
  '((t (:inherit default)))
  "Face for the typed prefix part in candidate lists.")

(defface ktn-completion-rest-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for the uncompleted/remaining part in candidate lists.")

(defface ktn-index-face
  '((t (:background "white" :foreground "black" :underline t)))
  "Face for candidate selection index keys."
  :group 'kotoni)

(defvar-local ktn--state nil
  "Internal state plist for Kotoni.
Keys:
  :mode                 'headword | 'converting | 'normal
  :headword-buffer      string
  :roma-buffer          string
  :okuri-buffer         string
  :okuri-roma-buffer    string
  :okuri-roma-first     string
  :headword-start       marker
  :candidates           list
  :candidate-index      int
  :henkan-count         int
  :completion-cands     list
  :completion-index     int
  :completion-base      string
  :completion-tab-count int
  :headword-overlay     overlay")

(defun ktn--state-get (key &optional default)
  "Get value for KEY from `ktn--state`.
If the value is nil, return DEFAULT, or for string-like keys, return \"\" automatically."
  (let ((val (plist-get ktn--state key)))
    (cond
     (val val)
     (default default)
     ;; 文字列を期待する内部バッファ系のキーは、nil の代わりに "" を返す
     ((memq key '(:headword-buffer
                  :roma-buffer
                  :okuri-buffer
                  :okuri-roma-buffer
                  :okuri-roma-first
                  :completion-base))
      "")
     (t nil))))

(defun ktn--state-set (key value)
  "Set VALUE for KEY in `ktn--state` plist."
  (setq ktn--state (plist-put ktn--state key value))
  value)

(defun ktn--mode-p (mode)
  "Return non-nil if current IME mode matches MODE."
  (eq (ktn--state-get :mode) mode))

(defun ktn--state-reset ()
  "Reset `ktn--state` plist in the current buffer to its initial empty state."
  (setq ktn--state
        (list :mode nil
              :headword-buffer ""
              :roma-buffer ""
              :okuri-buffer ""
              :okuri-roma-buffer ""
              :okuri-roma-first ""
              :headword-start-marker nil
              :candidates nil
              :candidate-index 0
              :henkan-count 0
              :completion-cands nil
              :completion-index 0
              :completion-base nil
              :completion-tab-count 0
              :headword-overlay nil)))

(provide 'kotoni-common)
;;; kotoni-common.el ends here
