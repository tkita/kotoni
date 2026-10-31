;;; kotoni-common.el --- Shared definitions for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

(require 'cl-lib)

(defgroup ktn nil
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
  :type 'string)

(defcustom ktn-jisyo-file (expand-file-name "~/.emacs.d/skk-large-jisyo")
  "Path to SKK Dictionary file (Text or JSON)."
  :type 'file
  :group 'kotoni)

(defcustom ktn-show-candidates-nth-henkan 3
  "何回目のスペース打鍵で候補一覧バッファを表示するか"
  :type 'integer
  :group 'kotoni)

(defvar-local ktn-henkan-count 0)

;;; 個人辞書（統計情報）の設定とデータ構造
(defvar ktn--stats-loaded-p nil
  "Non-nil when personal statistics have been loaded into memory.")

(defcustom ktn-stats-file (expand-file-name "~/.jisyo.json")
  "Path to JSON file storing user conversion statistics."
  :type 'file)

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

(defvar-local ktn-state nil
  "Current input state of Ktn. Either nil, 'headword (▽), or 'converting (▼).")

(defvar-local ktn-candidates nil
  "List of (CANDIDATE . ANNOTATION) candidate pairs for current conversion.")

(defvar-local ktn-candidate-index 0
  "Current index in `ktn-candidates`.")

(defvar-local ktn-roma-buffer ""
  "Buffer storing unconsumed Romaji sequence.")

(defvar-local ktn-headword-buffer ""
  "Buffer storing accumulated headword in ▽/▼ mode.")

(defvar-local ktn-headword-start-marker nil
  "Marker pointing to the start of headword input in the buffer.")

(defvar-local ktn-headword-overlay nil
  "Overlay used for ▽/▼ headword display.")

(defvar-local ktn-okuri-buffer ""
  "Buffer storing the okurigana (送り仮名) during okuri-ari conversion.")

(defvar-local ktn-okuri-roma-buffer ""
  "Buffer storing unconsumed Romaji sequence for okurigana.")

(defvar-local ktn--completion-cands nil
  "List of headword completion candidates matching current prefix.")

(defvar-local ktn--completion-index 0
  "Current index in `ktn--completion-cands`.")

(defvar-local ktn--completion-base nil
  "Base headword string before completion started.")

(defvar-local ktn--completion-tab-count 0
  "Count of TAB presses in the current completion session for the buffer.")

(defvar-local ktn-okuri-roma-first ""
  "Buffer storing the first romaji character of okurigana.")

(defvar ktn-lighter ""
  "Dynamic modeline lighter for `ktn-mode`.")

(defface ktn-completion-preview-face
  '((t (:foreground "#777777")))
  "Face for dynamic completion inline preview (pale/dim color)."
  :group 'kotoni)

(defface ktn-index-face
  '((t (:background "white" :foreground "black" :underline t)))
  "Face for candidate selection index keys."
  :group 'kotoni)

(provide 'kotoni-common)
;;; kotoni-common.el ends here
