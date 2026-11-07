;;; kotoni-data.el --- Data & Dictionary Storage Layer for Kotoni -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2026
;; Version: 0.1
;; Author: Tsuyoshi Kitamoto <tsuyohi.kitamoto@gmail.com>
;; License: GPL-3.0-or-later

;;; Commentary:
;; 第3階層：辞書ファイル・個人統計・ローマ字テーブルの保持および検索を担当するモジュール。

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup kotoni nil
  "Minimal SKK clone focusing on modeline and basic insertion."
  :group 'input-method)

(defcustom ktn-jisyo-file (expand-file-name "~/.emacs.d/jisyo.json")
  "Path to JSON Dictionary file."
  :type 'file
  :group 'kotoni)

(defcustom ktn-stats-file (expand-file-name "~/.jisyo.json")
  "Path to JSON file storing user conversion statistics."
  :type 'file
  :group 'kotoni)

;;; 個人辞書（統計情報）の内部状態
(defvar ktn--stats-loaded-p nil)
(defvar ktn--stats-data (make-hash-table :test 'equal))

;;; 辞書構造体
(cl-defstruct ktn-dictionary
  path data headwords)

(defvar ktn-dictionaries nil)

;;; ローマ字テーブル定義
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
    ("xtsu" . "っ")     ("xtu"  . "っ")     ("xwa"  . "ゎ")
    ("xwe"  . "ゑ")     ("xwi" . "ゐ")
    ("xya"  . "ゃ")                         ("xyu" . "ゅ")                         ("xyo"  . "ょ")
    ("ya"   . "や")                         ("yu"  . "ゆ")      ("ye"  . "いぇ")   ("yo"   . "よ") 
    ("za"   . "ざ")     ("zi"  . "じ")      ("zu"  . "ず")      ("ze"  . "ぜ")     ("zo"   . "ぞ")
    ("zya"  . "じゃ")   ("zyi" . "じぃ")    ("zyu" . "じゅ")    ("zye" . "じぇ")   ("zyo"  . "じょ")
    ("zh" . "←") ("zj" . "↓") ("zk"  . "↑") ("zl" . "→")
    ("zH" . "⇐") ("zJ" . "⇓") ("zK" . "⇑")  ("zL" . "⇒")
    ("z~"  . "〜")
    ("!" . "！") ("," . "、") ("-" . "ー") ("." . "。")
    (":" . "：") (";" . "；") ("?" . "？") ("[" . "「") ("]" . "」")))

(defvar ktn-roma-table nil)

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

;;; 辞書・統計関連処理
(defun ktn--load-stats ()
  "Load ktn stats from JSON file safely."
  (when (or (not ktn--stats-loaded-p)
            (null ktn--stats-data)
            (not (hash-table-p ktn--stats-data)))
    (let ((jisyo-path (or ktn-stats-file (expand-file-name "~/.jisyo.json"))))
      (if (and jisyo-path (file-exists-p jisyo-path))
          (condition-case nil
              (progn
                (with-temp-buffer
                  (insert-file-contents jisyo-path)
                  (if (= (buffer-size) 0)
                      (setq ktn--stats-data (make-hash-table :test 'equal))
                    (setq ktn--stats-data (json-parse-buffer :object-type 'hash-table))))
                (unless (hash-table-p ktn--stats-data)
                  (setq ktn--stats-data (make-hash-table :test 'equal)))
                (setq ktn--stats-loaded-p t))
            (error
             (setq ktn--stats-data (make-hash-table :test 'equal))
             (setq ktn--stats-loaded-p t)))
        (setq ktn--stats-data (make-hash-table :test 'equal))
        (setq ktn--stats-loaded-p t))))
  t)

(defun ktn-save-stats ()
  "Save `ktn--stats-data` to JSON file."
  (interactive)
  (when ktn-stats-file
    (condition-case err
        (let ((json-encoding-pretty-print t))
          (with-temp-file ktn-stats-file
            (insert (json-encode ktn--stats-data))))
      (error (message "Failed to save ktn stats file: %s" err)))))

(defun ktn--load-dictionary-data (dict)
  (let ((path (ktn-dictionary-path dict)))
    (when (and path (file-exists-p path))
      (with-temp-buffer
        (insert-file-contents (ktn-dictionary-path dict))
        (setf (ktn-dictionary-data dict)
              (json-parse-buffer :object-type 'hash-table))
        (setf (ktn-dictionary-headwords dict)
              (ktn--extract-headwords (ktn-dictionary-data dict)))))))

(defun ktn--extract-headwords (data)
  (let (hlist)
    (when (hash-table-p data)
      (dolist (section-key '("okuri_nasi" "okuri_ari"))
        (let ((section (gethash section-key data)))
          (when (hash-table-p section)
            (maphash (lambda (midasi _)
                       (push midasi hlist))
                     section)))))
    (sort hlist #'string<)))

(defun ktn--parse-candidate-items (items)
  (when (hash-table-p items)
    (setq items (or (gethash "word" items)
                    (gethash "candidates" items)
                    (gethash "cands" items)
                    items)))
  (let (result)
    (dolist (item (cond ((vectorp items) (append items nil))
                        ((listp items) items)
                        (t (list items))))
      (cond
       ((hash-table-p item)
        (let ((word (gethash "word" item))
              (annot (gethash "annotation" item)))
          (when word (push (cons word annot) result))))
       ((stringp item)
        (let* ((clean-item (replace-regexp-in-string "\\[[^]]*\\]" "" item))
               (cand (if (string-match "^\\([^;]+\\);\\(.*\\)$" clean-item)
                         (cons (match-string 1 clean-item) (match-string 2 clean-item))
                       (cons clean-item nil))))
          (push cand result)))))
    (nreverse result)))

(defun ktn-dictionary-search (dict midasi &optional is-okuri)
  (let ((data (ktn-dictionary-data dict)))
    (when (hash-table-p data)
      (let* ((section-key (if is-okuri "okuri_ari" "okuri_nasi"))
             (section   (gethash section-key data))
             (raw-cands (or (and (hash-table-p section)
                                 (gethash midasi section))
                            (gethash midasi data))))
        (when raw-cands
          (ktn--parse-candidate-items raw-cands))))))

(defun ktn-search-jisyo-combined (midasi &optional is-okuri)
  (let ((seen (make-hash-table :test 'equal))
        result)
    (let ((cands-hash (gethash midasi ktn--stats-data)))
      (when (and cands-hash (hash-table-p cands-hash))
        (let (stat-list)
          (maphash (lambda (word count) (push (cons word count) stat-list)) cands-hash)
          (setq stat-list (sort stat-list (lambda (a b) (> (cdr a) (cdr b)))))
          (dolist (item stat-list)
            (let ((word (car item)))
              (unless (gethash word seen)
                (puthash word t seen)
                (push (list word nil 'user) result)))))))
    (dolist (dict ktn-dictionaries)
      (dolist (c (ktn-dictionary-search dict midasi is-okuri))
        (let ((word (car c)) (annot (cdr c)))
          (unless (gethash word seen)
            (puthash word t seen)
            (push (list word annot 'system) result)))))
    (nreverse result)))

(defun ktn-search-jisyo-with-okuri-combined (midasi okuri-first-roma okuri-kana)
  (let* ((search-key (concat midasi okuri-first-roma))
         (seen (make-hash-table :test 'equal))
         stat-candidates system-candidates)
    (let ((cands-hash (gethash search-key ktn--stats-data)))
      (when (and cands-hash (hash-table-p cands-hash))
        (let (lst)
          (maphash (lambda (word count) (push (cons word count) lst)) cands-hash)
          (setq lst (sort lst (lambda (a b) (> (cdr a) (cdr b)))))
          (dolist (item lst)
            (let ((word (car item)))
              (unless (gethash word seen)
                (puthash word t seen)
                (push (list word nil 'user) stat-candidates)))))))
    (let ((raw-cands (ktn-search-jisyo-combined search-key t)))
      (dolist (cand-cell raw-cands)
        (let* ((word-orig (nth 0 cand-cell))
               (annot (nth 1 cand-cell))
               (word (concat word-orig okuri-kana))
               (source (nth 2 cand-cell)))
          (unless (gethash word seen)
            (puthash word t seen)
            (push (list word annot source) system-candidates)))))
    (append (nreverse stat-candidates) (nreverse system-candidates))))

(defvar-local ktn--completion-cache (make-hash-table :test 'equal))

(defun ktn--get-completion-cache (prefix)
  (gethash prefix ktn--completion-cache))

(defun ktn--set-completion-cache (prefix value)
  (puthash prefix value ktn--completion-cache))

(defun ktn--complete-search-headword (prefix)
  (let ((cached (ktn--get-completion-cache prefix)))
    (if cached
        (copy-sequence cached)
      (let ((result
             (let ((seen (make-hash-table :test 'equal))
                   stats-results system-results)
               (maphash
                (lambda (midasi _)
                  (when (and (string-prefix-p prefix midasi)
                             (not (gethash midasi seen)))
                    (puthash midasi t seen)
                    (push midasi stats-results)))
                ktn--stats-data)
               (setq stats-results (sort stats-results #'string<))
               (dolist (dict ktn-dictionaries)
                 (dolist (midasi (or (ktn-dictionary-headwords dict) nil))
                   (when (and (string-prefix-p prefix midasi)
                              (not (gethash midasi seen)))
                     (puthash midasi t seen)
                     (push midasi system-results))))
               (setq system-results (nreverse system-results))
               (append stats-results system-results))))
        (ktn--set-completion-cache prefix result)
        result))))

(defun ktn--register-word (midasi word &optional annotation)
  (let* ((clean-word (if annotation (concat word ";" annotation) word))
         (cands-hash (gethash midasi ktn--stats-data)))
    (unless cands-hash
      (setq cands-hash (make-hash-table :test 'equal))
      (puthash midasi cands-hash ktn--stats-data))
    (puthash clean-word (1+ (gethash clean-word cands-hash 0)) cands-hash)
    (ktn-save-stats)
    (message "辞書に登録しました: [%s] -> %s" midasi clean-word)))

(provide 'kotoni-data)
;;; kotoni-data.el ends here
