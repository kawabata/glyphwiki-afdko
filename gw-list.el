;;; gw-list.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Taichi Kawabata

;; Author: Taichi Kawabata <kawabata.taichi@gmail.com>
;; Keywords: font

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate list, sfont and fmndb for HanaMin series.

;;; Code:

(require 'cl-lib)

(defvar gw-list-fmndb
  '(("A"    "A"      " A"    )
    ("B"    "B"      " B"    )
    ("C"    "C"      " C"    )
    ("I"    "I"      " I"    )
    ("ExA1" "Ex A1"  "Ex A1")
    ("ExA2" "Ex A2"  "Ex A2")
    ("ExB"  "Ex B"   "Ex B" )
    ("ExC"  "Ex C"   "Ex C" ))
  "PS Name, Eng Name, JPN Name.")

(defvar gw-list-sfont-range
  '(("HanaMin" "Hanazono Mincho"
     (("HanaMinA" . "[[\\u0000-\\uD7FF][\\uE000-\\uFFFD][\\u10000-u1FFFD]]")
      ("HanaMinB" . "[\\u20000-\\u2A6D6]")
      ("HanaMinC" . "[\\u2A700-\\u2FFFD]")))
    ("HanaMinEx" "Hanazono Mincho Ex"
     (("HanaMinExA1" . "[[\\u0000-\\u007F][\\u4E00-\\u9FFF]]")
      ("HanaMinExA2" . "[[\\u0080-\\u4DFF][\\uA000-\\uD7FF][\\uE000-\\uFFFD][\\u10000-u1FFFD]]")
      ("HanaMinExB" . "[\\u20000-\\u2A6D6]")
      ("HanaMinExC" . "[\\u2A700-\\u2FFFD]")))))

(defvar gw-list
  '(("Basic Latin" "Y" "Y" "Y" "Y" "X" "X" "X" "X")
    ("Latin-1 Supplement" "Y" nil nil nil "X" "X" nil nil)
    ("Latin Extended-A" "Y" nil nil nil "X" "X" nil nil)
    ("Latin Extended-B" "Y" nil nil nil "X" "X" nil nil)
    ("IPA Extensions" "Y" nil nil nil nil "X" nil nil)
    ("Spacing Modifier Letters" "Y" nil nil nil nil "X" nil nil)
    ("Combining Diacritical Marks" "Y" nil nil nil nil "X" nil nil)
    ("Greek and Coptic" "Y" nil nil nil nil "X" nil nil)
    ("Cyrillic" "Y" nil nil nil nil "X" nil nil)
    ("Cyrillic Supplement" "Y" nil nil nil nil "X" nil nil)
    ("Hangul Jamo" "Y" nil nil nil "X" "X" nil nil)
    ("Combining Diacritical Marks Extended" "Y" nil nil nil nil "X" nil nil)
    ("Cyrillic Extended-C" "Y" nil nil nil nil "X" nil nil)
    ("Phonetic Extensions" "Y" nil nil nil nil "X" nil nil)
    ("Phonetic Extensions Supplement" "Y" nil nil nil nil "X" nil nil)
    ("Combining Diacritical Marks Supplement" "Y" nil nil nil nil "X" nil nil)
    ("Latin Extended Additional" "Y" nil nil nil nil "X" nil nil)
    ("Greek Extended" "Y" nil nil nil nil "X" nil nil)
    ("General Punctuation" "Y" "Y" "Y" "Y" "X" "X" "X" "X")
    ("Superscripts and Subscripts" "Y" nil nil nil nil "X" nil nil)
    ("Currency Symbols" "Y" nil nil nil nil "X" nil nil)
    ("Combining Diacritical Marks for Symbols" "Y" nil nil nil nil "X" nil nil)
    ("Letterlike Symbols" "Y" nil nil nil nil "X" nil nil)
    ("Number Forms" "Y" nil nil nil nil "X" nil nil)
    ("Arrows" "Y" nil nil nil nil "X" nil nil)
    ("Mathematical Operators" "Y" nil nil nil nil "X" nil nil)
    ("Miscellaneous Technical" "Y" nil nil nil nil "X" nil nil)
    ("Control Pictures" "Y" nil nil nil nil "X" nil nil)
    ("Optical Character Recognition" "Y" nil nil nil nil "X" nil nil)
    ("Enclosed Alphanumerics" "Y" nil nil nil nil "X" nil nil)
    ("Box Drawing" "Y" nil nil nil nil "X" nil nil)
    ("Block Elements" "Y" nil nil nil nil "X" nil nil)
    ("Geometric Shapes" "Y" nil nil nil nil "X" nil nil)
    ("Miscellaneous Symbols" "Y" nil nil nil nil "X" nil nil)
    ("Dingbats" "Y" nil nil nil nil "X" nil nil)
    ("Miscellaneous Mathematical Symbols-A" "Y" nil nil nil nil "X" nil nil)
    ("Supplemental Arrows-A" "Y" nil nil nil nil "X" nil nil)
    ("Braille Patterns" "Y" nil nil nil nil "X" nil nil)
    ("Supplemental Arrows-B" "Y" nil nil nil nil "X" nil nil)
    ("Miscellaneous Mathematical Symbols-B" "Y" nil nil nil nil "X" nil nil)
    ("Supplemental Mathematical Operators" "Y" nil nil nil nil "X" nil nil)
    ("Miscellaneous Symbols and Arrows" "Y" nil nil nil nil "X" nil nil)
    ("Latin Extended-C" "Y" nil nil nil nil "X" nil nil)
    ("Cyrillic Extended-A" "Y" nil nil nil nil "X" nil nil)
    ("Supplemental Punctuation" "Y" nil nil nil nil "X" nil nil)
    ("CJK Radicals Supplement" "Y" nil nil nil "X" "X" nil nil)
    ("Kangxi Radicals" "Y" nil nil nil "X" "X" nil nil)
    ("Ideographic Description Characters" "Y" "Y" "Y" "X" "Y" "Y" "Y" "Y")
    ("CJK Symbols and Punctuation" "Y" nil nil nil "X" "X" nil nil)
    ("Hiragana" "Y" nil nil nil "X" "X" nil nil)
    ("Katakana" "Y" nil nil nil "X" "X" nil nil)
    ("Bopomofo" "Y" nil nil nil "X" "X" nil nil)
    ("Hangul Compatibility Jamo" "Y" nil nil nil "X" "X" nil nil)
    ("Kanbun" "Y" nil nil nil "X" "X" nil nil)
    ("Bopomofo Extended" "Y" nil nil nil "X" "X" nil nil)
    ("CJK Strokes" "Y" nil nil nil "X" "X" nil nil)
    ("Katakana Phonetic Extensions" "Y" nil nil nil "X" "X" nil nil)
    ("Enclosed CJK Letters and Months" "Y" nil nil nil "X" "X" nil nil)
    ("CJK Compatibility" "Y" nil nil nil "X" "X" nil nil)
    ("CJK Unified Ideographs Extension A" "Y" nil nil nil nil "X" nil nil)
    ("Yijing Hexagram Symbols" "Y" nil nil nil nil "X" nil nil)
    ("CJK Unified Ideographs" "Y" nil nil nil "X" nil nil nil)
    ("Cyrillic Extended-B" "Y" nil nil nil nil "X" nil nil)
    ("Modifier Tone Letters" "Y" nil nil nil nil "X" nil nil)
    ("Latin Extended-D" "Y" nil nil nil nil "X" nil nil)
    ("Hangul Jamo Extended-A" "Y" nil nil nil nil "X" nil nil)
    ("Latin Extended-E" "Y" nil nil nil nil "X" nil nil)
    ("Hangul Syllables" "Y" nil nil nil nil "X" nil nil)
    ("Hangul Jamo Extended-B" "Y" nil nil nil nil "X" nil nil)
    ("Private Use Area" "Y" nil nil nil "X" "X" nil nil)
    ("CJK Compatibility Ideographs" "Y" nil nil nil "X" "X" nil nil)
    ("Alphabetic Presentation Forms" nil nil nil nil nil "X" nil nil)
    ("Variation Selectors" "Y" nil nil nil "X" "X" nil nil)
    ("Vertical Forms" "Y" nil nil nil "X" "X" nil nil)
    ("Combining Half Marks" "Y" nil nil nil "X" "X" nil nil)
    ("CJK Compatibility Forms" "Y" nil nil nil "X" "X" nil nil)
    ("Halfwidth and Fullwidth Forms" "Y" nil nil nil "X" "X" nil nil)
    ("Specials" "Y" nil nil nil "X" "X" nil nil)
    ("Kana Supplement" "Y" nil nil nil nil "X" nil nil)
    ("Kana Extended-A" "Y" nil nil nil nil "X" nil nil)
    ("Tai Xuan Jing Symbols" "Y" nil nil nil nil "X" nil nil)
    ("Counting Rod Numerals" "Y" nil nil nil nil "X" nil nil)
    ("Mathematical Alphanumeric Symbols" "Y" nil nil nil nil "X" nil nil)
    ("Mahjong Tiles" "Y" nil nil nil nil "X" nil nil)
    ("Domino Tiles" "Y" nil nil nil nil "X" nil nil)
    ("Playing Cards" "Y" nil nil nil nil "X" nil nil)
    ("Enclosed Alphanumeric Supplement" "Y" nil nil nil nil "X" nil nil)
    ("Enclosed Ideographic Supplement" "Y" nil nil nil nil "X" nil nil)
    ("Miscellaneous Symbols and Pictographs" "Y" nil nil nil nil "X" nil nil)
    ("Emoticons" "Y" nil nil nil nil "X" nil nil)
    ("Ornamental Dingbats" "Y" nil nil nil nil "X" nil nil)
    ("Transport and Map Symbols" "Y" nil nil nil nil "X" nil nil)
    ("Alchemical Symbols" "Y" nil nil nil nil "X" nil nil)
    ("Geometric Shapes Extended" "Y" nil nil nil nil "X" nil nil)
    ("Supplemental Arrows-C" "Y" nil nil nil nil "X" nil nil)
    ("Supplemental Symbols and Pictographs" "Y" nil nil nil nil "X" nil nil)
    ("Chess Symbols" "Y" nil nil nil nil "X" nil nil)
    ("CJK Unified Ideographs Extension B" nil "Y" nil nil nil nil "X" nil)
    ("CJK Unified Ideographs Extension C" nil nil "Y" nil nil nil nil "X")
    ("CJK Unified Ideographs Extension D" nil nil "Y" nil nil nil nil "X")
    ("CJK Unified Ideographs Extension E" nil nil "Y" nil nil nil nil "X")
    ("CJK Unified Ideographs Extension F" nil nil "Y" nil nil nil nil "X")
    ("CJK Compatibility Ideographs Supplement" nil nil "Y" nil nil nil nil "X")
    ("Tags" nil nil "Y" nil nil nil nil "X")
    ("Variation Selectors Supplement" nil nil "Y" nil nil nil nil "X")))

(defvar gw-list-blocks "Blocks.txt")
(defvar gw-list-dump-newest "dump_newest_only.txt")

(defvar gw-list-normal-suffix1 ;; Variation Selector
  "^\\(-u\\(\\(e01[0-9a-f][0-9a-f]\\)\\|\\(20d[de]\\)\\|\\(309[9a]\\)\\|\\(fe0[0-f]\\)\\)\\)?$")
(defvar gw-list-normal-suffix2 ;; language
  "^\\(-[gtjkvh]\\|jv?\\|us\\)?\\(-vert\\|-halfwidth\\)?$")
(defvar gw-list-extended-suffix1 "^-\\(var\\|itaiji\\)-[0-9][0-9][0-9]$")
(defvar gw-list-extended-suffix2 "^\\(-u[0-9a-f]\\{4,5\\}\\|-cdp-[0-9a-f]\\{4\\}\\)+$")

(defvar gw-list-font (mapcar 'car gw-list-fmndb))
(defvar gw-list-num (length gw-list-fmndb))
(defvar gw-list-wiki "gw-list.wiki")

(defvar gw-list-blocks-list) ;; ブロックの一覧を入れる
(defvar gw-list-blocks-hash) ;; 各文字が属するブロックを入れる
(defvar gw-list-blocks-table) ;; gw名を入れる

;; fmndb

(defvar gw-list-fmndb-file "HanaMin%s.fmndb")

(defvar gw-list-fmndb-format
  "[HanaMin%s]
	f=3,1,0x411,\\82b1\\5712\\660e\\671d%s
	s=3,1,0x411,Regular
	l=3,1,0x411,\\82b1\\5712\\660e\\671d%s Regular
        f=1,1,11,\\89\\d4\\89\\80\\96\\be\\92\\a9%s
        s=1,1,11,Regular
        l=1,1,11,\\89\\d4\\89\\80\\96\\be\\92\\a9%s Regular
	f=Hanazono Mincho %s
	s=Regular
	l=Hanazono Mincho %s Regular
")

;; sfont
(defvar gw-list-sfont-format
  "<?xml version='1.0' encoding='UTF-8' ?>
<!DOCTYPE PosingFont SYSTEM 'file://localhost/System/Library/DTDs/SplicedFont.dtd'>
<PosingFont name='%s' version='1.0'>
	<Name type='0' string='Copyright 2002-2018 GlyphWiki Project. All Rights Reserved.' language='en'/>
	<Name type='1' string='%s' language='en'/>
	<Name type='2' string='Regular' language='en'/>
	<Name type='3' string='%s;UKWN;%s' language='en'/>
	<Name type='4' string='%s' language='en'/>
	<Name type='8' string='GlyphWiki' language='en'/>
	<Components>
%s
	</Components>
</PosingFont>
")

(defvar gw-list-sfont-component-format
  "		<ComponentDef name=\"%s\">
			<UnicodeCharSet uset=\"%s\" />
		</ComponentDef>")

;; code

(defun gw-list-load-blocks ()
  "Load `Block.txt' data file."
  (interactive)
  ;; blocks
  (with-temp-buffer
    (insert-file-contents gw-list-blocks)
    (setq gw-list-blocks-list nil)
    (setq gw-list-blocks-hash (make-hash-table :test 'equal))
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9A-F]+\\)\\.\\.\\([0-9A-F]+\\); \\(.+\\)$" nil t)
      (let ((start (string-to-number (match-string 1) 16))
            (end (string-to-number (match-string 2) 16))
            (block-name (match-string 3)))
        (cl-pushnew block-name gw-list-blocks-list)
        (cl-do ((i start (1+ i))) ((> i end))
          (puthash i block-name gw-list-blocks-hash))))
    (setq gw-list-blocks-list (nreverse gw-list-blocks-list))))

(defun gw-list-load-dump-newest ()
  "Load `dump_newest_only.txt' file."
  (interactive)
  ;; matching
  (setq gw-list-blocks-table (make-hash-table :test 'equal))
  (with-temp-buffer
    (insert-file-contents gw-list-dump-newest)
    (goto-char (point-min))
    ;; ucs
    (while (re-search-forward "^ \\(u\\([0-9a-f]+\\)\\([^ ]*\\)\\) " nil t)
      (let* ((gw-name (match-string 1))
             (base-char (string-to-number (match-string 2) 16))
             (suffix (match-string 3))
             (block-name (gethash base-char gw-list-blocks-hash))
             (list-entry (assoc block-name gw-list))
             (block-list (gethash block-name gw-list-blocks-table))
             (message-log-max nil)
             )
        ;;(message gw-name)
        (if (null block-list)
            (setq block-list (puthash block-name
                                      (list nil nil nil nil nil nil nil nil)
                                      gw-list-blocks-table)))
        (cl-do ((i 0 (1+ i))) ((>= i gw-list-num))
          (if (and (equal (elt list-entry (1+ i)) "Y")
                   (or (string-match gw-list-normal-suffix1 suffix)
                       (string-match gw-list-normal-suffix2 suffix)))
              (cl-pushnew gw-name (elt block-list i))
            (if (and (equal (elt list-entry (1+ i)) "X")
                     (or (string-match gw-list-normal-suffix1 suffix)
                         (string-match gw-list-normal-suffix2 suffix)
                         (string-match gw-list-extended-suffix1 suffix)
                         (string-match gw-list-extended-suffix2 suffix)))
                (cl-pushnew gw-name (elt block-list i)))))))
    ;; cdp
    (goto-char (point-min))
    (let* ((block-name "Private Use Area")
           (list-entry (assoc block-name gw-list))
           (block-list (puthash "Private Use Area"
                                (list nil nil nil nil nil nil nil nil)
                                gw-list-blocks-table)))
      (while (re-search-forward "^ \\(cdp-[0-9a-f]+\\) " nil t)
        (cl-do ((i 0 (1+ i))) ((>= i gw-list-num))
          (if (or (equal (elt list-entry (1+ i)) "Y")
                  (equal (elt list-entry (1+ i)) "X"))
              (cl-pushnew (match-string 1) (elt block-list i))))))
    ))

;; This requires `dash', not working non-interactively.
(defun gw-list-output-wiki ()
  "Output numbers of glyph names to `gw-list.wiki'."
  (interactive)
  (require 'dash)
  (with-temp-file gw-list-wiki
    (insert (format ",Font Name,%s\n"
                    (mapconcat (lambda (x) (concat "HanaMin" x))
                               gw-list-font ",")))
    (let ((total (list 0 0 0 0 0 0 0 0)))
      (dolist (block gw-list-blocks-list)
        (let* ((val (gethash block gw-list-blocks-table))
               (len (mapcar 'length val)))
          (when len
            (setq total (-zip-with '+ total len))
            (insert
             (format ",%s,%s\n"
                     block
                     (mapconcat (lambda (x) (format "%d" x)) len ",")))
            gw-list-blocks-table)))
      (insert (format ",total,%s\n"
                      (mapconcat (lambda (x) (format "%d" x)) total ","))))
    (goto-char (point-min))
    (delete-matching-lines ",0,0,0,0,0,0,0,0")))

(defun gw-list-output-files ()
  "Output HanaMinXX.list files."
  (interactive)
  (cl-do ((i 0 (1+ i))) ((>= i gw-list-num))
    (with-temp-file (concat "HanaMin" (elt gw-list-font i) ".list")
      (dolist (block gw-list-blocks-list)
        (let* ((val (gethash block gw-list-blocks-table))
               (gw-names (elt val i)))
          (dolist (gw-name (sort (copy-sequence gw-names) 'string<))
            (insert gw-name "\n")))))))

(defun gw-list-output-sfonts (version)
  "Output HanaMinXX.sfont files with VERSION."
  (dolist (range gw-list-sfont-range)
    (let ((font-name (elt range 0))
          (font-eng  (elt range 1))
          (ranges    (elt range 2)))
      (with-temp-file (concat font-name ".sfont")
        (insert
         (format gw-list-sfont-format
                 font-name
                 font-eng
                 version
                 font-name
                 font-eng
                 (mapconcat (lambda (x)
                              (format gw-list-sfont-component-format (car x) (cdr x)))
                            ranges "\n") t t))))))

(defun gw-list-output-fmndbs ()
  "Output HanaMinXX.fmndb files."
  (dolist (entry gw-list-fmndb)
    (let* ((ps    (elt entry 0))
           (eng   (elt entry 1))
           (jpn   (elt entry 2))
           (sjis  (mapconcat (lambda (x) (format "\\%02x" x))
                             (string-to-list (encode-coding-string jpn 'shift_jis)) ""))
           (ucs   (mapconcat (lambda (x) (format "\\%04x" x))
                             (string-to-list jpn) "")))
      (with-temp-file (format gw-list-fmndb-file ps)
        (insert
         (format gw-list-fmndb-format
                 ps
                 ucs ucs
                 sjis sjis
                 eng eng))))))

(defun gw-list (argv)
  "Non interactive output to files.  ARGV is version."
  (gw-list-load-blocks)
  (gw-list-load-dump-newest)
  (gw-list-output-files)
  (gw-list-output-sfonts (car argv))
  (gw-list-output-fmndbs))

(when noninteractive
  (message "invoking from script")
  (gw-list argv))

(provide 'gw-list)

;;; gw-list.el ends here
