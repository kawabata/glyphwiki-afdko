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

;; Generate list for specified font

;;; Code:

(require 'cl-lib)

(defvar gw-list-blocks "Blocks.txt")
(defvar gw-list-dump-newest "dump_newest_only.txt")

(defvar gw-list-normal-suffix1 ;; Variation Selector
  "^\\(-u\\(\\(e01[0-9a-f][0-9a-f]\\)\\|\\(20d[de]\\)\\|\\(fe0[0-f]\\)\\)\\)?$")
(defvar gw-list-normal-suffix2 ;; language
  "^\\(-[ghmktuv]\\|j[av]?\\|kp\\|us\\)?\\(-vert\\|-halfwidth\\)?$")
(defvar gw-list-extended-suffix1 "^-\\(var\\|itaiji\\)-[0-9][0-9][0-9]$")
(defvar gw-list-extended-suffix2 "^\\(-u[0-9a-f]\\{4,5\\}\\|-cdp-[0-9a-f]\\{4\\}\\)+$")

(defvar gw-list-wiki "gw-list.wiki")
(defvar gw-list-font '("A" "AX" "UX" "B" "BX" "C" "CX" "I"))
(defvar gw-list-num (length gw-list-font))
(defvar gw-list
  '(("Basic Latin" "Y" "X" "X" "Y" "X" "Y" "X" "Y")
    ("Latin-1 Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Latin Extended-A" "Y" "X" nil nil nil nil nil nil)
    ("Latin Extended-B" "Y" "X" nil nil nil nil nil nil)
    ("IPA Extensions" "Y" "X" nil nil nil nil nil nil)
    ("Spacing Modifier Letters" "Y" "X" nil nil nil nil nil nil)
    ("Combining Diacritical Marks" "Y" "X" nil nil nil nil nil nil)
    ("Greek and Coptic" "Y" "X" nil nil nil nil nil nil)
    ("Cyrillic" "Y" "X" nil nil nil nil nil nil)
    ("Cyrillic Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Hangul Jamo" "Y" "X" nil nil nil nil nil nil)
    ("Combining Diacritical Marks Extended" "Y" "X" nil nil nil nil nil nil)
    ("Cyrillic Extended-C" "Y" "X" nil nil nil nil nil nil)
    ("Phonetic Extensions" "Y" "X" nil nil nil nil nil nil)
    ("Phonetic Extensions Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Combining Diacritical Marks Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Latin Extended Additional" "Y" "X" nil nil nil nil nil nil)
    ("Greek Extended" "Y" "X" nil nil nil nil nil nil)
    ("General Punctuation" "Y" "X" "X" "Y" "X" "Y" "X" "Y")
    ("Superscripts and Subscripts" "Y" "X" nil nil nil nil nil nil)
    ("Currency Symbols" "Y" "X" nil nil nil nil nil nil)
    ("Combining Diacritical Marks for Symbols" "Y" "X" nil nil nil nil nil nil)
    ("Letterlike Symbols" "Y" "X" nil nil nil nil nil nil)
    ("Number Forms" "Y" "X" nil nil nil nil nil nil)
    ("Arrows" "Y" "X" nil nil nil nil nil nil)
    ("Mathematical Operators" "Y" "X" nil nil nil nil nil nil)
    ("Miscellaneous Technical" "Y" "X" nil nil nil nil nil nil)
    ("Control Pictures" "Y" "X" nil nil nil nil nil nil)
    ("Optical Character Recognition" "Y" "X" nil nil nil nil nil nil)
    ("Enclosed Alphanumerics" "Y" "X" nil nil nil nil nil nil)
    ("Box Drawing" "Y" "X" nil nil nil nil nil nil)
    ("Block Elements" "Y" "X" nil nil nil nil nil nil)
    ("Geometric Shapes" "Y" "X" nil nil nil nil nil nil)
    ("Miscellaneous Symbols" "Y" "X" nil nil nil nil nil nil)
    ("Dingbats" "Y" "X" nil nil nil nil nil nil)
    ("Miscellaneous Mathematical Symbols-A" "Y" "X" nil nil nil nil nil nil)
    ("Supplemental Arrows-A" "Y" "X" nil nil nil nil nil nil)
    ("Braille Patterns" "Y" "X" nil nil nil nil nil nil)
    ("Supplemental Arrows-B" "Y" "X" nil nil nil nil nil nil)
    ("Miscellaneous Mathematical Symbols-B" "Y" "X" nil nil nil nil nil nil)
    ("Supplemental Mathematical Operators" "Y" "X" nil nil nil nil nil nil)
    ("Miscellaneous Symbols and Arrows" "Y" "X" nil nil nil nil nil nil)
    ("Latin Extended-C" "Y" "X" nil nil nil nil nil nil)
    ("Cyrillic Extended-A" "Y" "X" nil nil nil nil nil nil)
    ("Supplemental Punctuation" "Y" "X" nil nil nil nil nil nil)
    ("CJK Radicals Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Kangxi Radicals" "Y" "X" nil nil nil nil nil nil)
    ("Ideographic Description Characters" "Y" "Y" "Y" "Y" "Y" "Y" "Y" "X")
    ("CJK Symbols and Punctuation" "Y" "X" nil nil nil nil nil nil)
    ("Hiragana" "Y" "X" "X" nil nil nil nil nil)
    ("Katakana" "Y" "X" "X" nil nil nil nil nil)
    ("Bopomofo" "Y" "X" nil nil nil nil nil nil)
    ("Hangul Compatibility Jamo" "Y" "X" nil nil nil nil nil nil)
    ("Kanbun" "Y" "X" nil nil nil nil nil nil)
    ("Bopomofo Extended" "Y" "X" nil nil nil nil nil nil)
    ("CJK Strokes" "Y" "X" nil nil nil nil nil nil)
    ("Katakana Phonetic Extensions" "Y" "X" nil nil nil nil nil nil)
    ("Enclosed CJK Letters and Months" "Y" "X" nil nil nil nil nil nil)
    ("CJK Compatibility" "Y" "X" nil nil nil nil nil nil)
    ("CJK Unified Ideographs Extension A" "Y" "X" nil nil nil nil nil nil)
    ("Yijing Hexagram Symbols" "Y" "X" nil nil nil nil nil nil)
    ("CJK Unified Ideographs" "Y" nil "X" nil nil nil nil nil)
    ("Cyrillic Extended-B" "Y" "X" nil nil nil nil nil nil)
    ("Modifier Tone Letters" "Y" "X" nil nil nil nil nil nil)
    ("Latin Extended-D" "Y" "X" nil nil nil nil nil nil)
    ("Hangul Jamo Extended-A" "Y" "X" nil nil nil nil nil nil)
    ("Latin Extended-E" "Y" "X" nil nil nil nil nil nil)
    ("Hangul Syllables" "Y" "X" nil nil nil nil nil nil)
    ("Hangul Jamo Extended-B" "Y" "X" nil nil nil nil nil nil)
    ("Private Use Area" "Y" "X" nil nil nil nil nil nil)
    ("CJK Compatibility Ideographs" "Y" "X" nil nil nil nil nil nil)
    ("Alphabetic Presentation Forms" nil "X" nil nil nil nil nil nil)
    ("Variation Selectors" "Y" "X" nil nil nil nil nil nil)
    ("Vertical Forms" "Y" "X" nil nil nil nil nil nil)
    ("Combining Half Marks" "Y" "X" nil nil nil nil nil nil)
    ("CJK Compatibility Forms" "Y" "X" nil nil nil nil nil nil)
    ("Halfwidth and Fullwidth Forms" "Y" "X" nil nil nil nil nil nil)
    ("Specials" "Y" "X" nil nil nil nil nil nil)
    ("Kana Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Kana Extended-A" "Y" "X" nil nil nil nil nil nil)
    ("Tai Xuan Jing Symbols" "Y" "X" nil nil nil nil nil nil)
    ("Counting Rod Numerals" "Y" "X" nil nil nil nil nil nil)
    ("Mathematical Alphanumeric Symbols" "Y" "X" nil nil nil nil nil nil)
    ("Mahjong Tiles" "Y" "X" nil nil nil nil nil nil)
    ("Domino Tiles" "Y" "X" nil nil nil nil nil nil)
    ("Playing Cards" "Y" "X" nil nil nil nil nil nil)
    ("Enclosed Alphanumeric Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Enclosed Ideographic Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Miscellaneous Symbols and Pictographs" "Y" "X" nil nil nil nil nil nil)
    ("Emoticons" "Y" "X" nil nil nil nil nil nil)
    ("Ornamental Dingbats" "Y" "X" nil nil nil nil nil nil)
    ("Transport and Map Symbols" "Y" "X" nil nil nil nil nil nil)
    ("Alchemical Symbols" "Y" "X" nil nil nil nil nil nil)
    ("Geometric Shapes Extended" "Y" "X" nil nil nil nil nil nil)
    ("Supplemental Arrows-C" "Y" "X" nil nil nil nil nil nil)
    ("Supplemental Symbols and Pictographs" "Y" "X" nil nil nil nil nil nil)
    ("Chess Symbols" "Y" "X" nil nil nil nil nil nil)
    ("CJK Unified Ideographs Extension B" nil nil nil "Y" "X" nil nil nil)
    ("CJK Unified Ideographs Extension C" nil nil nil nil nil "Y" "X" nil)
    ("CJK Unified Ideographs Extension D" nil nil nil nil nil "Y" "X" nil)
    ("CJK Unified Ideographs Extension E" nil nil nil nil nil "Y" "X" nil)
    ("CJK Unified Ideographs Extension F" nil nil nil nil nil "Y" "X" nil)
    ("CJK Compatibility Ideographs Supplement" nil nil nil nil nil "Y" "X" nil)
    ("Tags" nil nil nil nil nil "Y" "X" nil)
    ("Variation Selectors Supplement" nil nil nil nil nil "Y" "X" nil))
  )

(defvar gw-list-blocks-list) ;; ブロックの一覧を入れる
(defvar gw-list-blocks-hash) ;; 各文字が属するブロックを入れる
(defvar gw-list-blocks-table) ;; gw名を入れる

;; sfont
(defvar gw-list-sfont-template "template.sfont")
(defvar gw-list-range '(("HanaMin"  ("HanaMinA" . "[[\\u0000-\\uD7FF][\\uE000-\\uFFFD]]")
                                    ("HanaMinB" . "[\\u20000-\\u2A6D6]")
                                    ("HanaMinC" . "[\\u2A700-\\u2FFFD]"))
                        ("HanaMinX" ("HanaMinAX" . "[[\\u0000-\\u4DFF][\\uA000-\\uD7FF][\\uE000-\\uFFFD]]")
                                    ("HanaMinUX" . "[\\u4E00-\\u9FFF]")
                                    ("HanaMinBX" . "[\\u20000-\\u2A6D6]")
                                    ("HanaMinCX" . "[\\u2A700-\\u2FFFD]"))))
(defvar gw-list-component-format
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

(defun gw-list-output-wiki ()
  "Output numbers of glyph names to `gw-list.wiki'."
  (interactive)
  (require 'dash)
  (with-temp-file gw-list-wiki
    (insert (format ",フォント名,%s\n"
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
    (delete-matching-lines ",0,0,0,0,0,0,0,0")
    ))

(defun gw-list-output-files ()
  "Output numbers of glyph names to `gw-list.wiki'."
  (interactive)
  (cl-do ((i 0 (1+ i))) ((>= i gw-list-num))
    (with-temp-file (concat "HanaMin" (elt gw-list-font i) ".list")
      (dolist (block gw-list-blocks-list)
        (let* ((val (gethash block gw-list-blocks-table))
               (gw-names (elt val i)))
          (dolist (gw-name (sort (copy-sequence gw-names) 'string<))
            (insert gw-name "\n")))))))

;; test (gw-list-output-sfont "8.021")
(defun gw-list-output-sfont (version)
  "Output sfont file with VERSION."
  (dolist (range gw-list-range)
    (let ((font (car range))
          (ranges (cdr range)))
      (with-temp-file (concat font ".sfont")
        (insert-file-contents gw-list-sfont-template)
        (goto-char (point-min))
        (search-forward "$$FONT$$")
        (replace-match font t)
        (search-forward "$$VERSION$$")
        (replace-match version t)
        (search-forward "$$COMPONENTS$$")
        (replace-match
         (mapconcat (lambda (x)
                      (format gw-list-component-format (car x) (cdr x)))
                    ranges "\n") t t)))))

(defun gw-list (argv)
  "Non interactive output to files.  ARGV is version."
  (gw-list-load-blocks)
  (gw-list-load-dump-newest)
  (gw-list-output-files)
  (gw-list-output-sfont (car argv)))

(when noninteractive
  (message "invoking from script")
  (gw-list argv))

(unless noninteractive
  (gw-list-load-blocks)
  (gw-list-load-dump-newest)
  (gw-list-output-wiki))

(provide 'gw-list)

;;; gw-list.el ends here
