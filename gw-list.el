;;; gw-list.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  川幡 太一

;; Author: 川幡 太一 <kawabata.taichi@gmail.com>
;; Keywords: 

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

;; Generate list for specified glyphs

;;; Code:

(defvar gw-list-blocks "Blocks.txt")
(defvar gw-list-dump-newest "dump_newest_only.txt")

(defvar gw-list-normal-suffix1 ;; Variation Selector
  "^\\(-u\\(\\(e01[0-9a-f][0-9a-f]\\)\\|\\(20d[de]\\)\\|\\(fe0[0-f]\\)\\)\\)?$")
(defvar gw-list-normal-suffix2 ;; language
  "^\\(-[ghmktuv]\\|j[av]?\\|kp\\|us\\)?\\(-vert\\|-halfwidth\\)?$")
(defvar gw-list-extended-suffix1 "^-\\(var\\|itaiji\\)-[0-9][0-9][0-9]$")
(defvar gw-list-extended-suffix2 "^\\(-u[0-9a-f]\\{4,5\\}\\|-cdp-[0-9a-f]\\{4\\}\\)+$")

(defvar gw-list-font '("A" "UX" "AX" "B" "BX" "C" "CX" "I"))
(defvar gw-list
  '(("Basic Latin" "Y" "X" "X" "Y" "X" "Y" "X" "Y")
    ("Latin-1 Supplement" "Y" "X" nil nil nil nil nil nil)
    ("Latin Extended-A" nil "X" nil nil nil nil nil nil)
    ("Latin Extended-B" nil "X" nil nil nil nil nil nil)
    ("IPA Extensions" nil "X" nil nil nil nil nil nil)
    ("Spacing Modifier Letters" nil "X" nil nil nil nil nil nil)
    ("Combining Diacritical Marks" nil "X" nil nil nil nil nil nil)
    ("Greek and Coptic" nil "X" nil nil nil nil nil nil)
    ("Cyrillic" nil "X" nil nil nil nil nil nil)
    ("Cyrillic Supplement" nil "X" nil nil nil nil nil nil)
    ("Hangul Jamo" nil "X" nil nil nil nil nil nil)
    ("Combining Diacritical Marks Extended" nil "X" nil nil nil nil nil nil)
    ("Cyrillic Extended-C" nil "X" nil nil nil nil nil nil)
    ("Phonetic Extensions" nil "X" nil nil nil nil nil nil)
    ("Phonetic Extensions Supplement" nil "X" nil nil nil nil nil nil)
    ("Combining Diacritical Marks Supplement" nil "X" nil nil nil nil nil nil)
    ("Latin Extended Additional" nil "X" nil nil nil nil nil nil)
    ("Greek Extended" nil "X" nil nil nil nil nil nil)
    ("General Punctuation" "Y" "X" "X" "Y" "X" "Y" "X" "Y")
    ("Superscripts and Subscripts" nil "X" nil nil nil nil nil nil)
    ("Currency Symbols" nil "X" nil nil nil nil nil nil)
    ("Combining Diacritical Marks for Symbols" nil "X" nil nil nil nil nil nil)
    ("Letterlike Symbols" nil "X" nil nil nil nil nil nil)
    ("Number Forms" nil "X" nil nil nil nil nil nil)
    ("Arrows" nil "X" nil nil nil nil nil nil)
    ("Mathematical Operators" nil "X" nil nil nil nil nil nil)
    ("Miscellaneous Technical" nil "X" nil nil nil nil nil nil)
    ("Control Pictures" nil "X" nil nil nil nil nil nil)
    ("Optical Character Recognition" nil "X" nil nil nil nil nil nil)
    ("Enclosed Alphanumerics" "Y" "X" nil nil nil nil nil nil)
    ("Box Drawing" "Y" "X" nil nil nil nil nil nil)
    ("Block Elements" nil "X" nil nil nil nil nil nil)
    ("Geometric Shapes" "Y" "X" nil nil nil nil nil nil)
    ("Miscellaneous Symbols" nil "X" nil nil nil nil nil nil)
    ("Dingbats" nil "X" nil nil nil nil nil nil)
    ("Miscellaneous Mathematical Symbols-A" nil "X" nil nil nil nil nil nil)
    ("Supplemental Arrows-A" nil "X" nil nil nil nil nil nil)
    ("Braille Patterns" nil "X" nil nil nil nil nil nil)
    ("Supplemental Arrows-B" nil "X" nil nil nil nil nil nil)
    ("Miscellaneous Mathematical Symbols-B" nil "X" nil nil nil nil nil nil)
    ("Supplemental Mathematical Operators" nil "X" nil nil nil nil nil nil)
    ("Miscellaneous Symbols and Arrows" nil "X" nil nil nil nil nil nil)
    ("Latin Extended-C" nil "X" nil nil nil nil nil nil)
    ("Cyrillic Extended-A" nil "X" nil nil nil nil nil nil)
    ("Supplemental Punctuation" nil "X" nil nil nil nil nil nil)
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
    ("Cyrillic Extended-B" nil "X" nil nil nil nil nil nil)
    ("Modifier Tone Letters" nil "X" nil nil nil nil nil nil)
    ("Latin Extended-D" nil "X" nil nil nil nil nil nil)
    ("Hangul Jamo Extended-A" nil "X" nil nil nil nil nil nil)
    ("Latin Extended-E" nil "X" nil nil nil nil nil nil)
    ("Hangul Syllables" nil "X" nil nil nil nil nil nil)
    ("Hangul Jamo Extended-B" nil "X" nil nil nil nil nil nil)
    ("Private Use Area" nil "X" nil nil nil nil nil nil)
    ("CJK Compatibility Ideographs" "Y" "X" nil nil nil nil nil nil)
    ("Alphabetic Presentation Forms" nil "X" nil nil nil nil nil nil)
    ("Variation Selectors" nil "X" nil nil nil nil nil nil)
    ("Vertical Forms" nil "X" nil nil nil nil nil nil)
    ("Combining Half Marks" nil "X" nil nil nil nil nil nil)
    ("CJK Compatibility Forms" nil "X" nil nil nil nil nil nil)
    ("Halfwidth and Fullwidth Forms" nil "X" nil nil nil nil nil nil)
    ("Specials" nil "X" nil nil nil nil nil nil)
    ("Kana Supplement" nil "X" nil nil nil nil nil nil)
    ("Kana Extended-A" nil "X" nil nil nil nil nil nil)
    ("Tai Xuan Jing Symbols" nil "X" nil nil nil nil nil nil)
    ("Counting Rod Numerals" nil "X" nil nil nil nil nil nil)
    ("Mathematical Alphanumeric Symbols" nil "X" nil nil nil nil nil nil)
    ("Mahjong Tiles" nil "X" nil nil nil nil nil nil)
    ("Domino Tiles" nil "X" nil nil nil nil nil nil)
    ("Playing Cards" nil "X" nil nil nil nil nil nil)
    ("Enclosed Alphanumeric Supplement" nil "X" nil nil nil nil nil nil)
    ("Enclosed Ideographic Supplement" nil "X" nil nil nil nil nil nil)
    ("Miscellaneous Symbols and Pictographs" nil "X" nil nil nil nil nil nil)
    ("Emoticons" nil "X" nil nil nil nil nil nil)
    ("Ornamental Dingbats" nil "X" nil nil nil nil nil nil)
    ("Transport and Map Symbols" nil "X" nil nil nil nil nil nil)
    ("Alchemical Symbols" nil "X" nil nil nil nil nil nil)
    ("Geometric Shapes Extended" nil "X" nil nil nil nil nil nil)
    ("Supplemental Arrows-C" nil "X" nil nil nil nil nil nil)
    ("Supplemental Symbols and Pictographs" nil "X" nil nil nil nil nil nil)
    ("Chess Symbols" nil "X" nil nil nil nil nil nil)
    ("CJK Unified Ideographs Extension B" nil nil nil "Y" "X" nil nil nil)
    ("CJK Unified Ideographs Extension C" nil nil nil nil nil "Y" "X" nil)
    ("CJK Unified Ideographs Extension D" nil nil nil nil nil "Y" "X" nil)
    ("CJK Unified Ideographs Extension E" nil nil nil nil nil "Y" "X" nil)
    ("CJK Unified Ideographs Extension F" nil nil nil nil nil "Y" "X" nil)
    ("CJK Compatibility Ideographs Supplement" nil nil nil nil nil "Y" "X" nil)
    ("Tags" nil nil nil nil nil "Y" "X" nil)
    ("Variation Selectors Supplement" nil nil nil nil nil "Y" "X" nil)))

(defvar gw-list-blocks-list) ;; ブロックの一覧を入れる
(defvar gw-list-blocks-hash) ;; 各文字が属するブロックを入れる
(defvar gw-list-blocks-table) ;; gw名を入れる

(defun gw-list-load-blocks ()
  "Load data files."
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
  "Load dump_newest_only.txt file."
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
        (message gw-name)
        (if (null block-list)
            (setq block-list (puthash block-name
                                      (list nil nil nil nil nil nil nil nil)
                                      gw-list-blocks-table)))
        (cl-do ((i 0 (1+ i))) ((> i 8))
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
    ;; cdp todo
    (goto-char (point-min))
    (let ((block-name "Private Use Area")
          (list-entry (assoc block-name gw-list))
          (block-list (puthash "Private Use Area"
                               (list nil nil nil nil nil nil nil nil)
                               gw-list-blocks-table)))
      (while (re-search-forward "^ \\(cdp-[0-9a-f]+\\) " nil t)
        (cl-do ((i 0 (1+ i))) ((> i 8))
          (if (or (equal (elt list-entry (1+ i)) "Y")
                  (equal (elt list-entry (1+ i)) "X"))
              (cl-pushnew (match-string 1) (elt block-list i))))))
    ))

(defun gw-list-output-numbers ()
  "Output each block numbers in GlyphWiki format."
  (interactive)
  (message ",%s" (mapconcat 'identity gw-list-font ","))
  (let ((total (list 0 0 0 0 0 0 0 0)))
    (dolist (block gw-list-blocks-list)
      (let* ((val (gethash block gw-list-blocks-table))
             (len (mapcar 'length val)))
        (when len
          (setq total (-zip-with '+ total len))
          (message ",%s,%s"
                   block
                   (mapconcat (lambda (x) (format "%d" x)) len ","))
          gw-list-blocks-table)))
    (message ",total,%s" (mapconcat (lambda (x) (format "%d" x)) total ","))))

(defun gw-list-output-files ()
  (interactive)
  (cl-do ((i 0 (1+ i))) ((> i 8))
    (with-temp-file (concat "HanaMin" (elt gw-list-font i) ".list")
      (dolist (block gw-list-blocks-list)
        (let* ((val (gethash block gw-list-blocks-table))
               (gw-names (elt val i)))
          (dolist (gw-name (sort (copy-sequence gw-names) 'string<))
            (insert gw-name "\n")))))))

(provide 'gw-list)

;;; gw-list.el ends here
