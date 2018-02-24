;;; gw-fmndb.el ---                                  -*- lexical-binding: t; -*-

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

;; | PostScript名 | 日本語名        | 英語名                | カバー範囲                    |
;; |--------------+-----------------+-----------------------+-------------------------------|
;; | HanaMinExA1   | 花園明朝ＥＸ A1 | Hanazono Mincho Ex A1 | JIS X 0208 所属ブロック + URO |
;; | HanaMinExA2   | 花園明朝ＥＸ A2 | Hanazono Mincho Ex A2 | Extension-A + BMPその他       |
;; | HanaMinExB    | 花園明朝ＥＸ B  | Hanazono Mincho Ex B  | Extension-B + SMPその他       |
;; | HanaMinExC    | 花園明朝ＥＸ C  | Hanazono Mincho Ex C  | Extension-C                   |

;;; Code:

(defvar gw-fmndb
  '(("A"    "A"      "A"     )
    ("B"    "B"      "B"     )
    ("C"    "C"      "C"     )
    ("I"    "I"      "I"     )
    ("ExA1" "Ex A1"  "Ｅｘ A1")
    ("ExA2" "Ex A2"  "Ｅｘ A2")
    ("ExB"  "Ex B"   "Ｅｘ B" )
    ("ExC"  "Ex C"   "Ｅｘ C" )))

(defvar gw-fmndb-file "HanaMin%s.fmndb")

(defvar gw-fmndb-format
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

(defun gw-fmndb (sub)
  "Generate fmndb file for SUB."
  (let ((entry (assoc sub gw-fmndb)))
    (if entry
        (let* ((ps    (elt entry 0))
               (eng   (elt entry 1))
               (jpn   (elt entry 2))
               (sjis  (mapconcat (lambda (x) (format "\\%02x" x))
                                 (string-to-list (encode-coding-string jpn 'shift_jis)) ""))
               (ucs   (mapconcat (lambda (x) (format "\\%04x" x))
                                 (string-to-list jpn) "")))
          (with-temp-file (format gw-fmndb-file sub)
            (insert
             (format gw-fmndb-format
                     ps
                     ucs ucs
                     sjis sjis
                     eng eng))))
      (error "Font %s is not specified" sub))))

(when noninteractive
  (gw-fmndb (car argv)))

(provide 'gw-fmndb)

;;; gw-fmndb.el ends here
