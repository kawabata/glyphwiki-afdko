;;; fix-features.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Taichi Kawabata

;; Author: Taichi Kawabata <kawabata@morven>
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

;; This will fix AFDKO feature file if possible.

;; rule 1.  If it is more than 5000 lines, then split them to lookup tables.
;; rule 2.  If there is no substitutes, then remove entire features.

;;; Code:

(require 'cl-lib)

(defvar fix-features-skip-lines
  '(("ccmp" . 5000)
    ("locl" . 100000)
    ("trad" . 500)))

(defun fix-features (fname)
  "Fix too long feature of FNAME."
  (with-temp-file fname
    (let (feature
          start end
          end-line
          skip-lines
          count)
      (insert-file-contents fname)
      (while (and (re-search-forward "feature \\(....\\) {" nil t)
                  (setq feature (match-string 1))
                  (setq skip-lines (assoc feature fix-features-skip-lines)))
        (setq start (match-beginning 0))
        (search-forward (concat "} " feature "\n"))
        (setq end (match-end 0))
        (forward-line -2)
        (setq end-line (line-number-at-pos))
        (save-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (if (not (search-forward "substitute" nil t))
                ;; remove entire features
                (delete-region (point-min) (point-max))
              (setq count 0)
              (while (< skip-lines (- end-line (line-number-at-pos)))
                (insert (format "  lookup %s%d useExtension {\n" feature count))
                (forward-line skip-lines)
                (search-forward "substitute")
                (beginning-of-line)
                (insert (format "  } %s%d;\n" feature count))
                (cl-incf count)))))))))

(when noninteractive
  (fix-features (car argv)))

(provide 'fix-features)
;;; fix-features.el ends here
