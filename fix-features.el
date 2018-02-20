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

;;; Code:

(require 'cl-lib)

(defvar fix-features-substitute '("ccmp"))

(defun fix-features (fname)
  "Fix too long feature of FNAME."
  (with-temp-file fname
    (let (feature
          start
          end-line
          count)
      (insert-file-contents fname)
      (while (and (re-search-forward "feature \\(....\\) {" nil t)
                  (setq feature (match-string 1))
                  (member feature fix-features-substitute))
        (search-forward "substitute")
        (beginning-of-line)
        (setq start (point))
        (search-forward (concat "} " feature))
        (save-excursion
          (save-restriction
            (forward-line -1)
            (setq end-line (line-number-at-pos))
            (narrow-to-region start (point))
            (goto-char (point-min))
            (setq count 0)
            (while (< 5000 (- end-line (line-number-at-pos)))
              (insert (format "  lookup %s%d useExtension {\n" feature count))
              (forward-line 5000)
              (search-forward "substitute")
              (beginning-of-line)
              (insert (format "  } %s%d;\n" feature count))
              (cl-incf count))))))))

(when noninteractive
  (fix-features (car argv)))

(provide 'fix-features)
;;; fix-features.el ends here
