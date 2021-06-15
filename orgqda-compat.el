;;; orgqda-compat.el --- Compatibiity functions for orgqda  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Keywords: wp

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

;; A few compatibility functions for orgqda.

;;; Code:

(require 'orgqda)

(defun orgqda-compat-update-otag-links ()
  "Update otag links in buffer from old format to version 0.3 format.
This means “otag:filename:tag” → “otag:tag[:filename]”. This
doesn’t work for otag links with filenames containing : (like
windows ”C:/”). This has always been broken before. Regenerate
files in that case."
  (interactive)
  (orgqda--temp-work t
    (while (re-search-forward
            "\\[\\[otag:\\(?1:[^:]+\\)?:\\(?2:[[:alnum:]_@#%+{}]+\\)"
            nil t)
      (replace-match
       (concat "[[otag:" (match-string 2)
               (orgqda--string-or-empty (match-string 1) ":"))
       t t))))


(provide 'orgqda-compat)
;;; orgqda-compat.el ends here
