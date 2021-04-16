;;; orgqda-transient.el --- Transients for invoking orgqda commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Keywords: convenience, wp
;; Created: 2021-04-12
;; Modified: 2021-04-16
;; Package-Requires: ((orgqda "0.2") (transient "0.3.0"))

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

;; Define useful transients for orgqda. Bind ‘orgqda-transient’ to a
;; suitable key in ‘orgqda-mode-map’. With my swedish keyboard I use:
;; (bind-key "C-c C-ö" #'orgqda-transient orgqda-mode-map)

;;; Code:
(require 'orgqda)
(require 'transient)

;;;; Starting point

(transient-define-prefix orgqda-transient ()
  "Transient for invoking orgqda commands"
  [["Toggle"
    ("m" "Toggle mode" orgqda-mode)]
   ["List"
    ("l" "List tags" orgqda-list-tags)
    ("L" "List and extract" orgqda-list-tags-full)
    ("k" "List with options" orgqda-transient-list-tags)]
   ["Collect"
    ("c" "Collect tagged" orgqda-collect-tagged)
    ("C-c" "Collect tagged" orgqda-collect-tagged)]
   ["Relations"
    ("R" orgqda-transient-tag-relations-k)
    ("r" "Tag relations" orgqda-transient-tag-relations)]
   ["Csv"
    ("v" "Collect csv" orgqda-collect-tagged-csv)
    ("V" "Collect csv save" orgqda-collect-tagged-csv-save)
    ("b" "Save all as csv" orgqda-collect-tagged-csv-save-all)]])


;;;; List-tags
(transient-define-prefix orgqda-transient-list-tags ()
  "Transient for ‘orgqda-list-tags’"
  [["Options"
    ("s" orgqda-transient-sort)
    ("e" "Include extracts" "full")
    ("f" "No tag files" "notagfiles")
    ("p" orgqda-transient-startprefix)]]
  [("RET" orgqda-transient-list-tags-suffix)])

(transient-define-argument orgqda-transient-sort ()
  :description "Sort"
  :class 'transient-switches
  :argument-format "sort=%s"
  :argument-regexp "\\(count-decreasing\\|a-z\\|z-a\\|count-increasing\\)"
  :choices '("count-decreasing" "a-z" "z-a" "count-increasing"))

(transient-define-argument orgqda-transient-startprefix ()
  :description "Start prefix"
  :class 'transient-option
  :argument "sp="
  :reader (lambda (prompt initial-input history)
            (completing-read prompt
                             (orgqda--get-prefixes-for-completion)
                             nil nil
                             initial-input
                             history)))

(transient-define-suffix orgqda-transient-list-tags-suffix (sort full startprefix notagfiles)
  :description "orgqda-list-tags"
  (interactive (let ((args (transient-args transient-current-command)))
                 (list
                  (transient-arg-value "sort=" args)
                  (transient-arg-value "full" args)
                  (transient-arg-value "sp=" args)
                  (transient-arg-value "notagfiles" args))))
  (orgqda-list-tags (intern-soft sort)
                    full
                    nil nil nil
                    (when startprefix (concat startprefix "_"))
                    notagfiles))

;;;; Tag relations
(transient-define-suffix orgqda-transient-tag-relations (k)
  :description "Tag relations"
  (interactive (list
                (transient-arg-value "tr=" (transient-args transient-current-command))))

  (orgqda-view-tag-relations (if k (string-to-number k) 2)))

(transient-define-argument orgqda-transient-tag-relations-k ()
  :description "k-tuples"
  :class 'transient-option
  :argument "k="
  :reader #'transient-read-number-N+)




(provide 'orgqda-transient)
;;; orgqda-transient.el ends here
