;;; orgqda-transient.el --- Transients for invoking orgqda commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Keywords: convenience, wp
;; Created: 2021-04-12
;; Modified: 2021-07-05
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
    ("o" "List with options" orgqda-transient-list-tags)]
   ["Collect"
    ("c" "Collect tagged" orgqda-collect-tagged)]
   ["Relations"
    ("r" "Tag relations" orgqda-transient-tag-relations)
    ("R" orgqda-transient-tag-relations-k)]
   ["Csv"
    ("v" "Collect csv" orgqda-collect-tagged-csv)
    ("V" "Collect csv save" orgqda-collect-tagged-csv-save)
    ("b" "Save all as csv" orgqda-collect-tagged-csv-save-all)]
   ["Actions"
    ("m" "Refile and merge" orgqda-refile-and-merge-tags)
    ("n" "Rename tag" orgqda-rename-tag)
    ("p" "Prefix tag" orgqda-prefix-tag)
    ("P" "Rename prefix on this tag" orgqda-rename-prefix-on-one-tag)
    ("i" "Rename prefix on all tags" orgqda-rename-prefix)
    ("k" "Delete tag" orgqda-delete-tag)]])


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
  :choices '("count-decreasing" "a-z" "z-a" "count-increasing")
  :init-value #'orgqda-transient--set-sort-init)

(defun orgqda-transient--set-sort-init (obj)
  "Set initial value of sort OBJ."
  ;; CHECK: does this interfere with transient value saving? Haven’t
  ;; looked into that yet.
  (oset obj value
        (concat "sort="
                (if-let ((sort (plist-get orgqda--taglist-parameters :sort)))
                    (symbol-name sort)
                  (symbol-name orgqda-default-sort-order)))))

(transient-define-argument orgqda-transient-startprefix ()
  :description "Start prefix"
  :class 'transient-option
  :argument "sp="
  :reader (lambda (prompt initial-input _history)
            (orgqda--completing-read-prefix prompt initial-input)))

(transient-define-suffix orgqda-transient-list-tags-suffix (sort full startprefix notagfiles)
  :description "orgqda-list-tags"
  (interactive (let ((args (transient-args transient-current-command)))
                 (list
                  (transient-arg-value "sort=" args)
                  (transient-arg-value "full" args)
                  (transient-arg-value "sp=" args)
                  (transient-arg-value "notagfiles" args))))
  (orgqda-list-tags nil
                    :sort (intern-soft sort)
                    :full full
                    :startprefix (when startprefix (concat startprefix "_"))
                    :no-tag-files notagfiles))

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



;;;; Transient for orgqda-list-mode and orgqda-codebook-mode

(transient-define-prefix orgqda-transient-list ()
  "Transient for invoking orgqda commands in list- and codebook-mode."
  [["Orgqda"
    ("a" "Orgqda actions" orgqda-transient)]
   ["Revert"
    ("g" "Revert list (update buffer)" orgqda-revert-taglist)]
   ["Sorting"
    ("o" orgqda-transient-sort)
    ("n" "Non-recursive" "non-recursive")
    ("s" "Sort taglist" orgqda-transient-sort-taglist)
    ("S" "Sort taglist, whole buffer" orgqda-transient-sort-taglist-buffer)]
   ["Actions"
    ("m" "Refile and merge" orgqda-refile-and-merge-tags)
    ("r" "Rename tag" orgqda-rename-tag)
    ("p" "Prefix tag" orgqda-prefix-tag)
    ("P" "Rename prefix on this tag" orgqda-rename-prefix-on-one-tag)
    ("i" "Rename prefix on all tags" orgqda-rename-prefix)
    ("k" "Delete tag" orgqda-delete-tag)]])


(transient-define-suffix orgqda-transient-sort-taglist (sort non-recursive)
  :description "orgqda-sort-taglist"
  (interactive (let ((args (transient-args transient-current-command)))
                 (list (transient-arg-value "sort=" args)
                       (transient-arg-value "non-recursive" args))))
  (orgqda-sort-taglist (intern-soft sort)
                       non-recursive))

(transient-define-suffix orgqda-transient-sort-taglist-buffer (sort)
  :description "orgqda-sort-taglist-buffer"
  (interactive (list (transient-arg-value "sort=" (transient-args transient-current-command))))
  (orgqda-sort-taglist-buffer (intern-soft sort)))


(provide 'orgqda-transient)
;;; orgqda-transient.el ends here
