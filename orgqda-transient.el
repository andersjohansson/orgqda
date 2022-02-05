;;; orgqda-transient.el --- Transients for invoking orgqda commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.2
;; Created: 2021-04-12
;; Modified: 2022-02-05
;; Package-Requires: ((orgqda "0.5") (transient "0.3.0"))
;; Keywords: convenience, wp
;; URL: https://www.gitlab.com/andersjohansson/orgqda

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
  [["List tags"
    :pad-keys t
    ("-s" orgqda-transient-sort)
    ("-e" orgqda-transient-full)
    ("-f" orgqda-transient-notagfiles)
    ("-p" orgqda-transient-startprefix)
    ("l" "List tags" orgqda-transient-list-tags-suffix)]
   ["Taglist buffer"
    :if (lambda () (or orgqda-list-mode orgqda-codebook-mode))
    ("g" "Revert list (update buffer)" orgqda-revert-taglist)
    ;; ("o" orgqda-transient-sort)
    ("-n" "Non-recursive" orgqda-transient-buffer-non-recursive)
    ("s" "Sort taglist at point or region" orgqda-transient-sort-taglist)
    ("S" "Sort taglists in whole buffer" orgqda-transient-sort-taglist-buffer)]]
  [["Collect"
    ("c" "Collect tagged" orgqda-collect-tagged)]
   ["Relations"
    :pad-keys t
    ("r" "Tag relations" orgqda-transient-tag-relations)
    ("-k" orgqda-transient-tag-relations-k)]
   ["Csv"
    ("v" "Collect csv" orgqda-collect-tagged-csv)
    ("V" "Collect csv save" orgqda-collect-tagged-csv-save)
    ("B" "Save all tag collections as csv" orgqda-collect-tagged-csv-save-all)]
   ["Tag Actions"
    ("m" "Refile and merge" orgqda-refile-and-merge-tags)
    ("n" "Rename tag" orgqda-rename-tag)
    ("p" "Prefix tag" orgqda-prefix-tag)
    ("P" "Rename prefix on this tag" orgqda-rename-prefix-on-one-tag)
    ("i" "Rename prefix on all tags" orgqda-rename-prefix)
    ("k" "Delete tag" orgqda-delete-tag)]
   ["Toggle"
    ("m" "Toggle mode" orgqda-mode)]])


(defvar-local orgqda-transient--options nil)

;;;; Listing and options
(defclass orgqda-transient-option (transient-infix)
  ((argument :initform "")
   (var :initform 'orgqda-transient--options :allocation :class)
   (param :initarg :param)
   (format :initform " %k %d %v"))

  "Class for storing orgqda options in local lisp variables.
Uses slots var and param for determining variable and plist key
to store in.")

(defclass orgqda-transient-switch (orgqda-transient-option)
  ())

(cl-defmethod transient-init-value ((obj orgqda-transient-option))
  (oset obj value
        (plist-get (symbol-value (oref obj var)) (oref obj param))))

(cl-defmethod transient-format-value ((obj orgqda-transient-option))
  (propertize (pcase (oref obj value)
                ;; (plist-get (oref obj var) (oref obj param))

                ((pred (eq t)) "Yes")
                ((pred (eq nil)) "")
                ((and (pred symbolp) symb)
                 (symbol-name symb))
                ((and (pred stringp) str)
                 str)
                ((and (pred numberp) num)
                 (number-to-string num))
                (_ ""))
              'face 'transient-argument))

(defclass orgqda-transient-taglist-parameter (orgqda-transient-option)
  ((var :initform 'orgqda--taglist-parameters)))

(defclass orgqda-transient-taglist-parameter-switch (orgqda-transient-taglist-parameter orgqda-transient-switch)
  ())

(cl-defmethod transient-infix-read ((obj orgqda-transient-switch))
  "Toggle the switch on or off."
  (if (oref obj value) nil t))

(cl-defmethod transient-infix-set ((obj orgqda-transient-option) val)
  "Set orgqda list option defined by OBJ to VALUE."
  (oset obj value val)
  (setf (plist-get (symbol-value (oref obj var)) (oref obj param)) val))

(transient-define-infix orgqda-transient-full ()
  :class 'orgqda-transient-taglist-parameter-switch
  :param :full
  :description "Include extracts")

(transient-define-infix orgqda-transient-notagfiles ()
  :class 'orgqda-transient-taglist-parameter-switch
  :param :no-tag-files
  :description "Ignore ‘orgqda-tag-files’")

(transient-define-infix orgqda-transient-sort ()
  :class 'orgqda-transient-taglist-parameter
  :param :sort
  :always-read t
  :description "Sort"
  :reader (lambda (_p ii hist)
            (intern (completing-read
                     "Sort: "
                     (mapcar #'car orgqda-sort-parameters)
                     nil t ii hist))))

(transient-define-infix orgqda-transient-startprefix ()
  :class 'orgqda-transient-taglist-parameter
  :param :startprefix
  :description "Start prefix"
  :reader (lambda (_p ii hist)
            (concat (completing-read
                     "Start prefix: "
                     (orgqda--get-prefixes-for-completion)
                     nil nil ii hist)
                    orgqda-hierarchy-delimiter)))

(transient-define-suffix orgqda-transient-list-tags-suffix ()
  :description "Run orgqda-list-tags with given options."
  (interactive)
  (apply #'orgqda-list-tags nil orgqda--taglist-parameters))


;;;; Tag relations
(transient-define-suffix orgqda-transient-tag-relations (k)
  :description "Tag relations"
  (interactive (list (plist-get orgqda-transient--options :tag-relations-k)))
  (orgqda-view-tag-relations (or k 2)))

(transient-define-infix orgqda-transient-tag-relations-k ()
  :description "K-tuples"
  :class 'orgqda-transient-option
  :param :tag-relations-k
  :reader (lambda (_p ii hist)
            (string-to-number (transient-read-number-N+ "k-tuple: " ii hist))))

;;;; taglist-buffer sorting
(transient-define-infix orgqda-transient-buffer-non-recursive ()
  :description "Non-recursive"
  :class 'orgqda-transient-switch
  :param :buffer-non-recursive)

(transient-define-suffix orgqda-transient-sort-taglist-buffer (sort)
  :description "orgqda-sort-taglist-buffer"
  (interactive (list (plist-get orgqda--taglist-parameters :sort)))
  (orgqda-sort-taglist-buffer sort))

(transient-define-suffix orgqda-transient-sort-taglist (sort non-recursive)
  :description "orgqda-sort-taglist"
  (interactive (list (plist-get orgqda--taglist-parameters :sort)
                     (plist-get orgqda-transient--options :buffer-non-recursive)))
  (orgqda-sort-taglist sort non-recursive))



(provide 'orgqda-transient)
;;; orgqda-transient.el ends here
