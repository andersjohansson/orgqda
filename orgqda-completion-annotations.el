;;; orgqda-completion-annotations.el --- Annotate tag completions with orgqda data  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2021-07-28
;; Modified: 2022-01-03
;; Package-Requires: ((emacs "25.1") (org "9.3") (hierarchy "0.6.0") (orgqda "0.3"))
;; Keywords: outlines, wp
;; URL: http://www.gitlab.com/andersjohansson/orgqda

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

;;

;;; Code:

(require 'org)
(require 'orgqda)
(require 'marginalia)

;;;; Custom variables
(defgroup orgqda-completion-annotations nil
  "Options for orgqda tag completion annotations."
  :group 'orgqda)

(defcustom orgqda-completion-annotations-sort orgqda-default-sort-order
  "Sorting scheme used for tag completion in ‘orgqda-completion-annotations-mode’."
  :group 'orgqda
  :type '(choice (const :tag "By count, decreasing" count-decreasing)
                 (const :tag "By count, increasing" count-increasing)
                 (const :tag "A-Z" a-z)
                 (const :tag "Z-A" z-a))
  :safe #'orgqda-sort-arg-p)

(defcustom orgqda-completion-annotations-include-excluded nil
  "If non-nil, include tags listed in ‘orgqda-exclude-tags’ for completion."
  :group 'orgqda
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-completion-annotations-include-persistent nil
  "If non-nil, include tags in ‘org-tag-persistent-alist’ for completion."
  :group 'orgqda
  :type 'boolean)

(defcustom orgqda-completion-annotations-exclude-tags nil
  "List of tags to exclude from completion in ‘orgqda-helm-set-tags’.

This is an alternative to set an additional list of tags that
should not clutter the completion, apart from
‘orgqda-exclude-tags’. See also
‘orgqda-completion-annotations-include-excluded’."
  :group 'orgqda
  :type '(repeat string)
  :safe #'orgqda--list-of-strings-p)

(defvar orgqda-completion-annotations--align-space " "
  "String with displayed space property suitable for aligning annotations.
Slightly beyond longest tag.")

;;;###autoload
(define-minor-mode orgqda-completion-annotations-mode
  "Annotate org tag completions with orgqda data.

Note that activating this (local) minor mode does install some
persistent changes the first time it is invoked by adding the
completion category ‘org-tags’ to some commands in
‘marginalia-command-categories’ (just setting this variable
locally won’t work)."
  :keymap (make-sparse-keymap)
  (if orgqda-completion-annotations-mode
      (progn
        (add-to-list 'marginalia-command-categories '(org-set-tags-command . org-tags))

        ;; FIXME, should probably be using better completion
        ;; functions, but they are so tedious to create compared to
        ;; the simple (advice) made in marginalia.
        (add-to-list 'marginalia-command-categories '(orgqda-delete-tag . org-tags))
        (add-to-list 'marginalia-command-categories '(orgqda-rename-tag . org-tags))

        (add-to-list 'marginalia-annotator-registry '(org-tags orgqda-completion-annotations-annotate-count orgqda-completion-annotations-annotate-all none))
        (setq-local org-complete-tags-always-offer-all-agenda-tags t))
    ;; (setf (alist-get 'org-tags marginalia-annotator-registry nil 'remove) nil)
    (kill-local-variable 'org-complete-tags-always-offer-all-agenda-tags)))

(defun orgqda-completion-annotations-annotate-count (tag)
  "Annotate TAG with count."
  (concat orgqda-completion-annotations--align-space
          (propertize
           (orgqda-completion-annotations--get-count tag)
           'face 'shadow)))

(defun orgqda-completion-annotations-annotate-all (tag)
  "Annotate TAG with count and codebook info."
  (concat orgqda-completion-annotations--align-space
          (propertize (orgqda-completion-annotations--get-count tag) 'face 'bold)
          " "
          (propertize (orgqda-completion-annotations--get-info tag) 'face 'shadow)))

(defun orgqda-completion-annotations--get-count (tag)
  "Retrieve tag-count for TAG."
  (or (get-text-property 0 'orgqda-count tag) ""))

(defun orgqda-completion-annotations--get-info (tag)
  "Retrieve codebook info for TAG."
  (or (get-text-property 0 'orgqda-info tag) ""))

(defun orgqda-completion-annotations--get-tags-list ()
"Get list of tags, initialize auxilary variables.
Return alist of tags suitable as completion table in
‘org-set-tags-command’."
(let ((taglist (orgqda--get-tags-alist orgqda-completion-annotations-sort
                                       (append
                                        (if orgqda-completion-annotations-include-excluded
                                            ;; non-nil for overriding the default
                                            '("")
                                          orgqda-exclude-tags)
                                        orgqda-completion-annotations-exclude-tags)))
      (codebook-info (orgqda--get-codebook-info)))
  (append
   (cl-loop for (tag . count) in taglist
            ;; format of org tag completion table
            maximize (length tag) into ml
            and collect (list
                         (propertize
                          tag
                          'orgqda-count
                          (format "%5d" count)
                          'orgqda-info
                          (concat (alist-get tag codebook-info "" nil #'equal))))
            finally do
            (setq orgqda-completion-annotations--align-space
                  (propertize
                   " "
                   'display
                   `(space :align-to ,(+ 5 (or ml 0))))))
   ;; Tags in codebook with no count, add to returned taglist

   ;; FIXME: this does not follow sorting defined above. perhaps more
   ;; reasonable to lift that sort from orgqda--get-tags-alist and do
   ;; it correctly below. But when using for example prescient, this
   ;; sorting is ignored anyway.
   (cl-loop for (tag . i) in codebook-info
            ;; no prefixes or tags existing in hash
            unless (or (eq ?{ (string-to-char tag))
                       (gethash tag orgqda--current-tagscount nil))
            collect (list
                     (propertize
                      tag
                      'orgqda-count
                      "     0"
                      'orgqda-info (concat i)))))))


(provide 'orgqda-completion-annotations)
;;; orgqda-completion-annotations.el ends here
