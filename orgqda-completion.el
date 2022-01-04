;;; orgqda-completion.el --- Annotate, sort and group tag completions with orgqda data  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.2
;; Created: 2021-07-28
;; Modified: 2022-01-11
;; Package-Requires: ((emacs "28.1") (org "9.3") (hierarchy "0.6.0") (orgqda "0.3"))
;; Keywords: outlines, wp
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

;;

;;; Code:

(require 'org)
(require 'orgqda)
(require 'marginalia)

;;;; Custom variables
(defgroup orgqda-completion nil
  "Options for orgqda tag completion annotations."
  :group 'orgqda)

(defcustom orgqda-completion-sort orgqda-default-sort-order
  "Sorting scheme used for tag completion in ‘orgqda-completion-mode’."
  :type '(choice (const :tag "By count, decreasing" count-decreasing)
                 (const :tag "By count, increasing" count-increasing)
                 (const :tag "A-Z" a-z)
                 (const :tag "Z-A" z-a))
  :safe #'orgqda-sort-arg-p)

(defcustom orgqda-completion-group nil
  "Whether to group tags in completion by orgqda hierarchy."
  :type '(choice (const :tag "Don’t group" nil)
                 (integer :tag "Group up to level")))

(defcustom orgqda-completion-include-excluded nil
  "If non-nil, include tags listed in ‘orgqda-exclude-tags’ for completion."
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-completion-include-persistent nil
  "If non-nil, include tags in ‘org-tag-persistent-alist’ for completion."
  :type 'boolean)

(defcustom orgqda-completion-exclude-tags nil
  "List of tags to exclude from completion in ‘orgqda-completion-mode’.

This is an alternative to set an additional list of tags that
should not clutter the completion, apart from
‘orgqda-exclude-tags’. See also
‘orgqda-completion-include-excluded’."
  :type '(repeat string)
  :safe #'orgqda--list-of-strings-p)

(defvar orgqda-completion--align-space " "
  "String with displayed space property suitable for aligning annotations.
Slightly beyond longest tag.")

;;;###autoload
(define-minor-mode orgqda-completion-mode
  "Activate annotations, grouping and sorting for org tag completions.

Note that activating this (local) minor mode does install some
persistent changes the first time it is invoked by adding the
completion category ‘org-tags’ to some commands in
‘marginalia-command-categories’, modifying
‘marginalia-annotator-registry’, and advising
‘completion-metadata-get’ (just setting this locally won’t
work)."
  :keymap (make-sparse-keymap)
  (if orgqda-completion-mode
      (progn
        (add-to-list 'marginalia-command-categories '(org-set-tags-command . org-tags))

        ;; FIXME, should probably be using better completion
        ;; functions, but they are so tedious to create compared to
        ;; the simple (advice) made in marginalia. The only real other
        ;; option is to create a custom completion function, but then
        ;; we have to override ‘org-set-tags-command’ instead and
        ;; can’t reuse it. Otherwise, many parts of the approach from
        ;; marginalia would have to be used, for example the
        ;; ‘minibuffer-setup-hook’ to store command name etc.
        (add-to-list 'marginalia-command-categories '(orgqda-delete-tag . org-tags))
        (add-to-list 'marginalia-command-categories '(orgqda-rename-tag . org-tags))

        (add-to-list 'marginalia-annotator-registry '(org-tags orgqda-completion-annotate-count orgqda-completion-annotate-all none))

        (advice-add 'completion-metadata-get :before-until #'orgqda-completion--completion-metadata-get)

        (setq-local org-complete-tags-always-offer-all-agenda-tags t))
    ;; (setf (alist-get 'org-tags marginalia-annotator-registry nil 'remove) nil)
    (kill-local-variable 'org-complete-tags-always-offer-all-agenda-tags)))

(defun orgqda-completion-cycle-sorting ()
  "Cycle sorting method for tag completion with orgqda."
  (interactive)
  (setq orgqda-completion-sort
        (car (nth (mod (1+ (cl-position orgqda-completion-sort orgqda-sort-args :key #'car))
                       (length orgqda-sort-args))
                  orgqda-sort-args)))
  ;; Hacky way of making sure completion display for vertico,
  ;; selectrum etc. is updated.
  (insert "a")
  (run-hooks 'post-command-hook)
  (delete-char -1))

(defun orgqda-completion--completion-metadata-get (metadata prop)
  "Meant as around-advice for ‘completion-metadata-get’.
METADATA is the metadata.
PROP is the property which is looked up.

Returns ‘orgqda-completion--sort’ and ‘orgqda-completion--group’
for sorting and grouping if we are completing org tags."
  (or (and (eq prop 'display-sort-function)
           (eq 'org-tags (completion-metadata-get metadata 'category))
           #'orgqda-completion--sort)
      (and orgqda-completion-group (< 0 orgqda-completion-group)
           (eq prop 'group-function)
           (eq 'org-tags (completion-metadata-get metadata 'category))
           #'orgqda-completion--group)))

(defun orgqda-completion--group (tag transform)
  "Group function to use for completion on TAG.
TRANSFORM is used as described in Info node ‘(elisp)Programmed Completion’"
  (let* ((pos  (cl-loop with p = -1
                        repeat orgqda-completion-group
                        do (if-let ((np (string-search orgqda-hierarchy-delimiter tag (1+ p))))
                               (setq p np)
                             (cl-return p))
                        finally return p))
         (pos (if (< pos 0) nil pos)))
    (if transform ;; return transformed tag
        (if pos
            (substring tag (1+ pos))
          tag)
      ;; else possibly return group
      (when pos
        (substring tag 0 pos)))))

(defun orgqda-completion--sort (taglist)
  "Sort TAGLIST according to ‘orgqda-completion-sort’."
  (cl-case orgqda-completion-sort
    (count-decreasing (cl-sort taglist '> :key 'orgqda-completion--get-count))
    (count-increasing (cl-sort taglist '< :key 'orgqda-completion--get-count))
    (a-z (sort taglist #'orgqda--string-lessp))
    (z-a (sort taglist #'orgqda--string-greaterp))
    (t taglist)))

(defun orgqda-completion-annotate-count (tag)
  "Annotate TAG with count."
  (concat orgqda-completion--align-space
          (propertize
           (format "%5d" (orgqda-completion--get-count tag))
           'face 'shadow)))

(defun orgqda-completion-annotate-all (tag)
  "Annotate TAG with count and codebook info."
  (concat orgqda-completion--align-space
          (propertize (format "%5d" (orgqda-completion--get-count tag)) 'face 'bold)
          " "
          (propertize (orgqda-completion--get-info tag) 'face 'shadow)))

(defun orgqda-completion--get-count (tag)
  "Retrieve tag-count for TAG."
  (or (get-text-property 0 'orgqda-count tag) 0))

(defun orgqda-completion--get-info (tag)
  "Retrieve codebook info for TAG."
  (or (get-text-property 0 'orgqda-info tag) ""))

(defun orgqda-completion--get-tags-list ()
  "Get list of tags including counts and codebook info.
Return alist of tags suitable as completion table in
‘org-set-tags-command’, with extra data in text properties."
  (let ((taglist (orgqda--get-tags-alist nil
                                         (append
                                          (if orgqda-completion-include-excluded
                                              ;; non-nil for overriding the default
                                              '("")
                                            orgqda-exclude-tags)
                                          orgqda-completion-exclude-tags)))
        (codebook-info (orgqda--get-codebook-info)))
    (append
     (cl-loop for (tag . count) in taglist
              ;; format of org tag completion table
              maximize (length tag) into ml
              and collect (list
                           (propertize
                            tag
                            'orgqda-count
                            count
                            'orgqda-info
                            (concat (alist-get tag codebook-info "" nil #'equal))))
              finally do
              (setq orgqda-completion--align-space
                    (propertize
                     " "
                     'display
                     `(space :align-to ,(+ 5 (or ml 0))))))
     ;; Tags in codebook not present in any file, add these also to returned taglist
     (cl-loop for (tag . i) in codebook-info
              ;; no prefixes or tags existing in hash
              unless (or (eq ?{ (string-to-char tag))
                         (gethash tag orgqda--current-tagscount nil))
              collect (list
                       (propertize
                        tag
                        'orgqda-count
                        0
                        'orgqda-info (concat i)))))))


(provide 'orgqda-completion)
;;; orgqda-completion.el ends here
