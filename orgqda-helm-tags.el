;;; orgqda-helm-tags.el --- New completion for orgqda-mode tags -*- lexical-binding: t -*-

;; Copyright (C) 2017 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2017-02-06
;; Modified: 2017-06-06
;; Package-Requires: ((emacs "25.1"))
;; Keywords: outlines, wp
;; URL: http://www.github.com/andersjohansson/orgqda

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; orgqda-helm-tags redefines org tagging to work in a faster way with
;; helm under orgqda-mode

(require 'orgqda)
(require 'helm)
(require 'helm-mode)

(defcustom orgqda-helm-tags-completion t
  "Whether to use the custom `orgqda-helm-tags-set-tags' for
  inserting tags in `orgqda-mode'"
  :group 'orgqda
  :type 'boolean)

(defcustom orgqda-helm-tags-sort 'count-decreasing
  "Sorting scheme used in `orgqda-helm-set-tags'."
  :group 'orgqda
  :type '(choice (const :tag "By count, decreasing" count-decreasing)
                 (const :tag "By count, increasing" count-increasing)
                 (const :tag "A-Z" a-z)
                 (const :tag "Z-A" z-a)))

(defvar 'orgqda-helm-tags-history)

(defun orgqda-helm-tags--source ()
  (helm-build-sync-source "Orgqda select tags (C-RET finishes):"
    :history  'orgqda-helm-tags-history
    :fuzzy-match t
    :keymap helm-comp-read-map
    :action  '(("Set tags to" . (lambda (_c) (helm-marked-candidates))))
    :candidates 'orgqda-helm-tags-comp-list))

;;TODO, add action to remove a current item.

(defun orgqda-helm-tags--fallback-source ()
  (helm-build-dummy-source "Add new tag"
    :keymap helm-comp-read-map
    :action '(("Insert new" . (lambda (c) (list c))))))

(defvar orgqda-helm-tags-comp-list nil)

;;;###autoload
(advice-add 'org-set-tags-command :around #'orgqda-helm-tags-override)
;;;###autoload
(defun orgqda-helm-tags-override (oldfun &rest args)
  "Call `orgqda-tag-files' when `orgqda-mode' is active."
  (if (and (not (car args)) orgqda-mode orgqda-helm-tags-completion)
      (orgqda-helm-tags-set-tags)
    (apply oldfun args)))

(defmacro orgqda-helm-tags-propertize-if (condition string &rest properties)
  "Returns STRING propertized with PROPERTIES if condition
evaluates to non-nil, otherwise returns STRING"
  (declare (indent 1) (debug t))
  `(if ,condition
       (propertize ,string ,@properties)
     ,string))

;;;###autoload
(defun orgqda-helm-tags-set-tags ()
  "Use helm-completion to select multiple tags to add to heading
Continues completing until exited with C-RET,M-RET or C-g" 
  (interactive)
  (if (org-at-heading-p)
      (let* ((helm-exit-status 0)
             (current (nreverse (org-get-local-tags)))
             (cl1 (orgqda--get-tags-alist orgqda-helm-tags-sort))
             cllast
             (clfirst (cl-loop for x in cl1
                               if (cl-member x current
                                             :test (lambda (a b) (string= (car a) b)))
                               do (push (orgqda-helm-tags--format-list-item x t) cllast)
                               else collect (orgqda-helm-tags--format-list-item x)))
             (orgqda-helm-tags-comp-list (nconc clfirst (nreverse cllast))))
        (org-set-tags-to
         (cl-remove-duplicates
          (cl-loop with x
                   until (or (eq helm-exit-status 1) (equal "" x))
                   do (setq x (helm :sources
                                    (list (orgqda-helm-tags--source) (orgqda-helm-tags--fallback-source))
                                    :candidate-number-limit 99999
                                    :buffer "*helm orgqda tags*"))
                   if (listp x)
                   do (dolist (y x)
                        (push y current)
                        (setq orgqda-helm-tags-comp-list
                              (if-let ((n (cl-position y orgqda-helm-tags-comp-list
                                                       :test (lambda (a b) (string= a (cdr b))))))
                                  (nconc (cl-subseq orgqda-helm-tags-comp-list 0 n)
                                         (cl-subseq orgqda-helm-tags-comp-list (1+ n))
                                         (orgqda-helm-tags--reformat-added-list-item
                                          (nth n orgqda-helm-tags-comp-list)))
                                (nconc orgqda-helm-tags-comp-list
                                       (orgqda-helm-tags--reformat-added-list-item (cons y y))))))
                   finally return (nreverse current))
          :test 'string=)))
    (user-error "Not at org heading")))

(defun orgqda-helm-tags--reformat-added-list-item (li)
  (list (cons (propertize (concat (car li) " +1") 'face 'bold) (cdr li))))


(defun orgqda-helm-tags--format-list-item (x &optional incurrent?)
  (cons
   (orgqda-helm-tags-propertize-if incurrent?
     (concat
      (car x) " "
      (propertize (format "(%d)" (cdr x))
                  'face 'font-lock-function-name-face))
     'face '(font-lock-comment-face (:strike-through t)))
   (car x)))

(provide 'orgqda-helm-tags)

;;; orgqda-helm-tags.el ends here
