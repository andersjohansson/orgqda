;;; orgqda-helm-tags.el --- New completion for orgqda-mode tags -*- lexical-binding: t -*-

;; Copyright (C) 2017 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2017-02-06
;; Modified: 2017-08-07
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

(defvar orgqda-helm-tags-history)

(defun orgqda-helm-tags--source ()
  (helm-build-sync-source "Orgqda select tags (C-RET finishes):"
    :history  'orgqda-helm-tags-history
    :fuzzy-match t
    :keymap helm-comp-read-map
    :action  '(("Set tags to" . (lambda (_c) (helm-marked-candidates)))
               ("Delete marked tags" . orgqda-helm-tags-delete-tag))
    :persistent-action #'orgqda-helm-tags-display-tagged
    :candidates 'orgqda-helm-tags-comp-list))

(defvar orgqda-helm-tags--coll-buffer nil)
(defun orgqda-helm-tags-display-tagged (tag)
  "Show occurences of currently selected tag.
Calls `orgqda-collect-tagged'"
  (with-current-buffer helm-current-buffer
    ;; should ideally be killed after switching but how?
    (if (buffer-live-p orgqda-helm-tags--coll-buffer)
        (with-current-buffer orgqda-helm-tags--coll-buffer
          (read-only-mode -1)
          (erase-buffer))
      (setq orgqda-helm-tags--coll-buffer
            (generate-new-buffer "orgqda-helm-tags-view")))
    (orgqda-collect-tagged tag 1 orgqda-helm-tags--coll-buffer)
    (bury-buffer orgqda-helm-tags--coll-buffer)))


(defun orgqda-helm-tags--fallback-source ()
  (helm-build-dummy-source "Add new tag"
    :keymap helm-comp-read-map
    :action '(("Insert new" . (lambda (c) (list c))))))

(defvar orgqda-helm-tags-comp-list nil)
(defvar orgqda-helm-tags-current-tags nil)

(defun orgqda-helm-tags-delete-tag (_c)
  "Deletes current or marked tags"
  (let ((delete-tags
         (cl-intersection orgqda-helm-tags-current-tags
                          (helm-marked-candidates)
                          :test 'equal)))
    (setq orgqda-helm-tags-current-tags
          (cl-nset-difference orgqda-helm-tags-current-tags delete-tags
                              :test 'equal))
    (dolist (y delete-tags)
      (when-let ((n (cl-position ; otherwise just fail
                     y orgqda-helm-tags-comp-list
                     :test (lambda (a b) (string= a (cdr b))))))
        (setq orgqda-helm-tags-comp-list
              (nconc (cl-subseq orgqda-helm-tags-comp-list 0 n)
                     ;; this will be in the end, no good way of
                     ;; "resorting" it correctly
                     (orgqda-helm-tags--reformat-deleted-list-item
                      (nth n orgqda-helm-tags-comp-list))
                     (cl-subseq orgqda-helm-tags-comp-list (1+ n)))))))
  ;; return empty list to continue loop
  ())


;;;###autoload
(advice-add 'org-set-tags-command :around #'orgqda-helm-tags-override)
;;;###autoload
(defun orgqda-helm-tags-override (oldfun &rest args)
  "Call `orgqda-helm-set-tags' when `orgqda-mode' is active."
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
             (orgqda-helm-tags-current-tags (nreverse (org-get-local-tags)))
             (cl1 (orgqda--get-tags-alist orgqda-helm-tags-sort))
             cllast
             (clfirst (cl-loop for tag in cl1
                               if (cl-member tag orgqda-helm-tags-current-tags
                                             :test
                                             (lambda (a b) (string= (car a) b)))
                               do (push (orgqda-helm-tags--format-list-item tag t) cllast)
                               else collect (orgqda-helm-tags--format-list-item tag)))
             (orgqda-helm-tags-comp-list (nconc clfirst (nreverse cllast))))
        (org-set-tags-to
         (cl-remove-duplicates
          (cl-loop with newtags
                   until (or (eq helm-exit-status 1) (equal "" newtags))
                   do (setq newtags
                            (helm :sources
                                  (list (orgqda-helm-tags--source)
                                        (orgqda-helm-tags--fallback-source))
                                  :candidate-number-limit 99999
                                  :buffer "*helm orgqda tags*"))
                   if (listp newtags)
                   do (dolist (tag newtags)
                        (push tag orgqda-helm-tags-current-tags)
                        (orgqda-helm-tags--update-tag-in-complist tag))
                   finally return (nreverse orgqda-helm-tags-current-tags))
          :test 'string=)))
    (user-error "Not at org heading")))

(defun orgqda-helm-tags--update-tag-in-complist (tag)
  (setq orgqda-helm-tags-comp-list
        (if-let ((n (cl-position
                     tag orgqda-helm-tags-comp-list
                     :test (lambda (a b) (string= a (cdr b))))))
            (nconc (cl-subseq orgqda-helm-tags-comp-list 0 n)
                   (cl-subseq orgqda-helm-tags-comp-list (1+ n))
                   (orgqda-helm-tags--reformat-added-list-item
                    (nth n orgqda-helm-tags-comp-list)))
          (nconc orgqda-helm-tags-comp-list
                 (orgqda-helm-tags--reformat-added-list-item (cons tag tag))))))

(defun orgqda-helm-tags--reformat-added-list-item (li)
  (list (cons (propertize (concat (car li) " +1") 'face 'bold) (cdr li))))

(defun orgqda-helm-tags--reformat-deleted-list-item (li)
  (list (cons (propertize (replace-regexp-in-string " \\+1$" "" (car li))
                          'face nil)
              (cdr li))))

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
