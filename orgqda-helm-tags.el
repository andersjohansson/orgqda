;;; orgqda-helm-tags.el --- New completion for orgqda-mode tags -*- lexical-binding: t -*-

;; Copyright (C) 2017 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2017-02-06
;; Modified: 2018-03-22
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
  inserting tags in `orgqda-mode'

If not set through customize, set it through calling
`orgqda-helm-tags-activate-completion' as:
\(orgqda-helm-tags-activate-completion 'orgqda-helm-tags-completion VALUE\)"
  :group 'orgqda
  :type 'boolean
  :set #'orgqda-helm-tags-activate-completion
  :initialize #'custom-initialize-reset)

(defun orgqda-helm-tags-activate-completion (name val)
  (set-default name val)
  (if val
      (add-hook 'orgqda-mode-hook #'orgqda-helm-tags-mode-activate-in-hook)
    (remove-hook 'orggda-mode-hook #'orgqda-helm-tags-mode-activate-in-hook)))

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
    ;; :match '(orgqda-helm-tags--fuzzy-match-name helm-fuzzy-match)
    ;; TODO, can this be made to work so it primarily matches on tag
    ;; name? (or real)
    :keymap helm-comp-read-map
    :action  '(("Set tags to" . (lambda (_c) (helm-marked-candidates)))
               ("Delete marked tags" . orgqda-helm-tags-delete-tag))
    :persistent-action #'orgqda-helm-tags-display-tagged
    :candidates 'orgqda-helm-tags-comp-list))

;; (defun orgqda-helm-tags--fuzzy-match-name (cand)
;;   (helm-fuzzy-match
;;    (substring cand 0 (next-property-change 0 cand))))


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


(defmacro orgqda-helm-tags-propertize-if (condition string &rest properties)
  "Returns STRING propertized with PROPERTIES if condition
evaluates to non-nil, otherwise returns STRING"
  (declare (indent 1) (debug t))
  `(if ,condition
       (propertize ,string ,@properties)
     ,string))

;;;###autoload
(defun orgqda-helm-tags-set-tags (&optional arg just-align)
  "Use helm-completion to select multiple tags to add to heading
Continues completing until exited with C-RET,M-RET or C-g" 
  (interactive "P")
  (if (or arg just-align)
      (funcall #'org-set-tags-command arg just-align)
    (if (org-at-heading-p)
        (let* ((helm-exit-status 0)
               (helm-truncate-lines t)
               (orgqda-helm-tags-current-tags (nreverse (org-get-local-tags)))
               (cl1 (orgqda-helm-tags--get-tags-list))
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
      (user-error "Not at org heading"))))

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
  (list (cons (propertize (concat  "+ " (car li)) 'face 'bold) (cdr li))))

(defun orgqda-helm-tags--reformat-deleted-list-item (li)
  (list (cons (propertize (replace-regexp-in-string "^\\+" "" (car li))
                          'face nil)
              (cdr li))))

(defun orgqda-helm-tags--format-list-item (x &optional incurrent?)
  (cons
   (orgqda-helm-tags-propertize-if incurrent?
     (format
      "%-40s %5s %s"
      (car x)
      (propertize (format "%d" (cadr x))
                  'face 'font-lock-function-name-face)
      (if-let ((info (nth 2 x)))
          (propertize info 'face 'shadow)
        ""))
     'face '(font-lock-comment-face (:strike-through t)))
   (car x)))

;;; Info from codebook file
(defun orgqda-helm-tags--get-tags-list ()
  "Gets list of tags with count and (possible) coding info."
  (let ((info (orgqda-helm-tags--get-codebook-info)))
    (cl-loop for x in (orgqda--get-tags-alist orgqda-helm-tags-sort)
             collect (list (car x)
                           (cdr x)
                           (assoc-default (car x)
                                          info)))))

(defun orgqda-helm-tags--get-codebook-info ()
  "Returns an alist of tag names and coding info

Coding info is the first line of the matching line for the tag in
`orgqda-codebook-file'"
  (when orgqda-codebook-file
    (org-map-entries
     #'orgqda-helm-tags--get-tag-info
     t
     (list orgqda-codebook-file))))

(defun orgqda-helm-tags--get-tag-info ()
  (when (and (search-forward-regexp
              org-bracket-link-analytic-regexp (point-at-eol) t)
             (equal "otag" (match-string 2)))
    (let ((tag (cadr (split-string (match-string-no-properties 3) ":")))
          (text (substring-no-properties
                 (org-agenda-get-some-entry-text (point-marker) 1))))
      (when (and tag (not (string-blank-p text)))
        (cons tag text)))))

;;; Mode for overriding tag completion commands
(defvar orgqda-helm-tags-mode-map nil)
(unless orgqda-helm-tags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") #'orgqda-helm-tags-set-tags)
    (setq orgqda-helm-tags-mode-map map)))

;;;###autoload
(define-minor-mode orgqda-helm-tags-mode "Minor mode for using ‘orqda-helm-tags-completion’

\\{orgqda-helm-tags-mode-map}"
  :keymap orgqda-helm-tags-mode-map)

(defun orgqda-helm-tags-mode-activate-in-hook ()
  (if orgqda-mode
      (orgqda-helm-tags-mode)
    (orgqda-helm-tags-mode -1)))

(provide 'orgqda-helm-tags)

;;; orgqda-helm-tags.el ends here
