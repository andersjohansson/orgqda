;;; orgqda-helm-tags.el --- New completion for orgqda-mode tags -*- lexical-binding: t -*-

;; Copyright (C) 2017 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2017-02-06
;; Modified: 2020-10-02
;; Package-Requires: ((emacs "25.1") (org "9.3"))
;; Keywords: outlines, wp
;; URL: http://www.github.com/andersjohansson/orgqda

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; orgqda-helm-tags redefines org tagging to work in a faster way with
;; helm under orgqda-mode

;;; Code:

(require 'orgqda)
(require 'org-agenda)
(require 'org-capture)
(require 'helm)
(require 'helm-mode)

;;;; Custom variables
(defun orgqda-helm-tags-activate-completion (name val)
  "Setter for ‘orgqda-helm-tags-completion’.
Sets NAME to VAL and adds hook for activating
‘orgqda-helm-tags-mode’ in ‘orgqda-mode’"
  (set-default name val)
  (if val
      (add-hook 'orgqda-mode-hook #'orgqda-helm-tags-mode-activate-in-hook)
    (remove-hook 'orggda-mode-hook #'orgqda-helm-tags-mode-activate-in-hook)))

(defcustom orgqda-helm-tags-completion t
  "Whether to use the custom ‘orgqda-helm-tags-mode’  in ‘orgqda-mode’.

If not set through customize, set it through calling
‘orgqda-helm-tags-activate-completion’ as:
\(orgqda-helm-tags-activate-completion 'orgqda-helm-tags-completion VALUE\)"
  :group 'orgqda
  :type 'boolean
  :set #'orgqda-helm-tags-activate-completion
  :initialize #'custom-initialize-reset)

(defcustom orgqda-helm-tags-sort orgqda-default-sort-order
  "Sorting scheme used in ‘orgqda-helm-set-tags’."
  :group 'orgqda
  :type '(choice (const :tag "By count, decreasing" count-decreasing)
                 (const :tag "By count, increasing" count-increasing)
                 (const :tag "A-Z" a-z)
                 (const :tag "Z-A" z-a))
  :safe #'orgqda-sort-arg-p)

(defcustom orgqda-helm-tags-include-excluded nil
  "If non-nil, include tags listed in ‘orgqda-exclude-tags’ for completion."
  :group 'orgqda
  :type 'boolean)

(defcustom orgqda-helm-tags-include-persistent nil
  "If non-nil, include tags in ‘org-tag-persistent-alist’ for completion."
  :group 'orgqda
  :type 'boolean)

;;;; Variables
(defvar orgqda-helm-tags-history)

(defvar orgqda-helm-tags-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-comp-read-map)
    (define-key map (kbd "C-c C-s") #'orgqda-helm-tags-resort-comp-list)
    map))

(defvar orgqda-helm-tags--coll-buffer nil)
(defvar orgqda-helm-tags--comp-list nil)
(defvar orgqda-helm-tags--current-tags nil)

(defvar orgqda-helm-tags-mode-map nil)
(unless orgqda-helm-tags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-set-tags-command] #'orgqda-helm-tags-set-tags)
    (setq orgqda-helm-tags-mode-map map)))

;;;; Helm sources
(defvar orgqda-helm-tags-source
  (helm-build-sync-source "Orgqda select tags (C-RET finishes):"
    :history  'orgqda-helm-tags-history
    :fuzzy-match t
    :match-on-real t
    :keymap orgqda-helm-tags-map
    :action  '(("Set tags to" . (lambda (_c) (helm-marked-candidates)))
               ("Delete marked tags" . orgqda-helm-tags-delete-tag))
    :persistent-action #'orgqda-helm-tags-display-tagged
    :update #'orgqda-helm-tags--set-comp-list
    :mode-line #'orgqda-helm-tags--modeline
    :candidates 'orgqda-helm-tags--comp-list))

(defvar orgqda-helm-tags-new-tag-source
  (helm-build-dummy-source "Add new tag"
    :keymap orgqda-helm-tags-map
    :action '(("Insert new" . (lambda (c) (list c))))))

;;;; orgqda-helm-set-tags, main entry point
;;;###autoload
(defun orgqda-helm-tags-set-tags (&optional arg)
  "Use helm-completion to select multiple tags to add to heading.
Continues completing until exited with C-RET, M-RET or C-g.
Prefix ARG uses ordinary org tag insertion."
  (interactive "P")
  (if arg
      (org-set-tags-command)
    (if (org-at-heading-p)
        (org-set-tags (orgqda-helm-tags--get-tags))
      (save-excursion
        (org-back-to-heading t)
        (org-set-tags (orgqda-helm-tags--get-tags))))))

(defun orgqda-helm-tags--get-tags ()
  "Get list of new tags via helm completion."
  (let* ((helm-exit-status 0)
         (helm-truncate-lines t)
         (orgqda-helm-tags--current-tags (nreverse (org-get-tags nil t)))
         ;; sort can be changed in helm session, keep that local
         (orgqda-helm-tags-sort orgqda-helm-tags-sort))
    (orgqda-helm-tags--set-comp-list)
    (cl-remove-duplicates
     (cl-loop with newtags
              until (or (eq helm-exit-status 1) (equal "" newtags))
              do (setq newtags
                       (helm :sources
                             (list orgqda-helm-tags-source
                                   orgqda-helm-tags-new-tag-source)
                             :candidate-number-limit 99999
                             :buffer "*helm orgqda tags*"))
              when (listp newtags)
              do (dolist (tag newtags)
                   (push tag orgqda-helm-tags--current-tags)
                   (orgqda-helm-tags--update-tag-in-complist tag))
              finally return (nreverse orgqda-helm-tags--current-tags))
     :test 'string=)))

;; could be requested by capture templates
;;;###autoload
(defun orgqda-helm-tags-get-tagstring-for-capture ()
  "Return a string of tags with completion for the current file captured to."
  (let ((orgqda-collect-from-all-files t)
        (orgqda-tag-files (list (buffer-file-name
                                 (org-capture-get :buffer)))))
    (concat ":" (mapconcat #'identity (orgqda-helm-tags--get-tags) ":") ":")))

;;;; Minor mode definition
;;;###autoload
(define-minor-mode orgqda-helm-tags-mode "Minor mode for using ‘orqda-helm-tags-completion’

\\{orgqda-helm-tags-mode-map}"
  :keymap orgqda-helm-tags-mode-map)

(defun orgqda-helm-tags-mode-activate-in-hook ()
  "Activate ‘orgqda-helm-tags-mode’ if we are in ‘orgqda-mode’."
  (if orgqda-mode
      (orgqda-helm-tags-mode)
    (orgqda-helm-tags-mode -1)))

;;;; Actions in helm
(defun orgqda-helm-tags-display-tagged (tag)
  "Show occurences of currently selected TAG.
Calls ‘orgqda-collect-tagged’."
  (with-helm-current-buffer
    ;; should ideally be killed after switching but how?
    (if (buffer-live-p orgqda-helm-tags--coll-buffer)
        (with-current-buffer orgqda-helm-tags--coll-buffer
          (read-only-mode -1)
          (erase-buffer))
      (setq orgqda-helm-tags--coll-buffer
            (generate-new-buffer "orgqda-helm-tags-view")))
    (switch-to-buffer (orgqda-collect-tagged tag 1 orgqda-helm-tags--coll-buffer t))
    (bury-buffer orgqda-helm-tags--coll-buffer)))

(defun orgqda-helm-tags-delete-tag (_c)
  "Deletes selected or marked tags at entry."
  (let ((delete-tags
         (cl-intersection orgqda-helm-tags--current-tags
                          (helm-marked-candidates)
                          :test 'string=)))
    (setq orgqda-helm-tags--current-tags
          (cl-nset-difference orgqda-helm-tags--current-tags delete-tags
                              :test 'string=))
    (dolist (y delete-tags)
      (when-let ((n (cl-position ; otherwise just fail
                     y orgqda-helm-tags--comp-list
                     :test (lambda (a b) (string= a (cdr b))))))
        (setq orgqda-helm-tags--comp-list
              (nconc (cl-subseq orgqda-helm-tags--comp-list 0 n)
                     ;; this will be in the end, no good way of
                     ;; "resorting" it correctly
                     (orgqda-helm-tags--reformat-deleted-list-item
                      (nth n orgqda-helm-tags--comp-list))
                     (cl-subseq orgqda-helm-tags--comp-list (1+ n)))))))
  ;; return empty list to continue loop
  ())

(defun orgqda-helm-tags-resort-comp-list ()
  "Resort the current completion list."
  (interactive)
  (setq orgqda-helm-tags-sort
        (car (nth (mod (1+ (cl-position orgqda-helm-tags-sort orgqda-sort-args :key #'car))
                  (length orgqda-sort-args))
             orgqda-sort-args)))
  (helm-force-update))

;;;; Internal functions
;;;;; helm stuff
(defun orgqda-helm-tags--modeline ()
  "Modeline display for helm completsion in ‘orgqda-helm-tags-set-tags’."
  (list "Tags"
        (concat
         "sort:"
         (symbol-name orgqda-helm-tags-sort)
         " \\<orgqda-helm-tags-map>\\[helm-cr-empty-string],\\<global-map>\\[keyboard-quit]:Finish "
         "\\<helm-map> \\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/ f1/f2/f-n:NthAct")))

;;;;; Functions for initializing (or reloading) tag list
(defun orgqda-helm-tags--set-comp-list ()
  "Load tags and set ‘orgqda-helm-tags--comp-list’."
  (let* (cllast
         (taglist
          (if helm-alive-p
              (with-helm-current-buffer (orgqda-helm-tags--get-tags-list))
            (orgqda-helm-tags--get-tags-list)))
         (width (or (cl-loop for tag in taglist
                             maximize (length (car tag))) 0))
         (width (max width
                     (if orgqda-helm-tags-include-persistent
                         (cl-loop for (x . y) in org-tag-persistent-alist
                                  when (stringp x)
                                  maximize (length x))
                       0)))
         (clfirst (cl-loop for tag in taglist
                           if (cl-member tag orgqda-helm-tags--current-tags
                                         :test
                                         (lambda (a b) (string= (car a) b)))
                           do (push (orgqda-helm-tags--format-list-item tag width t) cllast)
                           else collect (orgqda-helm-tags--format-list-item tag width))))
    (setq orgqda-helm-tags--comp-list
          (nconc
           clfirst
           (when orgqda-helm-tags-include-persistent
             (cl-loop for (x . y) in org-tag-persistent-alist
                      when (and (stringp x)
                                (not (cl-member
                                      x taglist
                                      :test (lambda (a b) (string= a (car b))))))
                      collect (orgqda-helm-tags--format-list-item
                               (list x 0 "from ‘org-tag-persistent-alist’")
                               width)))
           (nreverse cllast)))))

(defun orgqda-helm-tags--get-tags-list ()
  "Gets list of tags with count and (possible) coding info."
  (let ((info (orgqda-helm-tags--get-codebook-info)))
    (cl-loop for x in (orgqda--get-tags-alist
                       orgqda-helm-tags-sort
                       (if orgqda-helm-tags-include-excluded
                           '("")
                         orgqda-exclude-tags))
             collect (list (car x)
                           (cdr x)
                           (assoc-default (car x)
                                          info)))))

(defun orgqda-helm-tags--get-codebook-info ()
  "Return alist of tag names and coding info.

Coding info is the first line of the matching line for the tag in
‘orgqda-codebook-file’"
  (when orgqda-codebook-file
    (org-map-entries
     #'orgqda-helm-tags--get-tag-info
     t
     (list orgqda-codebook-file))))

(defun orgqda-helm-tags--get-tag-info ()
  "Get tag info for current entry in codebook file."
  (when (and (search-forward-regexp
              org-link-bracket-re (point-at-eol) t)
             (save-match-data (string-match "^otag:" (match-string 1))))
    (let ((tag (cadr (split-string (match-string-no-properties 3) ":")))
          (text (substring-no-properties
                 (org-agenda-get-some-entry-text (point-marker) 1))))
      (when (and tag (not (string-blank-p text)))
        (cons tag text)))))

(defmacro orgqda-helm-tags-propertize-if (condition string &rest properties)
  "Return STRING with PROPERTIES if CONDITION is non-nil.
Otherwise return STRING."
  (declare (indent 1) (debug t))
  `(if ,condition
       (propertize ,string ,@properties)
     ,string))

(defun orgqda-helm-tags--format-list-item (x width &optional incurrent?)
  "Format item for completion list.
X is the item. WIDTH the display width to use. INCURRENT? a flag
if the tag is in current list for the entry."
  (cons
   (orgqda-helm-tags-propertize-if incurrent?
     (format
      (format "%%-%ds %%5s %%s" width)
      (if (and orgqda-helm-tags-include-excluded (member (car x) orgqda-exclude-tags))
          (propertize (car x) 'face 'font-lock-comment-face)
        (car x))
      (propertize (format "%d" (cadr x))
                  'face 'font-lock-function-name-face)
      (if-let ((info (nth 2 x)))
          (propertize info 'face 'shadow)
        ""))
     'face '(font-lock-comment-face (:strike-through t)))
   (car x)))

;;;;; Functions for rewriting (reformatting) tag list after tags are added/removed
(defun orgqda-helm-tags--update-tag-in-complist (tag)
  "Update display of TAG in completion list."
  (setq orgqda-helm-tags--comp-list
        (if-let ((n (cl-position
                     tag orgqda-helm-tags--comp-list
                     :test (lambda (a b) (string= a (cdr b))))))
            (nconc (cl-subseq orgqda-helm-tags--comp-list 0 n)
                   (cl-subseq orgqda-helm-tags--comp-list (1+ n))
                   (orgqda-helm-tags--reformat-added-list-item
                    (nth n orgqda-helm-tags--comp-list)))
          (nconc orgqda-helm-tags--comp-list
                 (orgqda-helm-tags--reformat-added-list-item (cons tag tag))))))

(defun orgqda-helm-tags--reformat-added-list-item (li)
  "Reformat display of a just added completion list item LI."
  (list (cons (propertize (concat  "+ " (car li)) 'face 'bold) (cdr li))))

(defun orgqda-helm-tags--reformat-deleted-list-item (li)
  "Reformat display of a just deleted completion list item LI."
  (list (cons (propertize (replace-regexp-in-string "^\\+" "" (car li))
                          'face nil)
              (cdr li))))

(provide 'orgqda-helm-tags)

;;; orgqda-helm-tags.el ends here
