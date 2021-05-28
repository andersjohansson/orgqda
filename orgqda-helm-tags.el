;;; orgqda-helm-tags.el --- New completion for orgqda-mode tags -*- lexical-binding: t -*-

;; Copyright (C) 2017-2020 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2017-02-06
;; Modified: 2021-05-27
;; Package-Requires: ((emacs "25.1") (org "9.3") (orgqda "0.2") (helm "3.6"))
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
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-helm-tags-include-persistent nil
  "If non-nil, include tags in ‘org-tag-persistent-alist’ for completion."
  :group 'orgqda
  :type 'boolean)

(defcustom orgqda-helm-tags-exclude-tags nil
  "List of tags to exclude from completion in ‘orgqda-helm-set-tags’.

This is an alternative to set an additional list of tags that should not clutter the completion, apart from ‘orgqda-exclude-tags’. See also ‘orgqda-helm-tags-include-excluded’."
  :group 'orgqda
  :type '(repeat string)
  :safe #'orgqda--list-of-strings-p)

(defcustom orgqda-helm-tags-fuzzy-match t
  "If non-nil, use fuzzy matching in ‘orgqda-helm-tags-set-tags’."
  :group 'orgqda
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-helm-tags-match nil
  "What should match when completing tags with ‘orgqda-helm-set-tags’.

Nil means match only tag name; ‘count’, means match on count,
‘description’ means to match also on codebook description, and
‘all’ means to match on full string."
  :group 'orgqda
  :type '(choice (const :tag "Match only tag name" nil)
                 (const :tag "Match tag name and count" 'count)
                 (const :tag "Match tag name and codebook description" 'description)
                 (const :tag "Match tag name, count, and codebook description" 'all))
  :safe 'symbolp)

(defcustom orgqda-helm-tags-display-align nil
  "If non-nil, align tag counts using display properties instead of faces.
Useful if a variable-pitch face is used in helm."
  :group 'orgqda
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-helm-tags-fuzzy-sort-fn #'helm-fuzzy-matching-sort-fn-preserve-ties-order
  "Function used for ‘helm-fuzzy-sort-fn’ in ‘orgqda-helm-tags-set-tags’."
  :group 'orgqda
  :type 'function)

;;;; Variables
(defvar orgqda-helm-tags-history)

(defvar orgqda-helm-tags-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-comp-read-map)
    (define-key map (kbd "C-c C-s") #'orgqda-helm-tags-resort-comp-list)
    (define-key map (kbd "C-i") #'orgqda-helm-tags-tab)
    (define-key map (kbd "C-l") #'orgqda-helm-tags-hierarchy-up)
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
(defun orgqda-helm-tags--build-source ()
  "Return a suitable helm-source for ‘orgqda-helm-tags-source’."
  (helm-build-sync-source "Orgqda select tags (C-RET finishes):"
    :history  'orgqda-helm-tags-history
    :fuzzy-match orgqda-helm-tags-fuzzy-match
    :keymap orgqda-helm-tags-map
    :action  '(("Set tags to" . (lambda (_c) (helm-marked-candidates)))
               ("Delete marked tags" . orgqda-helm-tags-delete-tag)
               ("Browse tag in codebook" . orgqda-helm-tags-browse-in-codebook))
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
         (orgqda-helm-tags-sort orgqda-helm-tags-sort)
         (sources (list (orgqda-helm-tags--build-source) orgqda-helm-tags-new-tag-source))
         (helm-fuzzy-sort-fn orgqda-helm-tags-fuzzy-sort-fn))
    (orgqda-helm-tags--set-comp-list)
    (cl-remove-duplicates
     (cl-loop with newtags
              until (or (eq helm-exit-status 1) (equal "" newtags))
              do (setq newtags
                       (helm :sources sources
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
(define-minor-mode orgqda-helm-tags-mode
  "Minor mode for using ‘orqda-helm-tags-completion’.

\\{orgqda-helm-tags-mode-map}"
  :keymap orgqda-helm-tags-mode-map)

(defun orgqda-helm-tags-mode-activate-in-hook ()
  "Activate ‘orgqda-helm-tags-mode’ if we are in ‘orgqda-mode’."
  (if orgqda-mode
      (orgqda-helm-tags-mode)
    (orgqda-helm-tags-mode -1)))

;;;; Actions in helm
(defun orgqda-helm-tags-tab ()
  "Complete tag prefixes or choose action."
  (interactive)
  (let ((sel (helm-get-selection)))
    (if (and
         (string-prefix-p helm-pattern sel)
         (string-match orgqda-hierarchy-delimiter sel (length helm-pattern)))
        (with-selected-window (or (active-minibuffer-window)
                                  (minibuffer-window))
          (delete-minibuffer-contents)
          (insert (substring-no-properties sel 0 (1+ (match-beginning 0)))))
      (helm-select-action))))

(defun orgqda-helm-tags-hierarchy-up ()
  "Remove one level of hierarchy from current pattern."
  (interactive)
  (with-selected-window (or (active-minibuffer-window)
                            (minibuffer-window))
    (zap-up-to-char -1 (string-to-char orgqda-hierarchy-delimiter))))

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

(defun orgqda-helm-tags-browse-in-codebook (tag)
  "Browse selected TAG in codebook."
  (with-helm-current-buffer
    (condition-case nil
        (progn
          (if (and orgqda-codebook-file (file-readable-p orgqda-codebook-file))
              (let ((buf (find-file-noselect orgqda-codebook-file)))
                (with-current-buffer buf
                  (orgqda--find-otag-link tag t))
                ;; if search worked, pop-to-buffer and pass "" to quit the helm loop
                (switch-to-buffer-other-window buf)
                "")
            (message "No codebook")
            ;; continue loop
            '())
          "")
      ;; if search failed, continue loop
      (search-failed (progn (message "Search in codebook failed")
                            '())))))

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
  "Get list of tags with count and (possible) coding info."
  (let ((info (orgqda-helm-tags--get-codebook-info))
        (taglist (orgqda--get-tags-alist orgqda-helm-tags-sort
                                         (append
                                          (if orgqda-helm-tags-include-excluded
                                              ;; non-nil for overriding the default
                                              '("")
                                            orgqda-exclude-tags)
                                          orgqda-helm-tags-exclude-tags))))
    (append
     (cl-loop for (tag . count) in taglist
              collect (list tag count (nth 2 (assoc-string tag info))))
     ;; Tags in codebook with no count
     (cl-set-difference info taglist :test #'string= :key #'car))))

(defun orgqda-helm-tags--get-codebook-info ()
  "Return alist of tag names and coding info.

Coding info is the first line of the matching line for the tag in
‘orgqda-codebook-file’"
  (when orgqda-codebook-file
    (cl-delete-if
     #'null
     (org-map-entries
      #'orgqda-helm-tags--get-tag-info
      t
      (list orgqda-codebook-file)))))

(defun orgqda-helm-tags--get-tag-info ()
  "Get tag info for current entry in codebook file."
  (when (and (search-forward-regexp
              org-link-bracket-re (point-at-eol) t)
             (save-match-data (string-match "^otag:" (match-string 1))))
    (let ((tag (nth 2 (split-string (match-string-no-properties 1) ":")))
          (text (substring-no-properties
                 (org-agenda-get-some-entry-text (point-marker) 1))))
      (when (and tag (not (eq ?{ (string-to-char tag))))
        (list tag 0 text)))))

(defmacro orgqda-helm-tags-propertize-if (condition string &rest properties)
  "Return STRING with PROPERTIES if CONDITION is non-nil.
Otherwise return STRING."
  (declare (indent 1) (debug t))
  `(if ,condition
       (propertize ,string ,@properties)
     ,string))

(cl-defun orgqda-helm-tags--format-list-item ((tag count info) width &optional incurrent?)
  "Format item for completion list.
TAG, COUNT, and INFO is the information for the tag. WIDTH the
display width to use. INCURRENT? a flag for whether the tag is in
the current list for the entry."
  (cons
   (orgqda-helm-tags-propertize-if incurrent?
     (format
      (if orgqda-helm-tags-display-align
          (concat "%s"
                  (propertize " " 'display `(space :align-to ,width))
                  "%s"
                  (propertize " " 'display `(space :align-to ,(+ 5 width)))
                  "%s")
        (format "%%-%ds %%5s %%s" width))
      (if (and orgqda-helm-tags-include-excluded (member tag orgqda-exclude-tags))
          (propertize tag 'face 'font-lock-comment-face)
        tag)
      (let* ((cs (format "%d" count)))
        (propertize
         (if (member orgqda-helm-tags-match '(count all))
             cs
           (propertize (make-string (length cs) ?\s) 'display cs))
         'face 'font-lock-function-name-face))
      (if info (propertize
                (if (member orgqda-helm-tags-match '(description all))
                    info
                  (propertize " " 'display info))
                'face 'shadow)
        ""))
     'face '(font-lock-comment-face (:strike-through t)))
   tag))

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
