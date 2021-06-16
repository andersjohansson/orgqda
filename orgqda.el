;;; orgqda.el --- Qualitative data analysis using org-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.3
;; Created: 2014-10-12
;; Modified: 2021-06-16
;; Package-Requires: ((emacs "25.1") (org "9.3") (hierarchy "0.6.0"))
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
;; orgqda defines a minor mode and several commands for making coding
;; and collection of coded snippets possible in text written in
;; org-mode. It works in a simple (and perhaps stupid) way by viewing
;; org-mode tags added to degenerate inlinetasks as applying to the
;; preceding paragraph.

;;; Code:

(require 'bookmark)
(require 'org)
(require 'org-inlinetask)
(require 'org-element)
(require 'org-agenda)
(require 'cl-lib)
(require 'subr-x) ;if-let, thread-last
(require 'hierarchy)

;;;; Variables
(defgroup orgqda nil
  "Customizations for orgqda-mode."
  :group 'org)

;;;###autoload
(defcustom orgqda-csv-dir "~"
  "Directory for saving csv-files."
  :type 'directory
  :safe 'file-directory-p)

(defcustom orgqda-collect-coverage nil
  "Display coverage (percentage of file the coded segment makes up) in ‘orgqda’."
  :type 'boolean)

(defcustom orgqda-tag-collect-extra-info nil
  "Possibly adds extra info to extracted tagged parts.
An alist where keys are regexps to be matched against ‘buffer-name’
and values are Lisp forms that are evaluated to get extra info.

An example that adds a possible parent heading on level 4 for
buffers with names containing 'fieldnotes':
'((\"fieldnotes\" . (format \" (from: %s)\" (orgqda-get-parent-hl 4))))"
  :type '(alist :key-type regexp :value-type sexp))

(defcustom orgqda-collect-from-all-files t
  "Whether to collect tags from all files defined in ‘orgqda-tag-files’.
This applies to the tag listing and collection commands in orgqda."
  :type 'boolean)

(defcustom orgqda-respect-restriction-for-single-file t
  "If only collecting tags from a single file, collect in narrowed buffer."
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-tag-collect-extra-info-csv nil
  "Like ‘orgqda-tag-collect-extra-info’ for the first extra-field in csv-export."
  :type '(alist :key-type regexp :value-type sexp))

(defcustom orgqda-tag-collect-extra-info2-csv nil
  "Like ‘orgqda-tag-collect-extra-info’ for the second extra-field in csv-export."
  :type '(alist :key-type regexp :value-type sexp))

(defcustom orgqda-convert-csv-to-encoding nil
  "Encoding to use for saved csv files.
By default csv files are saved with the encoding used by Emacs or
the files. Setting this to a symbol that represents another
encoding will ensure all characters are of this encoding and
replace those that are not by ? in saved csv files. t is a
shortcut for \"iso-8859-1\""
  :type (append
         '(choice
           (const nil :tag "don’t convert")
           (const t :tag "iso-8859-1"))
         (cl-loop for cs in coding-system-list
                  collect (list 'const cs))))

(defcustom orgqda-exclude-tags nil
  "Tags to exclude when listing coding tags.
Applies to ‘orgqda-list-tags’ and ‘orgqda-list-tags-full’."
  :type '(choice
          (const :tag "None" nil)
          (repeat :tag "List of tags" string))
  :safe #'orgqda--list-of-strings-p)

(defcustom orgqda-hierarchy-delimiter "_"
  "The character to use for delimiting a hierarchy within tags.
One of: _@#%, or nil to disable hierarchical grouping."
  :type '(choice (const "_")
                 (const "@")
                 (const "#")
                 (const "%")
                 (const :tag "Disable" nil)))

(defcustom orgqda-exclude-empty-file-trees t
  "When non-nil, excludes listing files without matches for current tag."
  :type 'boolean)

(defcustom orgqda-keep-tags-sorted nil
  "If non-nil, keep the taglist in the entry sorted in ‘orgqda-mode’.
If non-nil, should be a sorting scheme from ‘orgqda-sort-args’.

You could directly set ‘org-tags-sort-function’, but this is for
using it locally when using ‘orgqda-mode’ and with simple options
corresponding to orgqda sorting.

Most stable and useful is probably to sort alphabetically, using
‘a-z’ or ‘z-a’. Sorting by count means sorting by the current
“popularity” of tags across the orgqda collection and won’t be
updated when tags are changed in other places than this headline."
  :type '(choice (const :tag "Don’t sort" nil)
                 (const :tag "By count, decreasing" count-decreasing)
                 (const :tag "By count, increasing" count-increasing)
                 (const :tag "A-Z" a-z)
                 (const :tag "Z-A" z-a))
  :safe (lambda (x) (or (null x) (orgqda-sort-arg-p x))))

(defcustom orgqda-default-sort-order 'count-decreasing
  "Default order for sorting when listing tags."
  :type '(choice (const :tag "By count, decreasing" count-decreasing)
                 (const :tag "By count, increasing" count-increasing)
                 (const :tag "A-Z" a-z)
                 (const :tag "Z-A" z-a))
  :safe #'orgqda-sort-arg-p)

(defcustom orgqda-only-count-matching nil
  "When non-nil, only entries with matching tags are counted and collected.
Each item in this list is a regex matched against tags with
‘string-match-p’.

This is useful for temporarily working on a subset of the project, defined
via a certain tag or tag-prefix."
  :type '(repeat string)
  :safe #'orgqda--list-of-strings-p)

(defcustom orgqda-use-tag-inheritance nil
  "When non-nil, count entries using tag inheritance.

Useful together with ‘orgqda-only-count-matching’, if you for
example want to analyse data for a specific kind of entry (using
a tag) but those entries should be considered tagged with their
parents’ tags."
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-tagcount-show-files nil
  "When non-nil, show the counts for individual files when listing tags.
If you for example have a group of interview transcripts in
different files this gives a good overview of which interview
each tag was used in.

One of ‘all’, for displaying it on all nodes in the tag listing,
‘not-parents’ for only showing the overall count for parents in a
hierarchical listing, or nil, for disabling it."
  :type '(choice (const :tag "For all" all)
                 (const :tag "Not parents" not-parents)
                 (const :tag "Disable" nil))
  :safe #'symbolp)

(defcustom orgqda-tagcount-files-transform-functions '(file-name-base orgqda--file-name-remove-parentheses)
  "List of functions to apply to displayed file names for tagcounts.
Transforms the file name with each function in list order.

It is important that ‘orgqda--file-name-remove-parentheses’ is in
the list, otherwise updating codebook tag lists will fail."
  :type '(repeat function))

(defcustom orgqda-taglink-include-filename t
  "Style for taglinks (otag) when listing tags.
Non-nil means include the filename for the file where searching
started. When the filename is included, we can be more sure that
following the link looks in the right project."
  :type 'boolean
  :safe #'booleanp)

(defun orgqda--file-name-remove-parentheses (filename)
  "Return FILENAME with parentheses removed."
  (string-replace
   ")" ""
   (string-replace "(" "" filename)))

;;;###autoload
(defvar-local orgqda-tag-files nil
  "Extra files from which tags should be fetched for completion.
A list of files and directories, or the name of a file
containing such a list. Relative paths in such a file are read as
relative to the file itself.

For directories, all .org-files (matched by
‘org-agenda-file-regexp’) are added.

Usually set by the user as a file or dir local variable.")
;;;###autoload
(put 'orgqda-tag-files 'safe-local-variable
     #'orgqda--string-or-list-of-strings-p)

;;;###autoload
(defvar-local orgqda-codebook-file nil
  "A file that is used as a codebook.
Including lists of tags (otag-links) This file will be updated
when tags are renamed.

Usually set by the user as a file or dir local variable.")
;;;###autoload
(put 'orgqda-codebook-file 'safe-local-variable
     #'stringp)

;;;###autoload
(defun orgqda--string-or-list-of-strings-p (arg)
  "Return t if ARG is a string or a list of strings."
  (or (stringp arg)
      (orgqda--list-of-strings-p arg)))

;;;###autoload
(defun orgqda--list-of-strings-p (arg)
  "Return t if ARG is a list of strings."
  (and (listp arg) (cl-every 'stringp arg)))

(defvar-local orgqda--originating-buffer nil
  "Buffer from which the current orgqda list or collection was invoked.")
(defvar-local orgqda--taglist-sort nil
  "Sorting of current taglist buffer.")
(defvar-local orgqda--taglist-full nil
  "Whether current taglist buffer includes extracts.")
(defvar-local orgqda--old-org-current-tag-alist nil
  "Saved state of ‘org-current-tag-alist’ before enabling ‘orgqda-mode’.")

(defconst orgqda-sort-args
  '((count-decreasing . orgqda--hierarchy-count-greater-p)
    (a-z . orgqda--string-lessp)
    (z-a . orgqda--string-greaterp)
    (count-increasing . orgqda--hierarchy-count-less-p))
  "Alist mapping symbol keys for sorting to comparison functions.
The order is the order used when cycling sorting in
‘orgqda-helm-tags-set-tags’.")

;;;###autoload
(defun orgqda-sort-arg-p (arg)
  "Non-nil if ARG is in ‘orgqda-sort-args’."
  (cl-member arg orgqda-sort-args :key #'car))

;;;; KEYBINDINGS
;;;###autoload
(defvar orgqda-mode-map nil
  "Local keymap for orgqda-mode.")
;;;###autoload
(unless orgqda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x m") #'orgqda-insert-inlinetask)
    (define-key map (kbd "C-c C-x n") #'orgqda-insert-inlinetask-coding)
    (setq orgqda-mode-map map)))

;;;###autoload
(defvar orgqda-list-mode-map nil
  "Local keymap for ‘orgqda-list-mode’.")
;;;###autoload
(unless orgqda-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<drag-mouse-1>") #'orgqda-drag-merge-tags)
    (define-key map (kbd "R") #'orgqda-rename-tag)
    (define-key map (kbd "P") #'orgqda-prefix-tag)
    (define-key map (kbd "s") #'orgqda-sort-taglist)
    (define-key map (kbd "S") #'orgqda-sort-taglist-buffer)
    (define-key map (kbd "g") #'orgqda-revert-taglist)
    (define-key map (kbd "q") #'quit-window)
    (setq orgqda-list-mode-map map)))

(defvar orgqda-codebook-mode-map nil
  "Local keymap for ‘orgqda-codebook-mode’.")

(unless orgqda-codebook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<drag-mouse-1>") #'orgqda-drag-merge-tags)
    (define-key map (kbd "C-c (") orgqda-list-mode-map)
    (define-key map [remap org-refile] #'orgqda-refile-and-merge-tags)
    (setq orgqda-codebook-mode-map map)))

;;;; Macros and defsubst
(defmacro orgqda--temp-work (widened? &rest body)
  "Macro for working on BODY temporarily, possibly in WIDENED buffer.
Shortcut for (‘save-excursion’ (‘save-restriction’ (‘widen’) (‘goto-char’) (‘point-min’)))."
  (declare (indent 1) (debug t))
  `(save-excursion
     (save-restriction
       (when ,widened? (widen))
       (goto-char (point-min))
       ,@body)))

(defmacro orgqda--with-current-buffer-if (buffer &rest body)
  "Execute BODY in BUFFER if it exists, otherwise just execute BODY.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  `(if (and ,buffer (buffer-live-p ,buffer))
       (with-current-buffer ,buffer ,@body)
     ,@body))

(defmacro orgqda--inhibit-org-startups (&rest body)
  "Execute BODY while inhibiting mode hooks and org-startup.
Inhibits hooks for ‘text-mode’, ‘outline-mode’ and ‘org-mode’"
  (declare (debug t))
  `(let ((text-mode-hook nil)
         (outline-mode-hook nil)
         (org-mode-hook nil)
         (org-agenda-inhibit-startup t)
         (org-inhibit-startup t))
     ,@body))

(defmacro orgqda--with-many-files (manyfiles &rest body)
  "Execute BODY efficiently in each file of MANYFILES."
  (declare (indent 1))
  `(orgqda--inhibit-org-startups
    ;; this basically copies the essentials for running on a set of
    ;; files from ‘org-map-entries’
    (let ((org-agenda-skip-archived-trees t)
          (org-agenda-skip-comment-trees t)
          org-todo-keywords-for-agenda
	      org-done-keywords-for-agenda
	      org-todo-keyword-alist-for-agenda
	      org-tag-alist-for-agenda)
      (org-agenda-prepare-buffers ,manyfiles)
      (dolist (file ,manyfiles)
        (with-current-buffer (org-find-base-buffer-visiting file)
          ,@body)))))

(defsubst orgqda--string-or-empty (string &optional prefix suffix)
  "Return STRING with PREFIX and SUFFIX iff STRING is a non-blank string.
Else return empty string."
  (cond ((and (stringp string)
              (string-match-p "[^[:blank:]]" string))
         (concat prefix string suffix))
        (t "")))

;;;; Minor mode definitions
;;;###autoload
(define-minor-mode orgqda-mode
  "Toggle orgqda mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, ‘toggle’ toggles the state.

Minor mode for qualitative coding of text material and extraction
of codes.

Enables tag completion with tags from all files defined in ‘orgqda-tag-files’

Relevant commands not bound to any keys are:
‘orgqda-list-tags’, ‘orgqda-list-tags-full’ and ‘orgqda-collect-tagged’.

CSV files can be exported as well with
‘orgqda-collect-tagged-csv’, ‘orgqda-collect-tagged-csv-save’,
and ‘orgqda-collect-tagged-csv-save-all’. Be sure to customize
‘orgqda-csv-dir’ first.

\\{orgqda-mode-map}"
  :lighter " QDA"
  :keymap orgqda-mode-map
  :group 'orgqda
  (if orgqda-mode
      (progn
        (when orgqda-tag-files
          (setq-local org-complete-tags-always-offer-all-agenda-tags t))
        (setq orgqda--old-org-current-tag-alist org-current-tag-alist
              org-current-tag-alist nil)
        (setq-local org-open-at-point-functions '(orgqda-collect-tags-at-point))
        (setq-local org-tags-sort-function
                    (or (alist-get orgqda-keep-tags-sorted orgqda-sort-args)
                        org-tags-sort-function)))
    (kill-local-variable 'org-complete-tags-always-offer-all-agenda-tags)
    (kill-local-variable 'org-open-at-point-functions)
    (kill-local-variable 'org-tags-sort-function)
    (setq org-current-tag-alist orgqda--old-org-current-tag-alist)))

;;;###autoload
(define-minor-mode orgqda-list-mode
  "Mode for displaying lists of tags in orgqda.

\\{orgqda-list-mode-map}"
  :keymap orgqda-list-mode-map
  :lighter " QDAl")

(define-minor-mode orgqda-codebook-mode
  "Mode for updating and sorting lists of tags in a codebook file.

\\{orgqda-codebook-mode-map}"
  :keymap orgqda-codebook-mode-map
  :lighter "QDAc")

;;;; Interactive commands
;;;;; Commands for inserting
(autoload 'org-inlinetask-in-task-p "org-inlinetask")
(declare-function orgqda-helm-tags-set-tags "orgqda-helm-tags")

;;;###autoload
(defun orgqda-insert-inlinetask (&optional arg title coding)
  "Insert a degenerate inlinetask after current paragraph.
With prefix ARG, after current line.
TITLE is a string defining a title for the inlinetask and CODING
if non-nil additionally calls `org-set-tags-command`."
  (interactive "P")
  (let ((cg (prepare-change-group))
        (oldpoint (point))
        (inhibit-read-only t))
    (activate-change-group cg)
    (unless (org-inlinetask-in-task-p) ; add nothing if already in task
      (unless (and (bolp) (eolp)) ; only move if inside a line of text
        (if arg (forward-line) (forward-paragraph)))
      (unless (org-inlinetask-in-task-p) ; create inlinetask if not present
        (unless (and (bolp) (eolp)) (newline)) ;newline at e.g. eof
        (unless (and arg (bolp) (eolp)) (open-line 1)) ; open-line if at newline
        (insert (concat (make-string
                         (1+ org-inlinetask-min-level) 42) " " title))))
    (when (and coding (org-inlinetask-in-task-p))
      (if (bound-and-true-p orgqda-helm-tags-mode)
          (orgqda-helm-tags-set-tags)
        (org-set-tags-command)))
    ;; possibly cancel coding command
    (if (and coding
             (save-excursion
               (beginning-of-line)
               (and
                (looking-at org-complex-heading-regexp)
                (not (match-string 5)))))
        (progn
          (cancel-change-group cg)
          (goto-char oldpoint))
      (accept-change-group cg)
      (move-end-of-line nil))))

;;;###autoload
(defun orgqda-insert-inlinetask-coding (arg)
  "Call ‘orqda-insert-inlinetask’ with coding option and title \"∈\".
Prefix ARG is passed through."
  (interactive "P")
  (orgqda-insert-inlinetask arg "∈" t))

;;;;; Commands for listing tags

;;;###autoload
(defun orgqda-list-tags (&optional sort full buf noupdate roottext startprefix no-tag-files)
  "List all tags in this buffer and/or ‘orgqda-tag-files’, with counts.
Sorted by count, or alphabetically if optional (prefix) argument
SORT is non-nil.

For non-interactive calls: PREFIX can be given as a sort
symbol (see ‘orgqda--create-hierarchical-taglist’). FULL non-nil
means also insert extracted paragraphs for all tags. If a buffer
is provided in BUF overwrite this buffer with the list instead of
creating a new. The taglist is normally updated via
‘orgqda--create-hierarchical-taglist’, but this can be prevented
by giving a non-nil NOUPDATE. Then a special list can be used by
setting ‘orgqda--current-htl’ suitably. ROOTTEXT specifies the
text for the root node. STARTPREFIX, searches only tags under
this prefix. NO-TAG-FILES means only collect in current buffer,
ignoring any setting of ‘orgqda-tag-files’."
  (interactive "P")
  (let* ((origbuffer (current-buffer))
         (origfile (buffer-file-name))
         (ocm orgqda-only-count-matching)
         (include-filename orgqda-taglink-include-filename)
         (orgqda-tag-files
          (if no-tag-files nil orgqda-tag-files))
         ;; no need to show origin files if only one, or when
         ;; displaying full list
         (orgqda-tagcount-show-files
          (if (and orgqda-tag-files (not full))
              orgqda-tagcount-show-files
            nil))
         tagfiles)
    (unless noupdate
      (orgqda--create-hierarchical-taglist
       (cond
        ((symbolp sort) sort)
        (sort 'a-z)
        (t orgqda-default-sort-order)))
      ;; have to fetch the list again here for listing below, it is
      ;; done deep in the call of
      ;; ‘orgqda--create-hierarchical-taglist’ above
      (setq tagfiles (when orgqda-tag-files orgqda-collect-from-all-files (orgqda-tag-files))))
    (if buf
        (progn (pop-to-buffer buf)
               (setq buffer-read-only nil)
               (erase-buffer))
      (pop-to-buffer
       (generate-new-buffer "*orgqda-taglist*")))
    (if roottext
        (insert (format "* %s\n" roottext))
      (insert
       (concat "* Orgqda taglist "
               (when ocm
                 (format "(matching: %s) "
                         (mapconcat #'identity ocm "|")))
               (when startprefix
                 (format "(under prefix ~%s~) " startprefix))
               "generated from "
               (org-link-make-string
                (concat "file:" origfile)
                (buffer-name origbuffer))
               " at "
               (format-time-string "[%Y-%m-%d %a %H:%M]")
               "\n"
               (when tagfiles
                 (concat ":ORGQDA_TAG_FILES:\n"
                         (cl-loop for f in (sort tagfiles #'string-version-lessp)
                                  concat
                                  (concat "- " (org-link-make-string f (file-name-base f)) "\n"))
                         ":END:\n")))))
    (orgqda--insert-hierarchical-taglist full origbuffer (if include-filename origfile "") 1 startprefix)
    (goto-char (point-min))
    (org-mode) (view-mode) (orgqda-list-mode) (flyspell-mode -1)
    (setq ;; buffer-read-only t
     orgqda--originating-buffer origbuffer
     orgqda--taglist-sort sort
     orgqda--taglist-full full)))

;;;###autoload
(defun orgqda-list-tags-full (&optional sort buf)
  "List all tags including extracted parahraphs.

Sorted by count or alphabetically if optional (prefix) argument
SORT is non-nil. Two prefix arguments prompts for a tag prefix to
start from. BUF is passed on to ‘orgqda-list-tags’"
  (interactive "P")
  (orgqda-list-tags
   (equal '(4) sort)
   t buf nil nil
   (when (equal '(16) sort)
     (concat (orgqda--completing-read-prefix
              "Prefix to start from: ")
             "_"))))

(defun orgqda-revert-taglist ()
  "Reverts current ‘orgqda-list-mode’ buffer.
If not in ‘orgqda-list-mode’, calls
‘orgqda-update-taglist-general’."
  (interactive)
  (if (and orgqda-list-mode orgqda--originating-buffer)
      (let ((cb (current-buffer))
            (cts orgqda--taglist-sort)
            (ctf orgqda--taglist-full)
            (pos (point)))
        (setq buffer-read-only nil)
        (with-current-buffer orgqda--originating-buffer
          (orgqda-list-tags cts ctf cb))
        (goto-char pos))
    (orgqda-update-taglist-general)))


(defvar orgqda--current-sorting-args nil)

;;;###autoload
(defun orgqda-sort-taglist (&optional order non-recursive)
  "Sort current taglist using ‘org-sort-entries’.

Sorts current subtree and children, active region, or children of
first headline if before that.

Sorting is determined via ORDER which can be a/A/c/C for
alphabetical, alphabetical reversed, count decreasing, count
increasing, respectively.
With NON-RECURSIVE non-nil, only sorts
the direct descendants of current headline, and not their
children."
  (interactive "cSort order: a[lpha] c[count], A/C means reversed.")
  (let* ((inhibit-read-only t)
         (inhibit-message t)
         (order (or order ?c))
         (sortlist
          (list
           (cl-case order
             ((?a ?A a-z z-a)
              '(nil ?f orgqda--hl-get-count >))
             ((?c ?C count-decreasing count-increasing)
              '(nil ?a))
             (t (user-error "No correct order specified")))
           (cl-case order
             ((?a a-z) '(nil ?a))
             ((?A z-a) '(nil ?A))
             ((?c count-decreasing) '(nil ?f orgqda--hl-get-count >))
             ((?C count-increasing) '(nil ?f orgqda--hl-get-count <))
             (t (user-error "No correct order specified"))))))
    (if (region-active-p)
        ;; don’t do double sort here, too difficult keeping region
        (apply #'org-sort-entries (cadr sortlist))
      (when (org-before-first-heading-p)
        (outline-next-heading))
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (dolist (so sortlist)
        (let ((orgqda--current-sorting-args so))
          (if non-recursive
              (orgqda--sort-subtree)
            (org-map-entries #'orgqda--sort-subtree t 'tree))))))
  (when orgqda-list-mode
    (setq orgqda--taglist-sort
          (cl-case order
            (?a 'a-z) (?A 'z-a) (?c 'count-decreasing) (?C 'count-increasing)))))

;;;###autoload
(defun orgqda-sort-taglist-buffer (&optional order)
  "Sort the current taglist buffer.
calls ‘orgqda-sort-taglist’ for whole buffer.
Sort by ORDER."
  (interactive "cSort order: a[lpha] c[count], A/C means reversed.")
  (save-mark-and-excursion
    (while (org-up-heading-safe))
    (when (or (org-goto-sibling)
              (org-goto-sibling t))
      (goto-char (point-min))
      (set-mark (point-max)))
    (funcall #'orgqda-sort-taglist order)))


;;;;; Commands for collecting
;;;###autoload
(defun orgqda-collect-tagged (&optional match deeper-view buffer noswitch)
  "Collect all segments marked with tags matching MATCH.
Display them in custom buffer in other window.
In an interactive call, MATCH is prompted for.

For non-interactive use: DEEPER-VIEW (integer) adds visible
levels to folding with ‘org-content’. BUFFER specifies a buffer
to insert the collected tags in. NOSWITCH non-nil means no buffer
switching is done and view buffer just returned."
  (interactive)
  (let* ((matcher (orgqda--make-tags-matcher match))
         (mname (car matcher))
         (cont (orgqda--coll-tagged matcher 2))
         (oclevel
          (+ (if (and orgqda-collect-from-all-files orgqda-tag-files) 2 1)
             (or deeper-view 0)))
         (buffer (or buffer (generate-new-buffer
                             (format "*tags:%s*" mname)))))
    (if (equal cont '(0))
        (user-error "No matches for \"%s\"" mname)
      (with-current-buffer buffer
        (org-insert-time-stamp (current-time) t t
                               (format "* Tagged: %s, (%d) " mname (car cont)) "\n")
        (insert (cdr cont))
        (goto-char (point-min))
        (org-mode) (flyspell-mode -1) (view-mode)
        (org-content oclevel))
      (if noswitch
          buffer
        (pop-to-buffer buffer)))))

(defvar orgqda--csv-curr-mname nil)

;;;###autoload
(defun orgqda-collect-tagged-csv (&optional match)
  "Collect tagged paragraphs for csv export, use MATCH for matching."
  (interactive)
  (let* ((matcher (orgqda--make-tags-matcher match))
         (orgqda--csv-curr-mname (car matcher))
         (cont (orgqda--coll-tagged-csv matcher)))
    (pop-to-buffer
     (generate-new-buffer (format "*csvtags:%s*" orgqda--csv-curr-mname)))
    (when orgqda-convert-csv-to-encoding
      (orgqda--csv-convert-buffer-to-encoding))
    (insert cont)
    (goto-char (point-min))))

;;;###autoload
(defun orgqda-collect-tagged-csv-save (&optional match)
  "Collect  and save a file in ‘orgqda-csv-dir’, use MATCH for matching."
  (interactive)
  (let* ((matcher (orgqda--make-tags-matcher match))
         (orgqda--csv-curr-mname (car matcher))
         (cont (orgqda--coll-tagged-csv matcher))
         (current-csv-dir orgqda-csv-dir))
    (with-temp-buffer
      (insert cont)
      (when orgqda-convert-csv-to-encoding
        (orgqda--csv-convert-buffer-to-encoding))
      (unless nil
        (write-region (point-min) (point-max)
                      (expand-file-name
                       (concat orgqda--csv-curr-mname ".csv"
                               current-csv-dir)))))))

;;;###autoload
(defun orgqda-collect-tagged-csv-save-all (&optional threshold)
  "Save all tags used THRESHOLD or more times to csv-files.
One csv-file per tag is generated in ‘orgqda-csv-dir’."
  (interactive "P")
  (let ((tags (orgqda--get-tags-hash))
        (tn (prefix-numeric-value threshold)))
    (maphash (lambda (tag count)
               (when (or (not threshold)
                         (<= tn count))
                 (orgqda-collect-tagged-csv-save tag)))
             tags)))


;;;;; Commands for renaming tags
(defun orgqda-drag-merge-tags (ev)
  "Merge tags via dragging in tag listing buffer.
EV is the mouse event."
  (interactive "e")
  (let* ((start (event-start ev))
         (end (event-end ev))
         (s (posn-point start))
         (e (posn-point end))
         (s-tag (orgqda--tag-at-point s))
         (e-tag (orgqda--tag-at-point e)))
    (when (and s-tag e-tag
               (eq (posn-window start) (posn-window end)))
      (with-selected-window (posn-window start)
        (orgqda--refile-and-merge s e s-tag e-tag)))))

(defun orgqda-refile-and-merge-tags ()
  "Merge tags via refile selection (in codebook)."
  (interactive)
  (when-let* ((s (point))
              (s-tag (orgqda--otag-at-this-headline))
              (org-refile-targets '((nil :maxlevel . 4)))
              (newloc (org-refile-get-location "Refile and merge tags to: "))
              (e (nth 3 newloc))
              (e-tag (org-with-wide-buffer
                      (goto-char e)
                      (orgqda--otag-at-this-headline))))
    (orgqda--refile-and-merge s e s-tag e-tag)))

(defun orgqda-rename-tag (oldname newname)
  "Rename tag OLDNAME to NEWNAME in current orgqda files."
  (interactive (list
                (orgqda--completing-read-tag "Old tag name: " (orgqda--tag-at-point nil t) t)
                (orgqda--completing-read-tag "New tag name: " nil nil t)))
  (if newname
      (orgqda--delete-or-rename-tag oldname newname)
    (user-error "Not renaming! No new tag name!")))

(defun orgqda-delete-tag (tagname)
  "Delete tag TAGNAME in current orgqda files."
  (interactive (list (orgqda--completing-read-tag
                      "Tag to delete: "
                      (orgqda--tag-at-point nil t)
                      t)))
  (orgqda--delete-or-rename-tag tagname nil))

(defun orgqda-prefix-tag (oldname prefix)
  "Add a prefix PREFIX to existing tag OLDNAME.
Works on all current orgqda files."
  (interactive (list
                (orgqda--completing-read-tag "Old tag name: " (orgqda--tag-at-point nil t) t)
                (orgqda--completing-read-prefix
                 "Prefix: "
                 nil nil
                 ;; if we completed tags with helm we need to fetch a clean list
                 (unless (bound-and-true-p orgqda-helm-tags-mode)))))
  (orgqda-rename-tag oldname (concat prefix orgqda-hierarchy-delimiter oldname)))

(defun orgqda-rename-prefix-on-one-tag (oldname newprefix)
  "Rename the prefix(es) of tag OLDNAME to NEWPREFIX."
  (interactive (list
                (orgqda--completing-read-tag "Old tag name: " (orgqda--tag-at-point nil t) t)
                (orgqda--completing-read-prefix
                 "New prefix: "
                 nil nil
                 ;; if we completed tags with helm we need to fetch a clean list
                 (unless (bound-and-true-p orgqda-helm-tags-mode)))))
  (when-let ((tagleaf
              (car-safe
               (last (split-string oldname orgqda-hierarchy-delimiter)))))
    (orgqda-rename-tag oldname (concat newprefix orgqda-hierarchy-delimiter tagleaf))))

(defun orgqda-rename-prefix (oldprefix newprefix &optional taglist)
  "Rename OLDPREFIX to NEWPREFIX for all tags using it in current orgqda files.
TAGLIST can be passed as the list of tags to replace on."
  (interactive (list
                (orgqda--completing-read-prefix "Old prefix: " (orgqda--tag-prefix-at-point) t)
                (orgqda--completing-read-prefix "New prefix: " nil nil t)
                (orgqda--get-tags-for-completion t)))
  (cl-loop for tag in taglist
           when (string-prefix-p oldprefix tag)
           do (orgqda-rename-tag
               tag
               (concat newprefix (string-remove-prefix oldprefix tag)))))

;;;; Navigating to codebook

(defun orgqda-find-tag-in-codebook (tag)
  "Navigate to tag TAG in ‘orgqda-codebook-file’."
  (interactive (list (completing-read "Tag to to find in codebook: "
                                      (orgqda--get-tags-for-completion)
                                      nil nil
                                      (orgqda--tag-at-point))))
  (when (and orgqda-codebook-file (file-readable-p orgqda-codebook-file))
    (find-file orgqda-codebook-file)
    (orgqda--find-otag-link tag)))

(defun orgqda--find-otag-link (tag &optional pass-error)
  "Go to first otag link to TAG in current buffer.
Only reports errors if PASS-ERROR is non-nil."
  (widen)
  (goto-char (point-min))
  (when (search-forward-regexp (concat "\\[\\[otag:" tag) nil (not pass-error))
    (beginning-of-line)
    (pulse-momentary-highlight-one-line (point))))


;;;; Code relations functionality
(defvar orgqda--tag-relations-hash (make-hash-table :test 'equal)
  "Hash table for tag-relations.")

(defun orgqda-view-tag-relations (k)
  "Display occurrences of co-tagging.

Numeric prefix arg K defines which tuples to count"
  (interactive "p")
  (let ((origbuffer (current-buffer))
        (origfile (buffer-file-name))
        (k (if (< k 2) 2 k)))
    (orgqda--collect-tag-relations k)
    (pop-to-buffer
     (generate-new-buffer "*orgqda-tag-relations*"))
    (org-insert-time-stamp (current-time) t t
                           (format
                            "* Orgqda %d-tag-relations generated from %s at "
                            k
                            (org-link-make-string
                             (concat "file:" origfile)
                             (buffer-name origbuffer)))
                           "\n")
    (cl-loop for (combo . count)
             in (cl-sort (orgqda--hash-to-alist orgqda--tag-relations-hash) #'> :key #'cdr)
             do (insert (format "** [[otag:%s%s][%s]] (%d)\n"
                                combo
                                (and orgqda-taglink-include-filename
                                     (concat "::" origfile))
                                (replace-regexp-in-string "\\+" " + " combo)
                                count)))
    (goto-char (point-min))
    (org-mode) (view-mode) (orgqda-list-mode) (flyspell-mode -1)
    (setq orgqda--originating-buffer origbuffer)))

(defun orgqda--collect-tag-relations (k)
  "Collect tag relations between K tuples of tags in all orgqda files."
  (clrhash orgqda--tag-relations-hash)
  (org-map-entries
   (lambda () (orgqda--get-tag-relations-k k))
   nil
   (or
    (and orgqda-collect-from-all-files (orgqda-tag-files))
    (when-let ((bfn
                (or (buffer-file-name)
                    (buffer-file-name
                     (buffer-base-buffer)))))
      (list bfn)))
   'archive 'comment))

;; Inspiration from:
;; https://www.geeksforgeeks.org/print-subsets-given-size-set/

(defun orgqda--get-tag-relations-k (k)
  "Get tag relations for all K tuples of tags at point."
  (when-let ((taglist (org-get-tags nil t))
             (taglist (sort taglist #'string-lessp))
             (n (length taglist)))
    (orgqda--get-tag-relations-rec taglist n k nil 0)))

(defun orgqda--get-tag-relations-rec (tl n k data tlindex)
  "Recursive fn using TL N K DATA TLINDEX."
  ;; looks inefficient but it appears ok when byte-compiled
  (let ((data (copy-sequence data)))
    (if (= (length data) k)
        (let* ((tm (string-join (nreverse data) "+"))
               (ov (gethash tm orgqda--tag-relations-hash 0)))
          (puthash tm (1+ ov) orgqda--tag-relations-hash))
      (when (< tlindex n)
        (orgqda--get-tag-relations-rec
         tl n k (cons (nth tlindex tl) data) (1+ tlindex))
        (orgqda--get-tag-relations-rec tl n k data (1+ tlindex))))))


;;;; Internal functions
;;;;; collection-functions

(defun orgqda--coll-tagged (matcher level)
  "Collect tagged paragraphs or segments.
MATCHER is used for matching and LEVEL is the level in the hierarchy
for this tag.

Return cons-cell: (total count . string of taglists)"
  (let ((manyfiles (and orgqda-collect-from-all-files (orgqda-tag-files))))
    (if manyfiles
        ;; iterate over files assuming this file is included in
        ;; orgqda-tag-files (or shouldn't be)
        (let ((totalcount 0)
              str)
          (orgqda--with-many-files manyfiles
            (let ((ct (orgqda--coll-tagged-in-buffer matcher (1+ level))))
              (unless (and orgqda-exclude-empty-file-trees
                           (= 0 (car ct)))
                (cl-incf totalcount (car ct))
                (setq str (concat str
                                  (orgqda--taglist-file-heading
                                   (car ct) level)
                                  (cdr ct)
                                  "\n\n")))))
          (cons totalcount str))
      ;;only this buffer
      (orgqda--coll-tagged-in-buffer
       matcher level orgqda-respect-restriction-for-single-file))))

(defun orgqda--taglist-file-heading (number level)
  "Generate the heading for a file in a taglist.
NUMBER is the number of matches and LEVEL the level in the tree
to be used."
  (let ((fl (abbreviate-file-name
             (buffer-file-name (buffer-base-buffer)))))
    (format "%s [[file:%s][%s]] (%d)\n"
            (make-string level 42)
            fl (file-name-nondirectory fl) number)))

(defvar orgqda--ct-level 2
  "Level of headlines for tagcollection.
Used for passing this through to‘orgqda--get-paragraph-or-sub’")

(defvar orgqda--current-buffer-length nil)

(defun orgqda--coll-tagged-in-buffer (matcher level &optional unwidened)
  "Collect tagged paragraphs in buffer.
MATCHER is used for matching and LEVEL is the level in the hierarchy
for this tag. UNWIDENED preserves restriction for the collection.
Return cons-cell: (count in buffer count . string of taglist)"
  (let ((orgqda--ct-level level)
        (org-use-tag-inheritance orgqda-use-tag-inheritance))
    (orgqda--temp-work (not unwidened)
      (let* ((orgqda--current-buffer-length (point-max))
             (tl (org-scan-tags 'orgqda--get-paragraph-or-sub
                                (cdr matcher) nil)))
        (cons (length tl)
              (mapconcat 'identity tl "\n"))))))

(defun orgqda--get-paragraph-or-sub ()
  "Extract a paragraph or subtree depending on what is tagged."
  (save-excursion
    (if (or (org-at-heading-p) (org-inlinetask-in-task-p))
        (let* ((ln (line-number-at-pos))
               (bm (orgqda--get-encoded-bm))
               (link (format "opbm:%s" bm))
               (desc (orgqda--clean-up-heading-desc (org-get-heading t t t t)))
               (inherited-tags
                (when orgqda-use-tag-inheritance
                  (cl-loop for tag in org-scanner-tags
                           when (get-text-property 0 'inherited tag)
                           collect tag)))
               (ei1
                (and orgqda-tag-collect-extra-info
                     (assoc-default
                      (buffer-name)
                      orgqda-tag-collect-extra-info 'string-match-p)))
               (extrainfo (if ei1 (eval ei1) ""))
               (contents (orgqda--get-paragraph-or-sub-contents))
               (coverage (if orgqda-collect-coverage
                             (format " (%.2f%%)" (* 100 (/ (float (length contents)) orgqda--current-buffer-length)))
                           ""))
               (hl (format "%s [[%s][%d: %s]]%s%s%s %s\n"
                           (make-string orgqda--ct-level 42)
                           link ln desc coverage extrainfo
                           (if inherited-tags
                               (format
                                " (inherited: %s)"
                                (org-make-tag-string inherited-tags))
                             "")
                           (org-make-tag-string org-scanner-tags))))
          (concat hl contents))
      "ERROR: Not at a heading or inlinetask!")))

;;;;; CSV-collection-functions
;;TODO, perhaps more duplication could be avoided
(defun orgqda--coll-tagged-csv (matcher)
  "Return body of csv, depending on MATCHER."
  (let ((manyfiles (and orgqda-collect-from-all-files (orgqda-tag-files)))
        tags)
    (concat
     "citat,fil,fil:rad,file:hash,head,matchad,extra,extra2\n" ;;TODO, give reasonable names here
     ;; iterate orgqda-tag-files
     (if manyfiles
         (orgqda--with-many-files manyfiles
           (setq tags
                 (concat tags
                         (orgqda--coll-tagged-in-buffer-csv matcher))))
       ;;only this buffer
       (orgqda--coll-tagged-in-buffer-csv matcher)))))

(defun orgqda--coll-tagged-in-buffer-csv (matcher)
  "Collect tagged paragraphs in buffer for csv export.
MATCHER is used for matching.
Return cons-cell: (count in buffer count . string of taglist)"

  (let ((org-use-tag-inheritance orgqda-use-tag-inheritance))
    (orgqda--temp-work t
      (let ((tl (org-scan-tags 'orgqda--get-paragraph-or-sub-to-csv
                               (cdr matcher) nil)))
        (mapconcat 'identity tl "")))))

(defun orgqda--get-paragraph-or-sub-to-csv ()
  "Extract a paragraph or subtree for csv-export."
  (save-excursion
    (if (or (org-at-heading-p) (org-inlinetask-in-task-p))
        (let* ((ln (line-number-at-pos))
               (fl (file-name-base
                    (buffer-file-name (buffer-base-buffer))))
               (head (substring-no-properties (org-get-heading)))
               ;;(tags (org-get-tags));TODO Fix the list in some smart way
               (ei1
                (and orgqda-tag-collect-extra-info-csv
                     (assoc-default
                      (buffer-name)
                      orgqda-tag-collect-extra-info-csv 'string-match-p)))
               (extrainfo1 (when ei1 (eval ei1)))
               (ei2
                (and orgqda-tag-collect-extra-info2-csv
                     (assoc-default
                      (buffer-name)
                      orgqda-tag-collect-extra-info2-csv 'string-match-p)))
               (extrainfo2 (when ei2 (eval ei2)));TODO, This doesn’t
                                        ;work if extrainfo is undefined
               (contents
                (format
                 "\"%s\","
                 (org-export-string-as
                  (orgqda--get-paragraph-or-sub-contents)
                  'ascii t
                  '(:with-smart-quotes t :ascii-charset utf-8 :ascii-inner-margin 0 :ascii-text-width 99999))))
               (secondary
                (format "\"%s\",\"%s:%s\",\"%s:%s\",\"%s\",\"%s\""
                        fl fl ln fl (secure-hash 'md5 contents)
                        head orgqda--csv-curr-mname))) ; TODO, works
                                        ; with dynamic binding?
          ;;put everything together
          (concat contents secondary extrainfo1 extrainfo2 "\n"))
      "Not a heading or inlinetask???")))

(defun orgqda--csv-convert-buffer-to-encoding ()
  "Convert csv buffer to correct encoding.

Makes sure the current csv-buffer only contains characters
belonging to the encoding specified in
‘orgqda-convert-csv-to-encoding’ and sets the buffer's coding
system to that encoding. Probably quite inefficient as it checks
each character in the buffer."
  (when orgqda-convert-csv-to-encoding
    (let ((encoding (if (eq t orgqda-convert-csv-to-encoding)
                        'iso-8859-1
                      orgqda-convert-csv-to-encoding)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (unless (char-charset (char-after) encoding)
            ;;replace with question mark
            (delete-char 1)
            (insert-char 63))
          (forward-char 1))
        (set-buffer-file-coding-system encoding)))))

;;;;; common collection-functions

(defun orgqda--get-paragraph-or-sub-contents ()
  "Extract contents of matched paragraph or subtree."
  (let ((inhibit-message t))
    (string-trim
     (cond
      ((org-inlinetask-in-task-p)
       (if (orgqda-inlinetask-in-degenerate-task-p)
           (buffer-substring-no-properties
            (point) (progn (orgqda--backward-paragraph) (point)))
         (buffer-substring-no-properties
          (save-excursion (org-inlinetask-goto-beginning)
                          (end-of-line) (point))
          (save-excursion (org-inlinetask-goto-end)
                          (forward-line 0) (point)))))
      ((org-at-heading-p)
       (org-copy-subtree)
       (orgqda--inhibit-org-startups
        (with-temp-buffer
          (org-mode)
          (org-paste-subtree orgqda--ct-level nil nil t)
          (forward-line 1)
          (buffer-substring-no-properties
           (point) (point-max)))))))))

;; inspired by:
;; http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
;; org-backward-paragraph is better in some sense, but does a little
;; too much, so we hack around a bit here.

(defun orgqda--backward-paragraph ()
  "Go back to last blank line, but after headlines."
  (skip-chars-backward "\n[:blank:]")
  (if (search-backward-regexp
       "\n[[:blank:]]*\n[[:blank:]]*" nil t 1)
      (progn (goto-char (match-end 0))
             (when (looking-at org-heading-regexp)
               (forward-line 1)))
    (goto-char (point-min))))

;;;;; General helper-functions
(defun orgqda-get-parent-hl (level)
  "Get parent headline at level LEVEL."
  (save-excursion
    (when (re-search-backward (format "^\\*\\{%d,%d\\} " level level) nil t)
      (substring-no-properties (org-get-heading t t)))))

(defun orgqda-inlinetask-in-degenerate-task-p ()
  "Return non-nil when point is in a degenerate inlinetask."
  (save-excursion
    (let* ((case-fold-search t)
           (stars-re (org-inlinetask-outline-regexp))
           (task-beg-re (concat stars-re "\\(?:.*\\)"))
           (task-end-re (concat stars-re "END[ \t]*$")))
      (beginning-of-line)
      (and (and
            (org-looking-at-p task-beg-re)
            (not (org-looking-at-p task-end-re)))
           (progn
             (end-of-line)
             (re-search-forward "^\\*+[ \t]+" nil t)
             (beginning-of-line)
             (not (org-looking-at-p task-end-re)))))))


;;;;; link type for linking to parts of buffer

(org-link-set-parameters "opbm"
                         :follow #'orgqda-opbm-open
                         :export #'orgqda-link-desc-export
                         :store #'orgqda-opbm-store-link)

(defun orgqda-opbm-store-link ()
  "Store a bookmark-link to a position in an org-mode-buffer."
  (when (and (equal major-mode 'org-mode)
             orgqda-mode
             (not (org-at-heading-p)))
    (org-link-store-props
     :type "opbm"
     :link (format "opbm:%s" (orgqda--get-encoded-bm))
     :description (format "bm at: %s:%s"
                          (buffer-file-name) (line-number-at-pos)))))

(defun orgqda-opbm-open (opbm)
  "Display OPBM link in other buffer."
  (save-current-buffer
    (let ((bm (cons "n" (read (org-link-decode (org-link-unescape opbm))))))
      (find-file-other-window (bookmark-get-filename bm))
      (bookmark-default-handler bm)
      (recenter)))
  (pop-to-buffer (current-buffer)))

(defun orgqda-link-desc-export (_link desc format)
  "Export function for special orgqda links.
We can't export these links nicely, always export DESC, with
slightly different formatting depending on FORMAT."
  (format
   (cl-case format
     (latex "\\emph{%s}")
     (html "<em>%s</em>")
     (t "%s"))
   (substring-no-properties desc)))

(defun orgqda--get-encoded-bm ()
  "Encode a bookmark reference as a string."
  (require 'bookmark)
  (org-link-escape
   (org-link-encode
    (prin1-to-string
     (bookmark-make-record-default))
    '(10) ; add \n to escape table
    )))

(defun orgqda--clean-up-heading-desc (heading)
  "Clean up a HEADING string used as description for an opbm link."
  (thread-last heading
    (substring-no-properties)
    (org-link-display-format)
    (replace-regexp-in-string "\\[" "")
    (replace-regexp-in-string "\\]" "")))

;;;;; List tags functions
(defvar orgqda--current-tagscount (make-hash-table :test 'equal)
  "Hash table for collecting counts of tags.")

(defun orgqda--get-tags-hash (&optional exclude-tags)
  "Return a hash of all tags with counts.
In this buffer or in all files in ‘orgqda-tag-files’. If
EXCLUDE-TAGS is non nil, use that instead of
‘orgqda-exclude-tags’ for tags to exclude."
  (clrhash orgqda--current-tagscount)
  (let ((org-use-tag-inheritance orgqda-use-tag-inheritance))
    (orgqda--with-many-files (or (and orgqda-collect-from-all-files
                                      (orgqda-tag-files))
                                 (when-let ((bfn
                                             (or (buffer-file-name)
                                                 (buffer-file-name
                                                  (buffer-base-buffer)))))
                                   (list bfn)))
      (org-scan-tags
       #'orgqda--get-tags-with-count
       (cdr (orgqda--make-tags-matcher 'none))
       nil)))
  (dolist (ex (or exclude-tags orgqda-exclude-tags))
    (remhash ex orgqda--current-tagscount))
  orgqda--current-tagscount)

(defun orgqda--get-tags-alist (&optional sort exclude-tags)
  "Return an alist of all tags with counts.
In this buffer or in all files in ‘orgqda-tag-files’.
Optional SORT can be symbols: count-decreasing, count-increasing,
a-z, or z-a. EXCLUDE-TAGS overrides ‘orgqda-exclude-tags’."
  (let ((tl (orgqda--tags-hash-to-alist (orgqda--get-tags-hash exclude-tags))))
    (cl-case sort
      (count-decreasing (cl-sort tl '> :key 'cdr))
      (count-increasing (cl-sort tl '< :key 'cdr))
      (a-z (cl-sort tl #'orgqda--string-lessp :key 'car))
      (z-a (cl-sort tl #'orgqda--string-greaterp :key 'car))
      (t tl))))

(cl-defstruct (orgqda--hierarchical-taglist
               (:constructor orgqda--make-htl)
               (:conc-name orgqda--htl-))
  (hierarchy (hierarchy-new))
  (counts (make-hash-table :test 'equal)))

(defvar orgqda--current-htl (orgqda--make-htl)
  "Holds the current hierarchical list of tags with counts.")

(defun orgqda--create-hierarchical-taglist (&optional sort taghash)
  "Store a hierarchical taglist in ‘orgqda--current-htl’.
Optional SORT can be symbols: count-decreasing (default),
count-increasing, a-z, or z-a. TAGHASH specifies a custom taghash
not loaded with ‘orgqda--get-tags-hash’."
  (let ((taghash (or taghash (orgqda--get-tags-hash))))
    (setq orgqda--current-htl
          (orgqda--make-htl
           :counts taghash))
    (hierarchy-add-trees (orgqda--htl-hierarchy orgqda--current-htl)
                         (hash-table-keys taghash)
                         (if orgqda-hierarchy-delimiter
                             #'orgqda--hierarchy-parentfn
                           (lambda (_) nil)))
    ;; Possibly sort in two passes for a stable order
    ;; Initial sort (if primarily sorting by count, sort those with equal counts a-z)
    (when (not (member sort '(a-z z-a)))
      (hierarchy-sort (orgqda--htl-hierarchy orgqda--current-htl) #'orgqda--string-lessp))
    ;; The primary sorting
    (hierarchy-sort
     (orgqda--htl-hierarchy orgqda--current-htl)
     (alist-get sort orgqda-sort-args #'orgqda--hierarchy-count-greater-p))))

(defun orgqda--insert-hierarchical-taglist (full origbuffer filename &optional startlevel starttag)
  "Insert a hierarchical taglist.

FULL non-nil means also insert extracted paragraphs for all tags.

ORIGBUFFER is the buffer where the search should start.
FILENAME is the filename of the file where search starts.
STARTLEVEL is the root-level of the tree.
STARTTAG is the prefix where the search should start."
  (let* ((taginfo (when full
                    (with-current-buffer origbuffer
                      (orgqda--get-codebook-info))))
         (labelfn
          (hierarchy-labelfn-indent
           (lambda (item indent)
             (orgqda--insert-taglist-item
              item filename)
             (when full
               (when-let ((info (alist-get item taginfo nil nil #'equal)))
                 (insert (format "/%s/\n" info))))
             (when (and full (not (string=
                                   orgqda-hierarchy-delimiter
                                   (substring item -1))))
               (insert
                (with-current-buffer origbuffer
                  (cdr (orgqda--coll-tagged (orgqda--make-tags-matcher item t)
                                            (+ 2 indent))))
                "\n")))
           "*")))
    (if starttag
        (hierarchy-map-item labelfn starttag (orgqda--htl-hierarchy orgqda--current-htl) (or startlevel 0))
      (hierarchy-map labelfn (orgqda--htl-hierarchy orgqda--current-htl) (or startlevel 0)))))

(defun orgqda--insert-taglist-item (item filename)
  "Insert a single item in a taglist.
ITEM represents the item and FILENAME where it is from."
  (insert
   (format "* [[otag:%s%s][%s]] %s\n"
           (if (string=
                orgqda-hierarchy-delimiter
                (substring item -1))
               (concat "{" item "}") ;make regexp-match for prefix
             item)
           (orgqda--string-or-empty filename "::")
           item
           (orgqda--tagcount-string item))))

(defun orgqda--tagcount-string (tag)
  "Generate string of counts for TAG."
  (let ((count (gethash tag (orgqda--htl-counts orgqda--current-htl) nil)))
    (if (or (eq 'all orgqda-tagcount-show-files)
            (and (eq 'not-parents orgqda-tagcount-show-files)
                 (not (orgqda--tag-prefix-p tag))))
        (cl-loop for (file . c) in
                 (cl-sort count #'string-version-lessp :key #'car)
                 unless (equal file 'found)
                 collect
                 (if (equal "" file)
                     (format "*%d*" c)
                   (format "/%s/: *%d*"
                           (orgqda--tagcount-transform-file-name file)
                           c))
                 into l
                 finally return
                 (concat "(" (mapconcat #'identity l ", ") ")"))
      (format "(*%d*)" (alist-get "" count 0)))))

(defvar orgqda--tagcount-filename-hash (make-hash-table
                                        :size 10
                                        :rehash-size 5
                                        :test 'equal))

(defun orgqda--tagcount-transform-file-name (filename)
  "Transform FILENAME according to ‘orgqda-tagcount-files-transform-functions’.
Cache transformed values in ‘orgqda--tagcount-filename-hash’."
  (or (gethash filename orgqda--tagcount-filename-hash)
      (puthash filename
               (cl-loop with f = filename
                        for fun in orgqda-tagcount-files-transform-functions
                        do (setq f (funcall fun f))
                        finally return f)
               orgqda--tagcount-filename-hash)))

(defun orgqda--hierarchy-parentfn (tag)
  "Return parent of TAG. Also update count of all parents if we are at a leaf."
  (let* ((splittag (split-string tag orgqda-hierarchy-delimiter t)))
    (when (> (safe-length splittag) 1)
      ;; if this is a leaf, update count for all parents
      (when (not (orgqda--tag-prefix-p tag))
        (let ((prefixes (cl-loop for p in
                                 (orgqda--build-prefixes (butlast splittag 1))
                                 collect (concat p orgqda-hierarchy-delimiter))))
          (cl-loop for (file . count) in (gethash tag (orgqda--htl-counts orgqda--current-htl))
                   do
                   (cl-loop for p in prefixes do
                            (cl-incf (alist-get
                                      file
                                      (gethash p (orgqda--htl-counts orgqda--current-htl))
                                      0 nil #'equal)
                                     count)))))
      ;; return immediate parent:
      (replace-regexp-in-string
       (concat (car (last splittag)) orgqda-hierarchy-delimiter "?$") "" tag))))

(defun orgqda--hierarchy-count-greater-p (x y)
  "Return non-nil if tag X has greater count than Y."
  (> (alist-get "" (gethash x (orgqda--htl-counts orgqda--current-htl)) -1)
     (alist-get "" (gethash y (orgqda--htl-counts orgqda--current-htl)) -2)))
(defun orgqda--hierarchy-count-less-p (x y)
  "Return non-nil if tag X has smaller count than Y."
  (< (alist-get "" (gethash x (orgqda--htl-counts orgqda--current-htl)) -2)
     (alist-get "" (gethash y (orgqda--htl-counts orgqda--current-htl)) -1)))

(defun orgqda--string-lessp (s1 s2)
  "Return non-nil if S1 is less than S2 in collation order.
Ignore case and collate depending on current locale."
  (string-collate-lessp s1 s2 nil t))
(defun orgqda--string-greaterp (s1 s2)
  "Return non-nil if S1 is greater than S2 in collation order.
Ignore case and collate depending on current locale."
  (string-collate-lessp s2 s1 nil t))

(defun orgqda--get-tags-with-count ()
  "Add tags at point to ‘orgqda--current-tagscount’."
  (dolist (tag org-scanner-tags)
    (let ((ov (gethash tag orgqda--current-tagscount nil)))
      (cl-incf (alist-get buffer-file-name ov 0 nil #'equal)) ; need ‘equal’
      (cl-incf (alist-get "" ov 0)) ; this is ok because (eq "" "") is t
      (puthash (substring-no-properties tag) ov orgqda--current-tagscount))))

(defun orgqda--make-tags-matcher (&optional match force-simple)
  "Construct a tags matcher, excluding commented and archived trees.

Match string is passed in MATCH or prompted for.

The matcher is constructed by bypassing ‘org-make-tags-matcher’ if
the match is for a single tag, since generation otherwise takes
too long with long tag names (‘org-make-tags-matcher’ does some
expensive stuff, and this is no good when we are collecting
extracts for all tags). This behaviour can be forced with
FORCE-SIMPLE."
  (let* ((match (or match
                    (completing-read
                     "Match: "
                     (orgqda--get-tags-for-completion)
                     nil nil (orgqda--tag-at-point))))
         conds)
    (when orgqda-only-count-matching
      (push '(cl-intersection orgqda-only-count-matching tags-list :test #'string-match-p) conds))
    (cond
     ((symbolp match)) ;; no more conditions (usually when counting)
     ((or force-simple
          (string-match-p (concat "^" org-tag-re "$") match))
      (push `(member ,match tags-list) conds))
     (t (push (nth 2 (cdr (org-make-tags-matcher match))) conds)))
    (cons match `(lambda (todo tags-list level)
                   (setq org-cached-props nil)
                   (and ,@(nreverse conds))))))

;;;;;; link type for taglist

;; The format of the link string is:
;; otag:TAG-OR-SEARCH[::FILENAME]
;; Tag can be an exact tag match, or a tag search as defined in
;; Info node ‘(org)Matching tags and properties’
;; ::FILENAME is optional.


(org-link-set-parameters "otag"
                         :follow #'orgqda-otag-open
                         :export #'orgqda-link-desc-export
                         :store #'orgqda-otag-store-link)

(defun orgqda-otag-open (otag)
  "Collect extracts tagged with the tag defined in OTAG link."
  (cl-destructuring-bind (tag file) (orgqda--otag-parse otag)
    (orgqda--with-current-buffer-if
        (or (when (and file (file-readable-p file))
              (find-file-noselect file))
            (when (buffer-live-p orgqda--originating-buffer)
              orgqda--originating-buffer)
            (when orgqda-collect-from-all-files
              (when-let ((tf (car-safe (orgqda-tag-files))))
                (when (file-readable-p tf)
                  (find-file-noselect tf)))))
      (orgqda-collect-tagged tag))))

(defun orgqda-otag-store-link ()
  "Store a link to a org mode file and tag."
  (when (equal major-mode 'org-mode)
    (when-let ((oir (org-in-regexp
                     (concat "\\(:" org-tag-re "\\):[ \t]*$"))))
      (let* ((fn (if orgqda-taglink-include-filename
                     (concat "::" (buffer-file-name))
                   ""))
             (tagpos (org-between-regexps-p ":" ":" (car oir) (cdr oir)))
             (tag (buffer-substring-no-properties (1+ (car tagpos)) (1- (cdr tagpos))))
             (link (format "otag:%s%s" tag fn)))
        (org-link-store-props
         :type "otag"
         :link link
         :description tag)))))

(defun orgqda--otag-parse (linkstring &optional strict)
  "Parse LINKSTRING as link part an otag link. Return list (tag filename).
STRICT non-nil means to only return tag if it is a single tag
name and not a tag search, as defined in Info node ‘(org)Matching
tags and properties’. LINKSTRING may be a full “otag: tagname::
filename” link or just the path part: “tagname:: filename”, with
the last “::filename” always optional."
  (let ((tagmatch (if strict
                      (rx (regex org-tag-re))
                    (rx (or (+ (seq (regex org-tag-re)
                                    (? (any ?+ ?- ?| ))))  ; single tag or boolean combinations
                            (seq "{" (regex org-tag-re) "}") ; tag regex search (used for prefixes)
                            )))))
    (when (string-match (rx (seq bos
	                             (? "otag:")
                                 (group
                                  (regex tagmatch))
                                 (? (seq "::" (group (+ not-newline))))
                                 eos))
                        linkstring)
      (list (match-string 1 linkstring)
            (match-string 2 linkstring)))))

(defun orgqda--otag-tag (linkstring &optional strict)
  "Get tag from an otag LINKSTRING.
STRICT is passed to ‘orgqda--otag-parse’."
  (car (orgqda--otag-parse linkstring strict)))

(defun orgqda--otag-file (linkstring)
  "Get filename from an otag LINKSTRING."
  (cadr (orgqda--otag-parse linkstring)))

;;;;; Functions for taglist update
(defvar orgqda--pending-tag-count-replacements nil)
(defvar orgqda--removed-tags nil)

(defun orgqda-update-taglist-general ()
  "Update taglists in any org buffer.
Expects to find otag-links and updates any count number following
them. If ‘orgqda--originating-buffer’ is set, uses that,
otherwise assumes tags can be found in this buffer or the ones
defined by ‘orgqda-tag-files’.

Generates a list of \"new\" tags, tags not linked to in this buffer."
  (let ((newtags (make-hash-table :test 'equal)))
    (orgqda--with-current-buffer-if
        orgqda--originating-buffer
      (orgqda--create-hierarchical-taglist))
    (setq orgqda--pending-tag-count-replacements nil
          orgqda--removed-tags nil)
    ;; update counts
    (orgqda--temp-work t
      (save-match-data
        (org-element-map (org-element-parse-buffer) 'link #'orgqda--update-tag-count-link)
        ;; do the replacements
        (cl-loop for (match . rep) in (reverse orgqda--pending-tag-count-replacements)
                 do
                 (set-match-data match)
                 (replace-match rep)
                 (set-match-data match t))))
    ;; list new and removed tags
    (maphash
     (lambda (tag count)
       (unless (assq 'found count)
         (puthash tag count newtags)))
     (orgqda--htl-counts orgqda--current-htl))
    (when (or orgqda--removed-tags
              (not (hash-table-empty-p newtags)))
      (let ((buf (generate-new-buffer "*Tag changes*"))
            (inhibit-read-only t)
            (localvars (buffer-local-variables))
            (bf (buffer-file-name)))
        (with-current-buffer buf
          (cl-loop for (var . val) in localvars
                   if (string-match "^orgqda"
					                (symbol-name var))
                   do (set (make-local-variable var) val)))
        (orgqda--create-hierarchical-taglist nil newtags)
        (orgqda-list-tags nil nil buf t "Possibly added tags")
        (with-current-buffer buf
          (goto-char (point-min))
          (org-map-tree (lambda () (end-of-line) (newline) (org-time-stamp '(16) 'inactive)))
          (when orgqda--removed-tags
            (goto-char (point-max))
            (insert
             (concat "* Possibly removed tags\n- "
                     (mapconcat #'identity orgqda--removed-tags "\n- "))))
          (when bf
            (setq-local org-refile-targets
                        `(((,bf) . (:maxlevel . 4)))
                        org-refile-keep t))
          (goto-char (point-min)))))))

(defun orgqda--update-tag-count-link (link)
  "Update displayed tag count for tag linked to by LINK."
  (when (string= "otag" (org-element-property :type link))
    (let* ((sp (split-string (org-element-property :path link) ":"))
           (tag (or (cadr sp) (car sp)))
           (tag (if (string-match-p "^{[^{}]+}$" tag)
                    (substring tag 1 -1)
                  tag))
           (count (gethash tag (orgqda--htl-counts orgqda--current-htl))))
      (when (and count (not (assq 'found count)))
        ;;mark the ones found by adding found to the count alist
        (puthash tag
                 (cl-acons 'found nil count)
                 (orgqda--htl-counts orgqda--current-htl)))
      (unless count (push tag orgqda--removed-tags))
      (save-excursion
        (goto-char (org-element-property :end link))
        (when (looking-at "([^)]+)")
          (push (cons (match-data)
                      (if count (orgqda--tagcount-string tag) "(*0?!*)"))
                orgqda--pending-tag-count-replacements))))))


;;;;; Functions for rename commands
(defun orgqda--delete-or-rename-tag (oldname newname)
  "Renames tag OLDNAME → NEWNAME in current orgqda files.
If NEWNAME is nil, delete the tag."
  (let (repslist)
    (if-let ((manyfiles
              (orgqda--with-current-buffer-if orgqda--originating-buffer
                (and orgqda-collect-from-all-files (orgqda-tag-files)))))
        (orgqda--with-current-buffer-if orgqda--originating-buffer
          (dolist (file manyfiles)
            (with-current-buffer (find-file-noselect file)
              (push (cons (file-name-base file)
                          (orgqda--rename-tag-in-buffer oldname newname))
                    repslist))))
      ;; Only originating or this buffer.
      (orgqda--with-current-buffer-if orgqda--originating-buffer
        (push (cons (buffer-name)
                    (orgqda--rename-tag-in-buffer oldname newname))
              repslist)))
    ;; Maybe rename in codebook-file.
    (when-let
        ((cbfile (or orgqda-codebook-file
                     (orgqda--with-current-buffer-if orgqda--originating-buffer
                       orgqda-codebook-file))))
      (with-current-buffer (find-file-noselect cbfile)
        (push (cons
               (concat (file-name-base cbfile) " (links)")
               (orgqda--rename-tag-links-in-buffer oldname newname))
              repslist)))
    (setq repslist (nreverse repslist))
    ;; Report message:
    (cl-loop with total = 0
             for x in repslist
             unless (= 0 (cdr x))
             do (cl-incf total (cdr x))
             and concat (concat (car x) " "
                                (propertize (number-to-string (cdr x))
                                            'face 'bold) ", ")
             into filesums
             finally do
             (message "%s %s%s. Σ %s, %s"
                      (if newname "Replaced" "Deleted")
                      (propertize oldname 'face 'italic)
                      (if newname (concat " → " (propertize newname 'face 'italic)) "")
                      (propertize (number-to-string total) 'face 'bold)
                      filesums))
    (when (or orgqda-list-mode orgqda-codebook-mode)
      (orgqda-revert-taglist))))

(defun orgqda--rename-tag-in-buffer (oldname newname)
  "Rename all ocurrences of OLDNAME as an org tag with NEWNAME.
If NEWNAME is nil, deletes the tag.
Return number of replacements done."
  (let ((numberofreps 0)
        (inhibit-read-only t)) ;; important that tags are really replaced
    (orgqda--temp-work t
      (while (search-forward (concat ":" oldname ":") nil t)
        (org-set-tags
         (cl-remove-duplicates
          (if newname
              (cl-substitute newname oldname (org-get-tags nil t) :test #'string=)
            (cl-remove oldname (org-get-tags nil t) :test #'string=))
          :test #'string=))
        (setq numberofreps (1+ numberofreps))))
    numberofreps))

(defun orgqda--rename-tag-links-in-buffer (old new)
  "Rename all tag links in buffer with tag name OLD to NEW."
  (orgqda--temp-work t
    (cl-loop while (search-forward-regexp
                    (format "\\[\\[otag:%s\\(::[^]]+\\)?\\]\\[%s\\]\\]"
                            old old) nil t)
             count (progn (if new
                              (replace-match (format "[[otag:%s\\1][%s]]" new new))
                            (replace-match ""))
                          t))))

(defmacro orgqda--move-subtree-then (start end body)
  "Move subtree at START to subtree at END (if in a file). Then execute BODY."
  (declare (indent 2))
  `(progn
     (when (buffer-file-name)
       (save-excursion
         (when (and (save-excursion (goto-char ,start)
                                    (org-at-heading-p))
                    (save-excursion (goto-char ,end)
                                    (org-at-heading-p)))
           (goto-char ,start)
           (org-refile nil nil (list "end" (buffer-file-name) nil ,end)))))
     ,body))

(defun orgqda--format-bold-strings (format &rest args)
  "Put face property bold on all strings in ARGS and ‘format’ with FORMAT."
  (apply #'format format (cl-loop for a in args
                                  collect (propertize a 'face 'bold))))

(defun orgqda--refile-and-merge (start end s-tag e-tag)
  "Refile and merge tags S-TAG to E-TAG from points START to END."
  (let* ((prefre (concat "^{\\(" org-tag-re "\\)_}$"))
         (s-pref (when (string-match prefre s-tag)
                   (match-string 1 s-tag)))
         (e-pref (when (string-match prefre e-tag)
                   (match-string 1 e-tag))))
    (cond ((and s-pref e-pref)
           (when-let* ((subsume (concat e-pref orgqda-hierarchy-delimiter s-pref))
                       (newpref
                        (cl-case
                            (read-char-choice
                             (orgqda--format-bold-strings
                              "a. Rename prefix “%s” → “%s” in all tags.
b. Rename prefix “%s” → “%s” in all tags. "
                              s-pref subsume s-pref e-pref)
                             '(?a ?b ?q))
                          (?a subsume)
                          (?b e-pref))))
             (orgqda--move-subtree-then start end
               (orgqda-rename-prefix s-pref newpref (orgqda--get-tags-for-completion t)))))
          (e-pref
           (cl-case (read-char-choice
                     (orgqda--format-bold-strings
                      "a. Replace prefixes of tag “%s” with ”%s”.
b. Subsume tag ”%s” under prefix ”%s”. "
                      s-tag e-pref s-tag e-pref)
                     '(?a ?b ?q))
             (?a (orgqda--move-subtree-then start end
                   (orgqda-rename-prefix-on-one-tag s-tag e-pref)))
             (?b (orgqda--move-subtree-then start end
                   (orgqda-prefix-tag s-tag e-pref)))))
          (s-pref
           (user-error (orgqda--format-bold-strings
                        "Moving prefix “%s” to tag “%s” not supported"
                        s-pref e-tag)))
          ((y-or-n-p (format "Merge tags %s → %s? " s-tag e-tag))
           (orgqda--move-subtree-then start end
             (orgqda-rename-tag s-tag e-tag))))))

;;;;; Completion

(defvar orgqda--tag-completion-list nil
  "Tags offered for completion.")

(defun orgqda--completing-read-tag (prompt &optional initial-input require-match no-reload)
  "Fetch a tag-name via helm or ‘completing-read’.
PROMPT, INITIAL-INPUT, REQUIRE-MATCH as in ‘completing-read’.
NO-RELOAD reuses already initalized completion list (in
‘orgqda--tag-completion-list’ or ‘orgqda-helm-tags-comp-list’)."
  (if (bound-and-true-p orgqda-helm-tags-mode)
      (orgqda-helm-tags-get-tag prompt initial-input require-match no-reload)
    (completing-read prompt (orgqda--get-tags-for-completion no-reload) require-match initial-input)))

(defun orgqda--completing-read-prefix (prompt &optional initial-input require-match no-reload)
  "Fetch a tag prefix via ‘completing-read’.
PROMPT, INITIAL-INPUT, REQUIRE-MATCH as in ‘completing-read’.
NO-RELOAD reuses already initalized tag completion list (in
‘orgqda--tag-completion-list’ or ‘orgqda-helm-tags-comp-list’)."
  ;; FIXME: We don’t have special helm completion (in orgqda-helm-tags.el)
  ;; here yet. Complex to implement cleanly.
  (completing-read prompt (orgqda--get-prefixes-for-completion no-reload) nil require-match initial-input))

(defun orgqda--get-tags-for-completion (&optional no-reload)
  "Return current list of tags in orgqda (possibly many files).
NO-RELOAD means just use previously initialized list."
  (if no-reload
      orgqda--tag-completion-list
    (setq
     orgqda--tag-completion-list
     (hash-table-keys
      (orgqda--with-current-buffer-if
          orgqda--originating-buffer
        (orgqda--get-tags-hash))))))

(defun orgqda--get-prefixes-for-completion (&optional no-reload)
  "Return the current list of tag prefixes for tags in orgqda.
Prefixes are those delimited with ‘orgqda-hierarchy-delimiter’.
NO-RELOAD reuses previously initialized taglist."
  (let ((taglist (orgqda--get-tags-for-completion no-reload))
        prefixes)
    (dolist (tag taglist)
      (let* ((splittag (split-string tag orgqda-hierarchy-delimiter t))
             (cats (butlast splittag)))
        (when cats
          (setq prefixes
                (append prefixes (orgqda--build-prefixes cats))))))
    (cl-remove-duplicates prefixes :test 'string=)))

(defun orgqda--build-prefixes (preflist &optional pref)
  "Recursive function for constructing prefix list for completion.
PREFLIST is the remaining list. PREF the current prefix."
  (when preflist
    (let ((curr (if pref
                    (concat pref orgqda-hierarchy-delimiter (car preflist))
                  (car preflist))))
      (cons curr (orgqda--build-prefixes (cdr preflist) curr)))))

(defun orgqda--tag-at-point (&optional pos strict)
  "Get the tag name of the otag-link, or tag in taglist, at point or POS.
STRICT only gets real tag names from otag links."
  (save-excursion
    (when pos (goto-char pos))
    (let* ((context (org-element-lineage
                     (org-element-context)
                     '(link headline inlinetask)
                     t))
           (type (org-element-type context)))
      (cond ((and (eq type 'link)
                  (string= (org-element-property :type context) "otag"))
             (orgqda--otag-tag (org-element-property :path context) strict))
            ((and (memq type '(headline inlinetask))
                  (org-match-line org-complex-heading-regexp))
             (let ((tags-beg (match-beginning 5))
	               (tags-end (match-end 5)))
               (if (and tags-beg (>= (point) tags-beg) (< (point) tags-end))
	               ;; On tags.
                   (let* ((beg-tag (or (search-backward ":" tags-beg t) (point)))
			              (end-tag (search-forward ":" tags-end nil 2)))
		             (buffer-substring (1+ beg-tag) (1- end-tag)))
                 ;; last attempt: an otag-link in this headline
                 (orgqda--otag-at-this-headline))))))))

(defun orgqda--otag-at-this-headline ()
  "Return tag in first otag-link in current headline.
Assumes point is on a headline."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (and (search-forward-regexp org-link-bracket-re (point-at-eol) t)
           (save-match-data (string-match "^otag:" (match-string 1)))
           (orgqda--otag-tag (match-string-no-properties 1))))))

(defun orgqda--tag-prefix-at-point ()
  "Return prefix of tag at point, or nil if none found."
  (when-let ((tag (orgqda--tag-at-point)))
    (when
        (rx-let ((tagpref (group-n 1 (+ (seq (+ alnum) (literal orgqda-hierarchy-delimiter))))))
          (string-match (rx (seq bol (or (seq "{" tagpref  "}" eol)
                                         tagpref)))
                        tag))
      (substring (match-string 1 tag) 0 -1))))

;;;;; Various functions
(defun orgqda-tag-files ()
  "Return list of files which should be searched for tags.
Based on value of variable ‘orgqda-tag-files’ which could be a
list of files and directories, or a file containing such a list.
Calls function ‘org-agenda-files’ with variable ‘org-agenda-files’
set to ‘orgqda-tag-files’"
  (require 'org-agenda)
  (let ((org-agenda-files orgqda-tag-files)
        (org-directory
         (cond ((stringp orgqda-tag-files)
                (file-name-directory (expand-file-name orgqda-tag-files)))
               ((and (listp orgqda-tag-files)
                     (cl-notany #'file-name-absolute-p orgqda-tag-files))
                default-directory)
               (t org-directory))))
    (org-agenda-files t)))

(defun orgqda--hash-to-alist (hashtable)
  "Convert HASHTABLE to alist."
  (cl-loop for k being the hash-keys of hashtable
           using (hash-values v)
           collect (cons k v)))

(defun orgqda--tags-hash-to-alist (hashtable)
  "Convert tag HASHTABLE to alist."
  (cl-loop for k being the hash-keys of hashtable
           using (hash-values v)
           collect (cons k (alist-get "" v 0))))

(defun orgqda--hl-get-count ()
  "Return first count in parentheses in headline, or 0."
  (save-excursion
    (save-match-data
      (if (search-forward-regexp
           "(\\*\\([0-9]+\\)\\*.*)"
           (point-at-eol) t)
          (string-to-number (match-string 1))
        0))))

(defun orgqda--sort-subtree ()
  "Sort current tree if it has children."
  (when (save-excursion (org-goto-first-child))
    (apply #'org-sort-entries orgqda--current-sorting-args)))

(defun orgqda--tag-prefix-p (tag)
  "Non-nil if TAG ends in ‘orgqda-hierarchy-delimiter’."
  (when orgqda-hierarchy-delimiter
    (string= (substring tag -1) orgqda-hierarchy-delimiter)))

;;;;; Retrieve codebook info
(defun orgqda--get-codebook-info ()
  "Return alist of tag names and coding info.
Coding info is the first line of the matching line for the tag in
‘orgqda-codebook-file’"
  (when orgqda-codebook-file
    (cl-delete-if
     #'null
     (org-map-entries
      #'orgqda--get-tag-info
      t
      (list orgqda-codebook-file)))))

(defun orgqda--get-tag-info ()
  "Get tag info for current entry in codebook file."
  (when-let ((tag (orgqda--otag-at-this-headline)))
    (let ((text (substring-no-properties
                 (org-agenda-get-some-entry-text (point-marker) 1))))
      (cons tag (if (string-blank-p text) nil text)))))

;;;; Clicking on tags should open a orgqda tag view

;; we could as well add-to-list this fn to
;; org-open-at-point-functions, as it checks for orgqda-mode.

(defun orgqda-collect-tags-at-point ()
  "In ‘orgqda-mode’ this function call ‘orgqda-collect-tagged’.

Supposed to be run as one of the hooks in
 ‘org-open-at-point-functions’ for the single tag at point."
  (when orgqda-mode
    (when-let ((tag (orgqda--tag-at-point)))
      (orgqda-collect-tagged tag)
      t)))

;;;; Advice

;; This advice is irrelevant if orgqda-helm-tags.el (which overrides
;; ‘org-set-tags’) is used

;;;###autoload
(advice-add 'org-global-tags-completion-table :around #'orgqda-tags-completion-table-wrap)
;;;###autoload
(defun orgqda-tags-completion-table-wrap (oldfun &rest args)
  "Around advice for ‘org-global-tags-completion-table’ (OLDFUN).
In ‘orgqda-mode’, load tags from ‘orgqda-tag-files’ and inhibits
org hooks to speed up loading of files. Else just call OLDFUN
with ARGS."
  (if orgqda-mode
      (orgqda--inhibit-org-startups
       (funcall oldfun (orgqda-tag-files)))
    (apply oldfun args)))


(provide 'orgqda)

;;; orgqda.el ends here
