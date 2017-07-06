;;; orgqda.el --- Qualitative data analysis using org-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2014-10-12
;; Modified: 2017-06-06
;; Package-Requires: ((emacs "25.1") (xah-replace-pairs "2.0") (org "9.0") (hierarchy "0.6.0"))
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
;; orgqda defines a minor mode and several commands for making and
;; collection of coded snippets possible in text written in org-mode.
;; It works in a simple (and perhaps stupid) way by viewing org-mode
;; tags added to degenerate inlinetasks as applying to the preceding
;; paragraph.

(require 'xah-replace-pairs)
(require 'bookmark)
(require 'org-inlinetask)
(require 'cl-lib)
(require 'subr-x) ;if-let
(require 'hierarchy)

;;; Variables
(defgroup orgqda nil
  "Customizations for orgqda-mode"
  :group 'org)

;;;###autoload
(defcustom orgqda-csv-dir "~"
  "for saving csv-files"
  :type 'directory
  :group 'orgqda
  :safe 'file-directory-p)

(defcustom orgqda-tag-collect-extra-info nil
  "Possibly adds extra info to extracted tagged parts.
An alist where keys are regexps to be matched against buffer-name
and values are lisp forms that are evaluated to get extra info.

An example that adds a possible parent heading on level 4 for
buffers with names containing 'fieldnotes':
'((\"fieldnotes\" . (format \" (from: %s)\" (orgqda-get-parent-hl 4))))
"
  :type '(alist :key-type regexp :value-type sexp)
  :group 'orgqda)

(defcustom orgqda-collect-from-all-files t
  "Whether the tag listing and collection commands in orgqda should
collect tags and tagged parts from all files defined in `orgqda-tag-files'"
  :type 'boolean
  :group 'orgqda)

(defcustom orgqda-tag-collect-extra-info-csv nil
  "Like `orgqda-tag-collect-extra-info' for the first extra-field
in csv-export."
  :type '(alist :key-type regexp :value-type sexp)
  :group 'orgqda)

(defcustom orgqda-tag-collect-extra-info2-csv nil
  "Like `orgqda-tag-collect-extra-info' for the second extra-field
in csv-export."
  :type '(alist :key-type regexp :value-type sexp)
  :group 'orgqda)

(defcustom orgqda-convert-csv-to-encoding nil
  "Encoding to use for saved csv files By default csv files are
saved with the encoding used by emacs or the files. Setting this
to a symbol that represents another encoding will ensure all
characters are of this encoding and replace those that are not by
? in saved csv files. t is a shortcut for
\"iso-8859-1\""
  :type (append
         '(choice)
         '((const nil :tag "don't convert") )
         '((const t :tag "iso-8859-1"))
         (mapcar
          (lambda (cs) (list 'const cs))
          coding-system-list))
  :group 'orgqda)

(defcustom orgqda-exclude-tags nil
  "Tags to exclude when listing coding tags in `orgqda-list-tags'
  and `orgqda-list-tags-full'"
  :type '(choice
          (const :tag "None" nil)
          (repeat :tag "List of tags" string))
  :safe #'orgqda--list-of-strings-p)

(defcustom orgqda-hierarchy-delimiter "_"
  "The character to use for delimiting a hierarchy within tags.
One of: _@#%"
  :type '(choice (const :tag "_" "_")
                 (const :tag "@" "@")
                 (const :tag "#" "#")
                 (const :tag "%" "%")))

(defcustom orgqda-use-tag-hierarchy t
  "If tags delimited with `orgqda-hierarchy-delimiter' should be considered grouped.
Currently only works for one level."
  :type 'boolean
  :group 'orgqda)

(defcustom orgqda-exclude-empty-file-trees t
  "When non-nil, excludes listing files where the current tag was not found when collecting occurences of tags"
  :type 'boolean
  :group 'orgqda)

;;;###autoload
(defvar-local orgqda-tag-files nil
  "Extra files from which tags should be fetched for completion.
A list of files and directories, or the name of a file
containing such a list. Relative paths in such a file are read as
relative to the file itself.

For directories, all .org-files (matched by
`org-agenda-file-regexp') are added.

Usually set by the user as a file or dir local variable.")
;;;###autoload
(put 'orgqda-tag-files 'safe-local-variable
	 #'orgqda--string-or-list-of-strings-p)

;;;###autoload
(defvar-local orgqda-codebook-file nil
  "A file that is used as a codebook, including lists of
tags (otag-links) This file will be updated when tags are
renamed.

Usually set by the user as a file or dir local variable.")
;;;###autoload
(put 'orgqda-codebook-file 'safe-local-variable
	 #'stringp)

(defun orgqda--string-or-list-of-strings-p (arg)
  "Returns t if ARG is a string or a list of strings"
  (or (stringp arg)
      (orgqda--list-of-strings-p arg)))

(defun orgqda--list-of-strings-p (arg)
  "Returns t if ARG is a list of strings"
  (and (listp arg) (cl-every 'stringp arg)))

;;;###autoload
(defvar-local orgqda--originating-buffer nil
  "Buffer where the call for the current orgqda tag listing or
  collected regions listing were made")
(defvar-local orgqda--taglist-sort-alpha nil
  "Whether current taglist buffer is alphabetically sorted")
(defvar-local orgqda--taglist-full nil
  "Whether current taglist buffer includes extracts")

;;;###autoload
(defvar-local orgqda--old-org-current-tag-alist nil
  "Saves state of `org-current-tag-alist' between enabling and disabling `orgqda-mode'.")

;;; KEYBINDINGS
;;;###autoload
(defvar orgqda-mode-map nil
  "Local keymap for orgqda-mode")
;;;###autoload
(unless orgqda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x m") #'orgqda-insert-inlinetask)
    (define-key map (kbd "C-c C-x n") #'orgqda-insert-inlinetask-coding)
    (setq orgqda-mode-map map)))

(defvar orgqda-list-mode-map nil
  "Local keymap for orgqda-list-mode")
;;;###autoload
(unless orgqda-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<drag-mouse-1>") #'orgqda-drag-merge-tags)
    (define-key map (kbd "R") #'orgqda-rename-tag)
    (define-key map (kbd "P") #'orgqda-prefix-tag)
    (define-key map (kbd "g") #'orgqda-revert-taglist)
    (define-key map (kbd "q") #'kill-this-buffer)
    (setq orgqda-list-mode-map map)))

;;; Macros
(defmacro orgqda--temp-work (&rest body)
  "Shortcut for (`save-excursion' (`save-restriction' (`widen') (`goto-char') (`point-min')))"
  `(save-excursion
     (save-restriction
       (widen) (goto-char (point-min))
       ,@body)))

(defmacro orgqda--with-current-buffer-if (buffer &rest body)
  "Execute BODY in BUFFER if it exists, otherwise just execute BODY.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  `(if (and ,buffer (bufferp ,buffer))
       (with-current-buffer ,buffer ,@body)
     ,@body))

;;; Minor mode definitions
;;;###autoload
(define-minor-mode orgqda-mode
  "Toggle orgqda mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

Minor mode for qualitative coding of text material and extraction
of codes.

Enables tag completion with tags from all files defined in `orgqda-tag-files'

Relevant commands not bound to any keys are:
`orgqda-list-tags', `orgqda-list-tags-full' and `orgqda-collect-tagged'.

CSV files can be exported as well with
`orgqda-collect-tagged-csv', `orgqda-collect-tagged-csv-save',
and `orgqda-collect-tagged-csv-save-all'. Be sure to customize
`orgqda-csv-dir' first.

\\{orgqda-mode-map}"
  ;;TODO Dok ^
  :lighter " QDA"
  :keymap orgqda-mode-map
  :group 'orgqda
  (if orgqda-mode
      (progn
        (when orgqda-tag-files
          (setq-local org-complete-tags-always-offer-all-agenda-tags t))
        (setq orgqda--old-org-current-tag-alist org-current-tag-alist
              org-current-tag-alist nil)
        (setq-local org-open-at-point-functions '(orgqda-collect-tags-at-point)))
    (kill-local-variable 'org-complete-tags-always-offer-all-agenda-tags)
    (kill-local-variable 'org-open-at-point-functions)
    (setq org-current-tag-alist orgqda--old-org-current-tag-alist)))

;;;###autoload
(define-minor-mode orgqda-list-mode
  "Mode for displaying lists of tags in orgqda.

\\{orgqda-list-mode-map}"
  :keymap orgqda-list-mode-map
  :lighter " QDAl")

;;; Interactive commands
;;;; Commands for inserting
(autoload 'org-inlinetask-in-task-p "org-inlinetask")
;;;###autoload
(defun orgqda-insert-inlinetask (&optional arg title coding)
  "Insert a degenerate inlinetask after current paragraph or,
with non-nil prefix arg, after current line.

TITLE is a string defining a title for the inlinetask and CODING
if non-nil additionally calls `org-set-tags-command`."
  (interactive "P")
  (unless (org-inlinetask-in-task-p) ; add nothing if already in task
    (unless (and (bolp) (eolp)) ; only move if inside a line of text
      (if arg (forward-line) (forward-paragraph)))
    (unless (org-inlinetask-in-task-p) ; create inlinetask if not present
      (unless (and (bolp) (eolp)) (newline)) ;newline at e.g. eof
      (unless (and arg (bolp) (eolp)) (open-line 1)) ; open-line if at newline
      (insert (concat (make-string
                       (1+ org-inlinetask-min-level) 42) " " title))))
  (when (and coding (org-inlinetask-in-task-p))
    (org-set-tags-command)))

;;;###autoload
(defun orgqda-insert-inlinetask-coding (arg)
  "Call `orqda-insert-inlinetask' with coding option and title \"∈\".
Prefix arg is passed through."
  (interactive "P")
  (orgqda-insert-inlinetask arg "∈" t)
  (move-end-of-line nil))

;;;; Commands for listing tags

;;;###autoload
(defun orgqda-list-tags (&optional alpha full buf noupdate newbufname)
  "List all tags with counts, in this buffer and possibly all
files in `orgqda-tag-files'. Sorted by count or alphabetically if
optional (prefix) argument is non-nil. If buffer is provided as
second arg BUF overwrite this buffer with the list instead of
creating a new. The taglist is normally updated via `orgqda--create-hierarchical-taglist', but this can be prevented by giving a non-nil NOUPDATE.
Then a special list can be used by setting `orgqda--current-htl' suitably.
NEWBUFNAME gives name of new buffer"
  (interactive "P")
  (let ((origbuffer (current-buffer))
        (origfile (buffer-file-name)))
    (unless noupdate
      (orgqda--create-hierarchical-taglist (when alpha 'a-z)))
                                        ; TODO, expose more sort possibilities?
    (if buf
        (progn (switch-to-buffer buf)
               (widen)
               (delete-region (point-min) (point-max)))
      (switch-to-buffer-other-window
       (generate-new-buffer (or newbufname "*orgqda-taglist*"))))

    (orgqda--insert-hierarchical-taglist full origbuffer origfile)
    (goto-char (point-min))
    (org-mode) (orgqda-list-mode) (flyspell-mode -1)
    (setq buffer-read-only t
          orgqda--originating-buffer origbuffer
          orgqda--taglist-sort-alpha alpha
          orgqda--taglist-full full)))

;;;###autoload
(defun orgqda-list-tags-full (&optional alpha buf)
  "List all tags with counts, in this buffer and possibly all files
in `orgqda-tag-files'. Insert extracted paragraphs as a subtree for all tags.
Sorted by count or alphabetically if optional (prefix) argument is t."
  (interactive "P")
  (orgqda-list-tags alpha t buf))

(defun orgqda-revert-taglist ()
  "Reverts current `orgqda-list-mode' buffer.
If not in `orgqda-list-mode', calls
`orgqda-update-taglist-general'."
  (interactive)
  (if (and orgqda-list-mode orgqda--originating-buffer)
      (let ((cb (current-buffer))
            (pos (point)))
        (setq buffer-read-only nil)
        (with-current-buffer orgqda--originating-buffer
          (orgqda-list-tags orgqda--taglist-sort-alpha orgqda--taglist-full cb))
        (goto-char pos))
    (orgqda-update-taglist-general)))

;;;; Commands for collecting
;;;###autoload
(defun orgqda-collect-tagged (&optional match)
  "Collect all segments marked with tags matching MATCH,
In an interactive call, MATCH is prompted for."
  (interactive)
  (let* ((matcher (org-make-tags-matcher match))
         (mname (car matcher))
         (cont (orgqda--coll-tagged matcher 2)))
    (switch-to-buffer-other-window (generate-new-buffer
                                    (format "*tags:%s*" mname)))
    ;;(insert (format "* Taggat: %s " mname))
    (org-insert-time-stamp (current-time) t t
                           (format "* Taggat: %s, (%d) " mname (car cont)) "\n")
    (insert (cdr cont))
    (goto-char (point-min))
    (org-mode) (flyspell-mode -1) (setq buffer-read-only t)
    (org-content 2)))
;; TODO, maybe make level of org-content here customizable and in that case depend on
;; if we have manyfiles: level can be let along the lines of:
;; (level (if (and orgqda-collect-from-all-files orgqda-tag-files) 2 1))

(defvar orgqda--csv-curr-mname nil)

;;;###autoload
(defun orgqda-collect-tagged-csv (&optional match)
  (interactive)
  (let* ((matcher (org-make-tags-matcher match))
		 (orgqda--csv-curr-mname (car matcher))
		 (cont (orgqda--coll-tagged-csv matcher)))
	(switch-to-buffer-other-window
     (generate-new-buffer (format "*csvtags:%s*" orgqda--csv-curr-mname)))
	(when orgqda-convert-csv-to-encoding
      (orgqda--csv-convert-buffer-to-encoding))
	(insert cont)
	(goto-char (point-min))))

;;;###autoload
(defun orgqda-collect-tagged-csv-save (&optional match)
  "Collect  and save a file in `orgqda-csv-dir'"
  (interactive)
  (let* ((matcher (orgqda--make-simple-tags-matcher match))
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
  "Save all tags used THRESHOLD or more times in csv-files (one
per tag) in `orgqda-csv-dir'"
  (interactive "P")
  (let ((tags (orgqda--get-tags-hash))
		(tn (prefix-numeric-value threshold)))
    (maphash (lambda (tag count)
               (when (or (not threshold)
                         (<= tn count))
                 (orgqda-collect-tagged-csv-save tag)))
             tags)))


;;;; Commands for renaming tags
;;;###autoload
(defun orgqda-drag-merge-tags (ev)
  (interactive "e")
  (let* ((start (event-start ev))
         (end (event-end ev))
         (stag (orgqda--otag-at-point (posn-point start)))
         (etag (orgqda--otag-at-point (posn-point end))))
    (when (and stag etag
               (eq (posn-window start) (posn-window end))
               (y-or-n-p (format "Merge tags %s 🠖 %s" stag etag)))
      (orgqda-rename-tag stag etag))))

(defun orgqda-rename-tag (oldname newname)
  "Rename tag OLDNAME to NEWNAME in all current collection of orgqda files"
  (interactive (let ((complist (orgqda--get-tags-for-completion)))
                 (list
                  (completing-read "Old tag name: " complist nil nil (orgqda--otag-at-point))
                  (completing-read "New tag name: " (reverse complist) nil nil))))
  (let (repslist
        (cbfile (or orgqda-codebook-file
                    (orgqda--with-current-buffer-if orgqda--originating-buffer
                      orgqda-codebook-file))))
    (if-let ((manyfiles
              (orgqda--with-current-buffer-if orgqda--originating-buffer
                (and orgqda-collect-from-all-files (orgqda-tag-files)))))
        (orgqda--with-current-buffer-if orgqda--originating-buffer
          (dolist (file manyfiles)
            (with-current-buffer (find-file-noselect file)
              (push (cons (file-name-base file) (orgqda--rename-tag-in-buffer oldname newname))
                    repslist))))
      ;; only this buffer
      (push (cons (buffer-name) (orgqda--rename-tag-in-buffer oldname newname))
            repslist))
    (when cbfile
      (with-current-buffer (find-file-noselect cbfile)
        (push (cons
               (concat (file-name-base cbfile) " (links)")
               (orgqda--rename-tag-links-in-buffer oldname newname))
              repslist)))
    (setq repslist (nreverse repslist))
    (cl-loop with total = 0
             for x in repslist
             unless (= 0 (cdr x))
             do (cl-incf total (cdr x))
             and concat (concat (car x) " "
                                (propertize (number-to-string (cdr x))
                                            'face 'bold) ", ")
             into filesums
             finally do
             (message "Replaced %s → %s. Σ %s, %s"
                      (propertize oldname 'face 'italic)
                      (propertize newname 'face 'italic)
                      (propertize (number-to-string total) 'face 'bold)
                      filesums))
    (orgqda-revert-taglist)))

(defun orgqda-prefix-tag (oldname prefix)
  "Add a prefix PREFIX to existing tag OLDNAME in current
collection of orgqda files"
  (interactive (let* ((complist (orgqda--get-tags-for-completion))
                      (preflist (orgqda--get-prefixes-for-completion complist)))
                 (list
                  (completing-read "Old tag name: " complist nil nil (orgqda--otag-at-point))
                  (completing-read "Prefix:"  preflist nil nil))))
  (orgqda-rename-tag oldname (concat prefix orgqda-hierarchy-delimiter oldname)))


;;; internal functions
;;;; collection-functions
(defun orgqda--coll-tagged (matcher level)
  "Return cons-cell with total count as car and string of taglists as cdr"
  (let* ((manyfiles (and orgqda-collect-from-all-files (orgqda-tag-files)))
         (totalcount 0)
         tags
         (str
          ;; iterate over files assume this file is included in
          ;; orgqda-tag-files (or shouldn't be)
          (if manyfiles
              (let ;avoid running unneccesary hooks on extra selected
                  ;;files (e.g. flyspell causes noticeable delays)
                  ((org-mode-hook nil)
                   (text-mode-hook nil))
                (dolist (file manyfiles tags)
                  (with-current-buffer (find-file-noselect file)
                    (let ((ct (orgqda--coll-tagged-in-buffer matcher (1+ level))))
                      (unless (and orgqda-exclude-empty-file-trees
                                   (= 0 (car ct)))
                        (setq totalcount (+ totalcount (car ct)))
                        (setq tags (concat tags
                                           (orgqda--taglist-file-heading
                                            (car ct) level)
                                           (cdr ct)
                                           "\n\n")))))))
            ;;only this buffer
            (let ((ct (orgqda--coll-tagged-in-buffer matcher level)))
              (setq totalcount (+ totalcount (car ct)))
              (cdr ct)))))
    (cons totalcount str)))

(defun orgqda--taglist-file-heading (number level)
  (let ((fl (abbreviate-file-name
             (buffer-file-name (buffer-base-buffer)))))
    (format "%s [[file:%s][%s]] (%d)\n"
            (make-string level 42)
            fl (file-name-nondirectory fl) number)))

(defvar orgqda--ct-level 2
  "Level of headlines for tagcollection, (can't be passed to `orgqda--get-paragraph-or-sub')")

(defun orgqda--coll-tagged-in-buffer (matcher level)
  "Returns cons-cell with count in buffer as car and string of taglist as cdr."
  (let ((orgqda--ct-level level)
        (org-use-tag-inheritance nil))
    (orgqda--temp-work
      (let ((tl (org-scan-tags 'orgqda--get-paragraph-or-sub
                               (cdr matcher) nil)))
        (cons (length tl)
              (mapconcat 'identity tl "\n"))))))

(defun orgqda--get-paragraph-or-sub ()
  (save-excursion
	(if (or (org-at-heading-p) (org-inlinetask-in-task-p))
		(let* ((ln (line-number-at-pos))
			   (bm (orgqda-get-bm))
			   (link (format "opbm:%s" bm))
			   (head (substring-no-properties (org-get-heading)))
			   (ei1
				(and orgqda-tag-collect-extra-info
					 (assoc-default
					  (buffer-name)
                      orgqda-tag-collect-extra-info 'string-match-p)))
			   (extrainfo (when ei1 (eval ei1)))
			   (hl (format "%s %d: [[%s][%s]]%s\n"
                           (make-string orgqda--ct-level 42)
						   ln link head extrainfo))
			   (contents (orgqda--get-paragraph-or-sub-contents)))
		  (concat hl contents))
	  "Inte heading eller inlinetask???")))

;;;; CSV-collection-functions
;;TODO, perhaps more duplication could be avoided
(defun orgqda--coll-tagged-csv (matcher)
  "Return body of csv"
  (let ((manyfiles (and orgqda-collect-from-all-files (orgqda-tag-files)))
        tags)
	(concat
	 "citat,fil,fil:rad,file:hash,head,matchad,extra,extra2\n" ;;TODO, give reasonable names here
     ;; iterate orgqda-tag-files
     (if manyfiles
         (let ;avoid running unneccesary hooks on extra selected files
             ((org-mode-hook nil)
              (text-mode-hook nil))
           (dolist (file manyfiles tags)
             (with-current-buffer (find-file-noselect file)
               (setq tags
                     (concat tags
                             (orgqda--coll-tagged-in-buffer-csv matcher))))))
       ;;only this buffer
       (orgqda--coll-tagged-in-buffer-csv matcher)))))

(defun orgqda--coll-tagged-in-buffer-csv (matcher)
  "Returns cons-cell with count in buffer as car and string of taglist as cdr."
  (let ((org-use-tag-inheritance nil))
    (orgqda--temp-work
      (let ((tl (org-scan-tags 'orgqda--get-paragraph-or-sub-to-csv
                               (cdr matcher) nil)))
        (mapconcat 'identity tl "")))))

(defun orgqda--get-paragraph-or-sub-to-csv ()
  (save-excursion
	(if (or (org-at-heading-p) (org-inlinetask-in-task-p))
		(let* ((ln (line-number-at-pos))
			   (fl (file-name-base
					(buffer-file-name (buffer-base-buffer))))
                                        ;(bm (orgqda-get-bm))
                                        ;(link (format "opbm:%s" bm))
			   (head (substring-no-properties (org-get-heading)))
			   ;;(tags (org-get-tags));TODO Fixa listan på något smart sätt
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
			   (extrainfo2 (when ei2 (eval ei2)));TODO, det här går
										;typ sönder om extrainfo inte
										;är definierat
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
		  ;;sätt ihop det
		  (concat contents secondary extrainfo1 extrainfo2 "\n"))
      "Inte heading eller inlinetask???")))

(defun orgqda--csv-convert-buffer-to-encoding ()
  "Makes sure the current csv-buffer only contains characters
belonging to the encoding specified in
`orgqda-convert-csv-to-encoding' and sets the buffer's coding
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

;;;; common collection-functions

(defun orgqda--get-paragraph-or-sub-contents ()
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
       (with-temp-buffer
         (insert "*")
         (org-paste-subtree nil nil nil t)
         (forward-line 1)
         (buffer-substring-no-properties
          (point) (point-max))))))))

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

;;;; General helper-functions
(defun orgqda-get-parent-hl (level)
  (save-excursion
	(when (re-search-backward (format "^\\*\\{%d,%d\\} " level level) nil t)
      (substring-no-properties (org-get-heading t t)))))

(defun orgqda-inlinetask-in-degenerate-task-p ()
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


;;;; link types
(defvar orgqda-bm-link-encode-table
  '(["\n" "\\\\n"] ["[" "᚛"] ["]" "᚜"]))
(defvar orgqda-bm-link-decode-table
  '(["᚛" "["] ["᚜" "]"]))

(org-link-set-parameters "opbm"
                         :follow #'orgqda-opbm-open
                         :export #'orgqda-link-desc-export
                         :store #'orgqda-opbm-store-link)

(defun orgqda-opbm-store-link ()
  "Store a bookmark-link to a position in an org-mode-buffer."
  (when (and (equal major-mode 'org-mode)
             orgqda-mode)
    (org-store-link-props
     :type "opbm"
     :link (format "opbm:%s" (orgqda-get-bm))
     :description (format "bm at: %s:%s"
                          (buffer-file-name) (line-number-at-pos)))))

(defun orgqda-opbm-open (opbm)
  (save-current-buffer
	(let ((bm (cons "n" (read (xah-replace-pairs-in-string
							   opbm orgqda-bm-link-decode-table)))))
      (find-file-other-window (bookmark-get-filename bm))
      (bookmark-default-handler bm)
      (recenter)))
  (switch-to-buffer-other-window (current-buffer))) ; extremt hackigt

(defun orgqda-link-desc-export (_link desc format)
  "We can't export these links nicely, always export desc."
  (format
   (cl-case format
     (latex "\\emph{%s}")
     (html "<em>%s</em>")
     (t "%s"))
   (substring-no-properties desc)))

(defun orgqda-get-bm ()
  (require 'bookmark)
  ;;(require 'xfrp_find_replace_pairs)
  (xah-replace-pairs-in-string (prin1-to-string
                                (bookmark-make-record-default))
                               orgqda-bm-link-encode-table))

;;;; List tags functions
(defun orgqda--get-tags-hash ()
  "Return a hash of all tags with counts.
In this buffer or in all files in `orgqda-tag-files'."
  (let ((tagscount (make-hash-table :test 'equal))
        (manyfiles (and orgqda-collect-from-all-files (orgqda-tag-files))))
    (if manyfiles ;list only from orgqda-tag-files
        (dolist (file manyfiles tagscount)
          (let ((visiting (find-buffer-visiting file)))
            (if visiting
                (with-current-buffer visiting
                  (setq tagscount
                        (orgqda--get-tags-with-count tagscount)))
              (with-temp-buffer
                (insert-file-contents file)
                (setq tagscount (orgqda--get-tags-with-count tagscount))))))
      ;;only this buffer
      (setq tagscount (orgqda--get-tags-with-count tagscount)))
    (dolist (ex orgqda-exclude-tags)
      (remhash ex tagscount))
    tagscount))

(defun orgqda--get-tags-alist (&optional sort)
  "Return an alist of all tags with counts.
In this buffer or in all files in `orgqda-tag-files'.
Optional SORT can be symbols: count-decreasing, count-increasing,
a-z, or z-a."
  (let ((tl (cl-loop for k being the hash-keys of (orgqda--get-tags-hash)
                     using (hash-values v)
                     collect (cons k v))))
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
  "Variable holding the current hierarchical list of tags with
  counts")

(defun orgqda--create-hierarchical-taglist (&optional sort taghash)
  "Store a hierarchical taglist in `orgqda--current-htl'.
Optional SORT can be symbols: count-decreasing (default),
count-increasing, a-z, or z-a. TAGHASH specifies a custom taghash
not loaded with `orgqda--get-tags-hash'"
  (let ((taghash (or taghash (orgqda--get-tags-hash)))
        (sortfn
         (cl-case sort
           (count-decreasing #'orgqda--hierarchy-count-sort-decr)
           (count-increasing #'orgqda--hierarchy-count-sort-incr)
           (a-z #'orgqda--string-lessp)
           (z-a #'orgqda--string-greaterp)
           (t #'orgqda--hierarchy-count-sort-decr))))
    (setq orgqda--current-htl
          (orgqda--make-htl
           :counts taghash))
    (hierarchy-add-trees (orgqda--htl-hierarchy orgqda--current-htl)
                         (hash-table-keys taghash)
                         (if orgqda-use-tag-hierarchy
                             #'orgqda--hierarchy-parentfn
                           (lambda (_) nil)))
    (hierarchy-sort (orgqda--htl-hierarchy orgqda--current-htl) sortfn)))

(defun orgqda--insert-hierarchical-taglist (full origbuffer filename &optional startlevel)
  (hierarchy-map
   (hierarchy-labelfn-indent
    (lambda (item indent)
      (orgqda--insert-taglist-item
       item filename)
      (when (and full
                 (not (string=
                       orgqda-hierarchy-delimiter
                       (substring item -1))))
        (insert
         (with-current-buffer origbuffer
           (cdr (orgqda--coll-tagged (orgqda--make-simple-tags-matcher item)
                                     (+ 2 indent))))
         "\n")))
    "*")
   (orgqda--htl-hierarchy orgqda--current-htl)
   (or startlevel 0)))

(defun orgqda--insert-taglist-item (item filename)
  (insert
   (format "* [[otag:%s:%s][%s]] (%d)\n"
           filename
           (if (string=
                orgqda-hierarchy-delimiter
                (substring item -1))
               (concat "{" item "}") ;make regexp-match for prefix
             item)
           item
           (gethash item (orgqda--htl-counts orgqda--current-htl) 0))))

(defun orgqda--hierarchy-parentfn (tag)
  "Function returning parent of a tag. Also updates count of parent."
  (let* ((splittag (split-string tag orgqda-hierarchy-delimiter t)))
    (when (> (safe-length splittag) 1)
      (let* ((parent (replace-regexp-in-string (concat (car (last splittag)) orgqda-hierarchy-delimiter "?$") "" tag))
             (c (gethash tag (orgqda--htl-counts orgqda--current-htl) 0))
             (pc (gethash parent (orgqda--htl-counts orgqda--current-htl) 0)))
        (puthash parent (+ c pc) (orgqda--htl-counts orgqda--current-htl))
        parent))))

(defun orgqda--hierarchy-count-sort-decr (x y)
  (> (gethash x (orgqda--htl-counts orgqda--current-htl) -1)
     (gethash y (orgqda--htl-counts orgqda--current-htl) -2)))
(defun orgqda--hierarchy-count-sort-incr (x y)
  (< (gethash x (orgqda--htl-counts orgqda--current-htl) -2)
     (gethash y (orgqda--htl-counts orgqda--current-htl) -1)))


(defun orgqda--string-lessp (x y)
  "Case insensitive `string-lessp'"
  (string-lessp (downcase x) (downcase y)))
(defun orgqda--string-greaterp (x y)
  "Case insensitive `string-greaterp'"
  (string-greaterp (downcase x) (downcase y)))

(defun orgqda--get-tags-with-count (tagscount)
  "Counts occurences of org tags in buffer.
Expects a hash-table TAGSCOUNT and returns it modified"
  (orgqda--temp-work
    (while (re-search-forward
            "[ \t]:\\([[:alnum:]_@#%:]+\\):[ \t\r\n]" nil t)
      (when (equal (char-after (point-at-bol 0)) ?*)
        (dolist (x (split-string (match-string-no-properties 1) ":" t))
          (let ((ov (gethash x tagscount 0)))
            (puthash x (1+ ov) tagscount))))))
  tagscount)

(defun orgqda--make-simple-tags-matcher (tag)
  "Construct a tags matcher only matching a single tag.
Use for performance reasons since `org-make-tags-matcher'
generation takes too long with long tag names."
  (cons tag `(lambda (_todo tags-list _level)
               (setq org-cached-props nil)
               (member ,tag tags-list))))

;;;;; link type for taglist
(org-link-set-parameters "otag"
                         :follow #'orgqda-otag-open
                         :export #'orgqda-link-desc-export
                         :store #'orgqda-otag-store-link)

(defun orgqda-otag-open (otag)
  "Visit line number in file"
  (let ((fln (split-string otag ":")))
    (with-current-buffer (find-file-noselect (car fln))
	  (orgqda-collect-tagged (cadr fln)))))

(defun orgqda-otag-store-link ()
  "Store a link to a org-mode file and tag."
  (let* ((oir (org-in-regexp "\\(:[[:alnum:]_@#%:]+\\):[ \t]*$")))
	(when (and (equal major-mode 'org-mode) oir)
	  (let* ((fn (buffer-file-name))
			 (tagpos (org-between-regexps-p ":" ":" (car oir) (cdr oir)))
			 (tag (buffer-substring-no-properties (1+ (car tagpos)) (1- (cdr tagpos))))
			 (link (format "otag:%s:%s" fn tag)))
		(org-store-link-props
		 :type "otag"
		 :link link
		 :description tag)))))

;;;; Functions for taglist update
(defvar orgqda--pending-tag-count-replacements nil)

(defun orgqda-update-taglist-general ()
  "Updates taglists in any org buffer.
Expects to find otag-links and updates any count number
following them. If `orgqda--originating-buffer' is set, uses
that, otherwise assumes tags can be found in this buffer or the
ones defined by `orgqda-tag-files'.

Generates a list of \"new\" tags, tags not linked to in this
buffer."
  (let ((newtags (make-hash-table :test 'equal)))
    (orgqda--with-current-buffer-if
        orgqda--originating-buffer
      (orgqda--create-hierarchical-taglist))
    (setq orgqda--pending-tag-count-replacements nil)
    (save-match-data
      (org-element-map (org-element-parse-buffer) 'link #'orgqda--update-tag-count-link)
      ;; do the replacements
      (cl-loop for x in orgqda--pending-tag-count-replacements
               do
               (set-match-data (car x))
               (replace-match (cdr x))
               (set-match-data (car x) t)))
    ;; list new tags
    (maphash
     (lambda (key val)
       (unless (listp val)
         (puthash key val newtags)))
     (orgqda--htl-counts orgqda--current-htl))
    (unless (hash-table-empty-p newtags)
      (orgqda--create-hierarchical-taglist nil newtags)
      (orgqda-list-tags nil nil nil t "*Possible new tags*"))))

(defun orgqda--update-tag-count-link (link)
  (when (string= "otag" (org-element-property :type link))
    (let* ((sp (split-string (org-element-property :path link) ":"))
           (tag (or (cadr sp) (car sp)))
           (tag (if (string-match-p "^{[^{}]+}$" tag)
                    (substring tag 1 -1)
                  tag))
           (found (gethash tag (orgqda--htl-counts orgqda--current-htl)))
           (count (if (listp found) (car found) found))
           (count (concat "("
                          (if count (number-to-string count) "0?")
                          ")")))
      (when (and found (not (listp found)))
        ;;mark the ones found by making the count a list
        (puthash tag (list found) (orgqda--htl-counts orgqda--current-htl)))
      (save-excursion
        (goto-char (org-element-property :end link))
        (when (looking-at "([0-9]+)")
          (push (cons (match-data) count)
                orgqda--pending-tag-count-replacements))))))


;;;; Functions for rename commands
(defun orgqda--rename-tag-in-buffer (oldname newname)
  "Rename all ocurrences of OLDNAME as an org tag with NEWNAME.
Return number of replacements done."
  (let ((numberofreps 0))
    (orgqda--temp-work
      (while (search-forward (concat ":" oldname ":") nil t)
        (org-set-tags-to
         (cl-remove-duplicates
          (cl-substitute newname oldname (org-get-local-tags) :test 'string=)
          :test 'string=))
        (setq numberofreps (1+ numberofreps))))
    numberofreps))

(defun orgqda--rename-tag-links-in-buffer (old new)
  (orgqda--temp-work
    (cl-loop while (search-forward-regexp
                    (format "\\(\\[\\[otag:[^:]+:\\)%s\\]\\[%s\\]\\]"
                            old old) nil t)
             count (progn (replace-match (format "\\1%s][%s]]" new new)) t))))

(defun orgqda--get-tags-for-completion ()
  "Return current list of tags in orgqda (possibly many files)"
  (hash-table-keys
   (orgqda--with-current-buffer-if
       orgqda--originating-buffer
     (orgqda--get-tags-hash))))

(defun orgqda--get-prefixes-for-completion (&optional taglist)
  "Return the current list of tag prefixes
 (delimited with `orgqda-hierarchy-delimiter') for tags in
orgqda (possibly many files)

TAGLIST can be passed or else will be fetched with
`orgqda--get-tags-for-completion'"
  (let ((taglist (or taglist (orgqda--get-tags-for-completion)))
        prefixes)
    (dolist (tag taglist)
      (let* ((splittag (split-string tag orgqda-hierarchy-delimiter t))
             (cats (butlast splittag)))
        (when cats
          (setq prefixes
                (append prefixes (orgqda--build-prefixes cats))))))
    (cl-remove-duplicates prefixes :test 'string=)))

(defun orgqda--build-prefixes (preflist &optional pref)
  (when preflist
    (let ((curr (if pref
                    (concat pref orgqda-hierarchy-delimiter (car preflist))
                  (car preflist))))
      (cons curr
            (orgqda--build-prefixes
             (cdr preflist) orgqda-hierarchy-delimiter
             curr)))))

(defun orgqda--otag-at-point (&optional pos)
  "Get the tag name of the otag-link at point."
  (save-excursion
    (when pos (goto-char pos))
    (let ((context (org-element-lineage (org-element-context) '(link) t)))
      (when (and (eq (org-element-type context) 'link)
                 (string= (org-element-property :type context) "otag"))
        (cadr (split-string (org-element-property :path context) ":"))))))

;;;; Various functions
(defun orgqda-tag-files ()
  "Returns list of files which should be searched for tags. Based
on value of variable `orgqda-tag-files' which could be a list of
files and directories, or a file containing such a list.

Used for overriding `org-agenda-files' when `orgqda-mode' is
active."
  ;; Based on org-agenda-files
  (require 'org-agenda)
  (let ((files
         (cond
          ((stringp orgqda-tag-files)
           (let ((org-agenda-files orgqda-tag-files)
                 (org-directory
                  (file-name-directory (expand-file-name
                                        orgqda-tag-files))))
             ;; read it as an agenda file list
             (org-read-agenda-file-list)))
          ((listp orgqda-tag-files) orgqda-tag-files)
          (t (error "Invalid value of `orgqda-tag-files'")))))
    ;; expand directories
    (setq files
          (apply 'append
                 (mapcar ;;TODO, replace with loop?
                  (lambda (f)
                    (if (file-directory-p f)
                        (directory-files f t org-agenda-file-regexp)
                      (list f))) files)))
    ;; delete unreadable files
    (setq files (delq nil
                      (mapcar ;;TODO, replace with loop
                       (function
                        (lambda (file)
                          (and (file-readable-p file)
                               file)))
                       files)))
    files))


;;; Clicking on tags should open a orgqda tag view

;; we could as well add-to-list this fn to
;; org-open-at-point-functions, as it checks for orgqda-mode.

(defun orgqda-collect-tags-at-point ()
  "Supposed to be run as one of the hooks in `org-open-at-point-functions'

 In `orgqda-mode' this function calls `orgqda-collect-tagged'
 for the single tag at point."
  (when (and orgqda-mode
             (progn
  (save-excursion (beginning-of-line)
                               (looking-at org-complex-heading-regexp))
               (and (match-beginning 5)
                    (>= (point) (match-beginning 5)))))
    (let ((min (match-beginning 5))
          (max (point-at-eol)))
      (when-let ((end (save-excursion (search-forward ":" max t)))
                 (beg (save-excursion (search-backward ":" min t))))
        (orgqda-collect-tagged
         (buffer-substring-no-properties (1+ beg) (1- end)))
        t))))

;;; Advice

;;;###autoload
(advice-add 'org-agenda-files :around #'orgqda-agenda-files-override)
;;;###autoload
(defun orgqda-agenda-files-override (oldfun &rest args)
  "Overrides with `orgqda-tag-files' when `orgqda-mode' is non-nil."
  (if orgqda-mode
      (orgqda-tag-files)
    (apply oldfun args)))

;;;###autoload
(advice-add 'org-global-tags-completion-table :around #'orgqda-tags-completion-table-wrap)
;;;###autoload
(defun orgqda-tags-completion-table-wrap (oldfun &rest args)
  "Lets `org-mode-hook' and `text-mode-hook' be nil for the
execution to speed up loading of tags from related files when
`orgqda-mode' is non-nil."
  (if orgqda-mode
      (let ((org-mode-hook nil)
            (text-mode-hook nil))
        (apply oldfun args))
    (apply oldfun args)))

;; ;;;###autoload
;; (advice-add 'org-set-tags :around #'orgqda-set-tags-override)
;; ;;;###autoload
;; (defun orgqda-set-tags-override (oldfun &rest args)
;;   (let ((org-current-tag-alist
;;          (if orgqda-mode nil org-current-tag-alist)))
;;     (apply oldfun args)))

;;; We really need to avoid org-persistent-tags-alist.
;;; org-current-tag-alist is set when loading an org buffer and if
;;; org-persistent-tags-alist is nil prevents getting all buffer tags
;;; for completion both for current buffer in org-set-tags and for
;;; "agenda"-buffers in org-global-tags-completion-table (where files
;;; are loaded with find-file-noselect). We can't make sure all files
;;; get orgqda-mode enabled so the easiest thing is to use #+STARTUP:
;;; noptags in all files. But orgqda-mode will set
;;; org-current-tag-alist to nil as well.


(provide 'orgqda)

;;; orgqda.el ends here
