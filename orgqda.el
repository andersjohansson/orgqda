;;; orgqda.el --- Qualitative data analysis using org-mode

;; Copyright (C) 2015 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2014-10-12
;; Modified: 2015-11-27
;; Package-Requires: ((xah-replace-pairs "2.0"))
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


;;(require 'cl-extra) ;;autoloaded?
(require 'xah-replace-pairs)
(require 'bookmark)
(require 'org-inlinetask)

(eval-when-compile
  (require 'cl-macs))

;;; Variables
(defgroup orgqda '((orgqda-mode custom-variable))
  "Customizations for orgqda-mode"
  :group 'org)

;;;###autoload
(defcustom orgqda-csv-dir "~"
  "for saving csv-files"
  :type 'directory
  :group 'orgqda)

(defcustom orgqda-tag-collect-extra-info nil
  "Possibly adds extra info to extracted tagged parts.
An alist where keys are regexps to be matched against buffer-name
and values are lisp forms that are evaluated to get extra info.

An example that adds a possible parent heading on level 4 for
buffers with names containing 'fieldnotes':
'((\"fieldnotes\" . (format \" (from: %s)\" (orgqda-get-parent-hl 4))))
"
  :type '(alist :key-type regexp :value-type sexp))

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

;;;###autoload
(defvar orgqda-tag-files nil
  "Extra files from which tags should be fetched for completion.
A list of files and directories, or a the name of a file
containing such a list. Relative paths in such a file are read as
relative to the file itself.

For directories, all .org-files (matched by
`org-agenda-file-regexp') are added.")
;;;###autoload
(make-variable-buffer-local 'orgqda-tag-files)
;;;###autoload
(put 'orgqda-tag-files 'safe-local-variable
	 (lambda (arg) (or (stringp arg) (and (listp arg) (cl-every 'stringp arg)))))

(defvar orgqda-mode-map nil
  "Local keymap for orgqda-mode")

;;; Interactive commands

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
  "Call `orqda-insert-inlinetask` with coding option and title \"∈\".
Prefix arg is passed through."
  (interactive "P")
  (orgqda-insert-inlinetask arg "∈" t))

;;;###autoload
(defun orgqda-list-buffer-tags (&optional alpha)
  "List all tags with counts, in a buffer and possibly all files
in `orgqda-tag-files'. Sorted by count or alphabetically
if optional (prefix) argument is non-nil."
  (interactive "P")
  (let ((tcl (orgqda--buffer-tags-list alpha))
        (fn (buffer-file-name)))
    (switch-to-buffer-other-window (generate-new-buffer "*orgqda-taglist*"))
    (mapc (lambda (x)
            (insert (format "- [[otag:%s:%s][%s]] (%d)\n"
                            fn (car x) (car x) (cadr x))))
          tcl)
    (goto-char (point-min))
    (org-mode)))

;;;###autoload
(defun orgqda-list-buffer-tags-full (&optional alpha)
  "List all tags with counts, in a buffer and possibly all files
in `orgqda-tag-files'. Insert extracted paragraphs as a subtree for all tags.
Sorted by count or alphabetically if optional (prefix) argument is t."
  (interactive "P")
  (let ((tcl (orgqda--buffer-tags-list alpha))
		(fn (buffer-file-name))
		(cb (current-buffer)))
	(switch-to-buffer-other-window
     (generate-new-buffer "*orgqda-taglist-full*"))
	(mapc (lambda (x)
			(let* ((todo-only nil)
                   (matcher (org-make-tags-matcher (car x))))
			  (insert (format "* [[otag:%s:%s][%s]] (%d)\n%s"
							  fn (car x) (car x) (cadr x)
							  (with-current-buffer cb
                                (cdr (orgqda--coll-tagged matcher)))))))
		  tcl)
	(goto-char (point-min))
	(org-mode)))

;;;###autoload
(defun orgqda-collect-tagged (&optional match)
  (interactive)
  (let* ((todo-only nil)
		 (matcher (org-make-tags-matcher match))
		 (mname (car matcher))
		 (cont (orgqda--coll-tagged matcher)))
	(switch-to-buffer-other-window (generate-new-buffer
                                    (format "*tags:%s*" mname)))
	;;(insert (format "* Taggat: %s " mname))
	(org-insert-time-stamp (current-time) t t
                           (format "* Taggat: %s, (%d) " mname (car cont)) "\n")
	(insert (cdr cont))
    (goto-char (point-min))
    (org-mode)
    (org-content 2)))
;; TODO, maybe make level of org-content here customizable and in that case depend on
;; if we have manyfiles: level can be let along the lines of:
;; (level (if (and orgqda-collect-from-all-files orgqda-tag-files) 2 1))

;;;###autoload
(defun orgqda-collect-tagged-csv (&optional match)
  (interactive)
  (let* ((todo-only nil)
		 (matcher (org-make-tags-matcher match))
		 (orgqda--csv-curr-mname (car matcher))
		 (cont (orgqda--coll-tagged-csv matcher)))
	(switch-to-buffer-other-window
     (generate-new-buffer (format "*csvtags:%s*" orgqda--csv-curr-mname)))
	(when orgqda-convert-csv-to-encoding
      (orgqda--csv-convert-buffer-to-encoding))
	(insert cont)
	(goto-char (point-min))
	;; (csv-mode)))
	))

;;TODO AUTOLOAD neccessary?
;;;###autoload
(defvar orgqda--csv-curr-mname nil)

;;;###autoload
(defun orgqda-collect-tagged-csv-save (&optional match)
  "Collect  and save a file in `orgqda-csv-dir'"
  (interactive)
  (let* ((todo-only nil)
		 (matcher (org-make-tags-matcher match))
		 (orgqda--csv-curr-mname (car matcher))
		 (cont (orgqda--coll-tagged-csv matcher)))
	(with-temp-buffer
	  (insert cont)
      (when orgqda-convert-csv-to-encoding
        (orgqda--csv-convert-buffer-to-encoding))
      (write-region (point-min) (point-max)
					(concat orgqda-csv-dir orgqda--csv-curr-mname ".csv")))))


;;;###autoload
(defun orgqda-collect-tagged-csv-save-all (&optional threshold)
  "Save all tags used THRESHOLD or more times in csv-files (one
per tag) in `orgqda-csv-dir'"
  (interactive "P")
  (let ((tags (orgqda--buffer-tags-list))
		(tn (prefix-numeric-value threshold))
		tc)
	(dolist (tc tags)
	  (when (or (not threshold)
				(<= tn (cadr tc)))
		(orgqda-collect-tagged-csv-save (car tc))))))



;;; internal functions
;;;; collection-functions
(defun orgqda--coll-tagged (matcher)
  "Return cons-cell with total count as car and string of taglists as cdr"
  (let* ((manyfiles (and orgqda-collect-from-all-files (orgqda-tag-files)))
		 (level (if manyfiles 3 2))
		 (totalcount 0)
         tags
		 (str
          ;; iterate over files assume this file is included in
          ;; orgqda-tag-files (or shouldn't be)
          (if manyfiles
              (let ;avoid running unneccesary hooks on extra selected files
                  ((org-mode-hook nil)
                   (text-mode-hook nil))
                (dolist (file manyfiles tags)
                  (with-current-buffer (find-file-noselect file)
                    (let ((ct (orgqda--coll-tagged-in-buffer matcher level)))
                      (setq totalcount (+ totalcount (car ct)))
                      (setq tags (concat tags
                                         (orgqda--taglist-file-heading (car ct))
                                         (cdr ct)
                                         "\n\n"))))))
            ;;only this buffer
            (let ((ct (orgqda--coll-tagged-in-buffer matcher level)))
              (setq totalcount (+ totalcount (car ct)))
              (cdr ct)))))
    (cons totalcount str)))

  (defun orgqda--taglist-file-heading (number)
    (let ((fl (abbreviate-file-name
               (buffer-file-name (buffer-base-buffer)))))
      (format "** [[file:%s][%s]] (%d)\n"
              fl (file-name-nondirectory fl) number)))

(defvar orgqda--ct-level 2)

(defun orgqda--coll-tagged-in-buffer (matcher &optional level)
  "Returns cons-cell with count in buffer as car and string of taglist as cdr."
  (let ((orgqda--ct-level (or level 2)))
	(save-excursion
	  (save-restriction
		(widen) (goto-char (point-min))
		(let ((tl (org-scan-tags 'orgqda--get-paragraph-or-sub
								 (cdr matcher) nil)))
		  (cons (length tl)
				(mapconcat 'identity tl "\n\n")))))))



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
		  ;;sätt ihop det
		  (concat hl contents))
	  "Inte heading eller inlinetask???")))


;;;; CSV-collection-functions
;;TODO, perhaps more duplication could be avoided
(defun orgqda--coll-tagged-csv (matcher)
  "Return body of csv"
  (let* ((manyfiles (and orgqda-collect-from-all-files (orgqda-tag-files)))
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
  (save-excursion
	(save-restriction
	  (widen) (goto-char (point-min))
	  (let ((tl (org-scan-tags 'orgqda--get-paragraph-or-sub-to-csv
							   (cdr matcher) nil)))
		(mapconcat 'identity tl "")))))

(defun orgqda--get-paragraph-or-sub-to-csv () 
  (save-excursion
	(if (or (org-at-heading-p) (org-inlinetask-in-task-p))
		(let* ((ln (line-number-at-pos))
			   (fl (file-name-base
					(abbreviate-file-name
                     (buffer-file-name (buffer-base-buffer)))))
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
                (format "\"%s\","
                        (replace-regexp-in-string
                         "\\\"" "»" (orgqda--get-paragraph-or-sub-contents))))
               (secondary
                (format "\"%s\",\"%s:%s\",\"%s:%s\",\"%s\",\"%s\""
                        fl fl ln fl (secure-hash 'md5 contents)
                        head orgqda--csv-curr-mname)))
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
  (require 'subr-x)
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
						(forward-line -1) (point)))))
	((org-at-heading-p)
	 (org-copy-subtree)
	 (with-temp-buffer
	   (insert "*")
	   (org-paste-subtree nil nil nil t)
	   (forward-line 1)
	   (buffer-substring-no-properties
		(point) (point-max)))))))


;; inspired by: http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
(defun orgqda--backward-paragraph ()
  "Go back to last blank line."
  (skip-chars-backward "\n[:blank:]")
  (if (search-backward-regexp
       "\n[[:blank:]]*\n[[:blank:]]*" nil t 1)
      (goto-char (match-end 0))
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
(org-add-link-type "opbm" 'orgqda-opbm-open 'orgqda-link-desc-export)
(defun orgqda-opbm-open (opbm)
  (save-current-buffer
	(let ((bm (cons "n" (read (xah-replace-pairs-in-string
							   opbm orgqda-bm-link-decode-table)))))
      (find-file-other-window (bookmark-get-filename bm))
      (bookmark-default-handler bm)
      (recenter)))
  (switch-to-buffer-other-window (current-buffer))) ; extremt hackigt

(defun orgqda-link-desc-export (link desc format)
  "We can't export these links nicely, always export desc."
  (format
   (cl-case format
     (latex "\\emph{%s}")
     (html "<em>%s</em>")
     (t "%s"))
   (substring-no-properties desc)))
(defvar orgqda-bm-link-encode-table
  '(["\n" "\\\\n"] ["[" "᚛"] ["]" "᚜"]))
(defvar orgqda-bm-link-decode-table
  '(["᚛" "["] ["᚜" "]"]))

(defun orgqda-get-bm ()
  (require 'bookmark)
  ;;(require 'xfrp_find_replace_pairs)
  (xah-replace-pairs-in-string (prin1-to-string
                                (bookmark-make-record-default))
                               orgqda-bm-link-encode-table))

;; (defun orgqda-goto-prop-bm ()
;;   (let ((bm (cons "name" (read (cdr (assoc "AJFBM" (org-entry-properties)))))))
;; 	(bookmark-jump-other-window bm)))



;;(add-hook 'org-store-link-functions 'orgqda-ofl-store-link)



;;;; List-buffer-tags-functions

(defun orgqda--buffer-tags-list (&optional alpha)
  "Return an alist of all tags with with counts, in a buffer and possibly all files
in `orgqda-tag-files'. Sorted by count or alphabetically
if optional (prefix) argument is t."
  (let ((tagscount (make-hash-table :test 'equal))
        (manyfiles (and orgqda-collect-from-all-files (orgqda-tag-files)))
		tcl)
    (if manyfiles ;list only from orgqda-tag-files
        (dolist (file manyfiles tagscount)
          (with-temp-buffer
            (insert-file-contents file)
            (setq tagscount (orgqda--get-tags-with-count tagscount))))
      ;;only this buffer
      (setq tagscount (orgqda--get-tags-with-count tagscount)))
    (setq tcl (orgqda--hash-to-list tagscount))
	(if alpha
		(sort tcl (lambda (a b) (string< (car a) (car b))))
	  (sort tcl (lambda (a b) (> (cadr a) (cadr b)))))))

(defun orgqda--get-tags-with-count (tagscount)
  "Expects a hash-table TAGSCOUNT and returns it modified"
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
			(org-re "[ \t]:\\([[:alnum:]_@#%:]+\\):[ \t\r\n]") nil t)
	  (when (equal (char-after (point-at-bol 0)) ?*)
		(mapc (lambda (x)
				(let ((ov (gethash x tagscount 0))) ; gethash def=0
				  (puthash x (1+ ov) tagscount)))
			  (org-split-string (org-match-string-no-properties 1) ":")))))
  tagscount)


;;från: http://ergoemacs.org/emacs/elisp_hash_table.html
(defun orgqda--hash-to-list (hashtable)
  "Return a list that represent the HASHTABLE."
  (let (myList)
    (maphash (lambda (kk vv) (setq myList (cons (list kk vv) myList))) hashtable)
	myList))

;;;;; link type for taglist

(org-add-link-type "otag" 'orgqda-otag-open 'orgqda-link-desc-export) ;;link to org-tag display
(add-hook 'org-store-link-functions 'orgqda-otag-store-link)

(defun orgqda-otag-open (otag)
  "Visit line number in file"
  (let ((fln (split-string otag ":")))
                                        ;(find-file (car fln))
	(with-current-buffer (find-file-noselect (car fln))
	  (orgqda-collect-tagged (cadr fln)))))

(defun orgqda-otag-store-link ()
  "Store a link to a org-mode file and tag."
  (let* ((oir (org-in-regexp (org-re "\\(:[[:alnum:]_@#%:]+\\):[ \t]*$"))))
	(when (and (equal major-mode 'org-mode) oir)
	  (let* ((fn (buffer-file-name))
			 (tagpos (org-between-regexps-p ":" ":" (car oir) (cdr oir)))
			 (tag (buffer-substring-no-properties (1+ (car tagpos)) (1- (cdr tagpos))))
			 (link (format "otag:%s:%s" fn tag)))
		(org-store-link-props
		 :type "otag"
		 :link link
		 :description tag)))))


;;; internal functions
(defun orgqda-tag-files (&optional unrestricted archives)
  "Returns list of files in addition to the current which should
be searched for tags. Based on value of variable
`orgqda-tag-files' which could be a list of files and
directories, or a file containing such a list.

Used for overriding `org-agenda-files' when `orgqda-mode' is
active.

Parameters have no meaning and are just for compatibility with
org-agenda-files."
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
                 (mapcar
                  (lambda (f)
                    (if (file-directory-p f)
                        (directory-files f t org-agenda-file-regexp)
                      (list f))) files)))
    ;; delete unreadable and current file (silently)
    (setq files (delq nil
                      (mapcar
                       (function
                        (lambda (file)
                          (and (file-readable-p file)
                               ;; (not (string= file
                               ;; (buffer-file-name)));;TODO, tror
                               ;; inte att den ska vara där i alla fall
                               file)))
                       files)))
    files))

;;; internal variables

(defvar orgqda--save-ctao nil)

;;; minor mode definition
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

COMMANDS??"
  ;;TODO Dok ^
  :lighter " QDA"
  :global t
  :keymap orgqda-mode-map
  :group 'orgqda
  (if orgqda-mode
      (progn
        (setq orgqda--save-ctao org-complete-tags-always-offer-all-agenda-tags)
        (when orgqda-tag-files
          (setq org-complete-tags-always-offer-all-agenda-tags t)
          (advice-add 'org-agenda-files :override #'orgqda-tag-files)))
    ;;deactivate
    (advice-remove 'org-agenda-files #'orgqda-tag-files)
    (setq org-complete-tags-always-offer-all-agenda-tags orgqda--save-ctao)))

;;; Keybindings
(unless orgqda-mode-map
  (setq orgqda-mode-map (make-sparse-keymap)))

(define-key orgqda-mode-map (kbd "C-c C-x m") #'orgqda-insert-inlinetask)
(define-key orgqda-mode-map (kbd "C-c C-x n") #'orgqda-insert-inlinetask-coding)


(provide 'orgqda)

;;; orgqda.el ends here
