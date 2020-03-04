;;; orgqda-transcript.el --- Interview transcript mode for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Created: 2016-09-27
;; Modified: 2020-03-04
;; Package-Requires: ((mplayer-mode "2.0") (emacs "25.1"))
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
;; orgqda-transcript defines a minor mode and several commands and
;; keybindings for making interview transcriptions into a structured
;; org-mode file easy.

(require 'mplayer-mode)
(require 'subr-x) ; only when-let
(require 'cl-lib) ; only cl-every
(require 'cl-seq) ; only cl-member-if-not
(require 'org)
(require 'orgqda) ; only for a few convenience functions

;;; Custom variables
(defgroup orggqda-transcript nil
  "Settings for ‘orgqda-transcript-mode’"
  :group 'text)
(defcustom orgqda-transcript-bind-fn-keys nil
  "Whether to bind the F1,F2,... keys to useful commands in
‘orgqda-transcript-mode’"
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-transcript-bind-1-4-keys nil
  "Whether to bind keys 1-4 keys to useful commands in
‘orgqda-transcript-mode’"
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-transcript-set-up-speaker-keys nil
  "Whether to bind F5 up to maximally F12 to special functions
  for inserting speakers and timestamps in
  ‘orgqda-transcript-mode’"
  :type 'booleanp
  :safe #'booleanp)

(defcustom orgqda-transcript-set-up-speaker-keys-5-9 nil
  "Whether to bind keys from 5 up to maximally 0 to special functions
  for inserting speakers and timestamps in
  ‘orgqda-transcript-mode’"
  :type 'booleanp
  :safe #'booleanp)

(defcustom orgqda-transcript-rebind-c-s-ret nil
  "Whether to rebind C-S-<RET> to insert timestamp and speaker
name in ‘orgqda-transcript-mode’"
  :type 'boolean
  :safe #'booleanp)

(defcustom orgqda-transcript-rebind-s-ret nil
  "Whether to rebind S-<RET> to insert speaker name in
parenthesis in ‘orgqda-transcript-mode’"
  :type 'boolean
  :safe #'booleanp)

;;; Keybindings
;;;###autoload
(defvar orgqda-transcript-mode-map nil
  "Local keymap for orgqda-transcript-mode")
;;;###autoload
(unless orgqda-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key map (kbd "C-a") nil)
    (define-key map (kbd "C-c C-x n") #'orgqda-transcript-insert-inlinetask-coding)
    (setq orgqda-transcript-mode-map map)))

;;; Internal or locally defined variables.
(defvar orgqda-transcript-fn-bindings
  `(([f1] . mplayer-toggle-pause-with-rewind)
    ([f2] . mplayer-seek-backward)
    ([f3] . mplayer-seek-forward)
    ([f4] . orgqda-transcript-insert-link)
    (,(kbd "C-<f4>") . orgqda-transcript-seek-timestamp-backwards))
  "Bindings for first four Fn-keys.
Used in ‘orgqda-transcript-mode’ activation if
‘orgqda-transcript-bind-fn-keys’ it non-nil")

(defvar orgqda-transcript-1-4-bindings
  `(("1" . mplayer-toggle-pause-with-rewind)
    ("2" . mplayer-seek-backward)
    ("3" . mplayer-seek-forward)
    ("4" . orgqda-transcript-insert-link)
    (,(kbd "C-4") . orgqda-transcript-seek-timestamp-backwards))
  "Bindings for first four number-keys.
Used in ‘orgqda-transcript-mode’ activation if
‘orgqda-transcript-bind-number-keys’ it non-nil")

;;;###autoload
(defvar-local orgqda-transcript-namelist nil "List of the names
of speakers in current ‘orgqda-transcript-mode’ session")
;;;###autoload
(put 'orgqda-transcript-namelist 'safe-local-variable
	 #'orgqda--list-of-strings-p)

;;; Minor mode
;;;###autoload
(define-minor-mode orgqda-transcript-mode
  "Minor mode for transcribing audio or video into structured
org-mode files with the help of ‘mplayer-mode’

\\{orgqda-transcript-mode-map}"
  :lighter " OQT"
  :keymap orgqda-transcript-mode-map
  :group 'orgqda-transcript
  (when orgqda-transcript-mode
    (orgqda-transcript-set-up-commands-and-bindings)))

;;;###autoload
(defun orgqda-transcript-set-up-commands-and-bindings (&optional arg)
  "Define speaker-insertion commands and keys for ‘orgqda-transcript’.

Typically run when the mode is activated.

If ‘orgqda-transcript-bind-fn-keys’ is non-nil, binds F1-F4 to navigation commands etc.

If ‘orgqda-transcript-set-up-speaker-keys’ is non-nil, defines
<f[5-8]>, C-<f[5-8]>, S-<f[5-8], and C-S-<f[5-8]> as keys for
‘orgqda-transcript-insert-linebreak-speaker’
‘orgqda-transcript-insert-parenthesis-speaker’,
‘orgqda-transcript-insert-speaker’, and
‘orgqda-transcript-switch-speaker’ respectively, with names taken
in order from ‘orgqda-transcript-namelist’ or read from
minibuffer if ‘orgqda-transcript-namelist’ is empty or a
prefix-argument is given.

For example: S-<f6> will insert the name of the second
speaker (surrounded by **) <f5> will insert a newline, timestamp,
name of the first speaker and a colon.

If ‘orgqda-transcript-rebind-s-ret’ and
‘orgqda-transcript-rebind-c-s-ret’ are non-nil and
‘orgqda-transcript-namelist’ only contains two names,redefines
S-<RET> and C-S-<RET> to insert the next guessed name in
parenthesis and on a new line."
  (interactive "P")
  (let* ((namelist (if (and (called-interactively-p 'any)
                            (or arg (not orgqda-transcript-namelist)
                                (not (orgqda--list-of-strings-p
                                      orgqda-transcript-namelist))))
                       (list (read-minibuffer "Name list: " "(\"Me\" )"))
                     (or arg orgqda-transcript-namelist
                         (list "Me" "Informant"))))
         (count (safe-length namelist))
         (cc 5))
    ;; F1-F4
    (dolist (key orgqda-transcript-fn-bindings)
      (if orgqda-transcript-bind-fn-keys
          (define-key orgqda-transcript-mode-map (car key) (cdr key))
        (define-key orgqda-transcript-mode-map (car key) nil)))

    ;; 1-4
    (dolist (key orgqda-transcript-1-4-bindings)
      (if orgqda-transcript-bind-1-4-keys
          (define-key orgqda-transcript-mode-map (car key) (cdr key))
        (define-key orgqda-transcript-mode-map (car key) nil)))

    ;;F5-F12
    (if (and orgqda-transcript-set-up-speaker-keys (< 0 count 9))
        (dolist (name namelist)
          (let ((fkey (format "<f%d>" cc)))
            (define-key orgqda-transcript-mode-map (kbd fkey)
              (orgqda-transcript--make-speaker-fn name 'orgqda-transcript-insert-newline-ts-speaker))
            (define-key orgqda-transcript-mode-map (kbd (concat "C-" fkey))
              (orgqda-transcript--make-speaker-fn name 'orgqda-transcript-insert-parenthesis-speaker))
            (define-key orgqda-transcript-mode-map (kbd (concat "S-" fkey))
              (orgqda-transcript--make-speaker-fn name 'orgqda-transcript-insert-speaker))
            (define-key orgqda-transcript-mode-map (kbd (concat "C-S-" fkey))
              (orgqda-transcript--make-speaker-fn name 'orgqda-transcript-switch-speaker)))
          (setq cc (1+ cc)))
      ;; else disable
      (if (= count 0)
          (message "orgqda-trancript set up: no names!")
        (message "orgqda-transcript set up: More than 8 names, no keys for speakers bound."))
      (dotimes (i 8)
        (define-key orgqda-transcript-mode-map (kbd (format "<f%d>" (+ 5 i))) nil)
        (define-key orgqda-transcript-mode-map (kbd (format "C-<f%d>" (+ 5 i))) nil)
        (define-key orgqda-transcript-mode-map (kbd (format "S-<f%d>" (+ 5 i))) nil)
        (define-key orgqda-transcript-mode-map (kbd (format "C-S-<f%d>" (+ 5 i))) nil)))

    ;; same for 5-0-keys
    (setq cc 5)
    (if (and orgqda-transcript-set-up-speaker-keys-5-9 (< 0 count 7))
        (dolist (name namelist)
          (let ((fkey (format "%d" (mod cc 10))))
            (define-key orgqda-transcript-mode-map (kbd fkey)
              (orgqda-transcript--make-speaker-fn name 'orgqda-transcript-insert-newline-ts-speaker))
            (define-key orgqda-transcript-mode-map (kbd (concat "C-" fkey))
              (orgqda-transcript--make-speaker-fn name 'orgqda-transcript-insert-parenthesis-speaker))
            (define-key orgqda-transcript-mode-map (kbd (concat "S-" fkey))
              (orgqda-transcript--make-speaker-fn name 'orgqda-transcript-insert-speaker))
            (define-key orgqda-transcript-mode-map (kbd (concat "C-S-" fkey))
              (orgqda-transcript--make-speaker-fn name 'orgqda-transcript-switch-speaker)))
          (setq cc (1+ cc)))
      ;; else disable
      (if (= count 0)
          (message "orgqda-trancript set up: no names!")
        (message "orgqda-transcript set up: More than 6 names, no 5-0 keys for speakers bound."))
      (dotimes (i 6)
        (let ((key (mod (+ 5 i) 10)))
          (define-key orgqda-transcript-mode-map (kbd (format "%d" key)) nil)
          (define-key orgqda-transcript-mode-map (kbd (format "C-%d" key)) nil)
          ;; FIXME, this doesn’t work, since S shifts the symbol
          ;; (S-7=/). And this will depend on keyboard layout...
          ;; (define-key orgqda-transcript-mode-map (kbd (format "S-%d" key)) nil)
          ;; (define-key orgqda-transcript-mode-map (kbd (format "C-S-%d" key)) nil)
          )))
    ;; S-RET and C-S-RET
    (if (and orgqda-transcript-rebind-c-s-ret (eq count 2))
        (define-key orgqda-transcript-mode-map (kbd "<C-S-return>") #'orgqda-transcript-insert-newline-ts-other-speaker)
      (define-key orgqda-transcript-mode-map (kbd "<C-S-return>") nil))
    (if (and orgqda-transcript-rebind-s-ret (eq count 2))
        (define-key orgqda-transcript-mode-map (kbd "<S-return>") #'orgqda-transcript-insert-parenthesis-other-speaker)
      (define-key orgqda-transcript-mode-map (kbd "<S-return>") nil))))

;;; Commands (typically called with specially bound keys)

(defun orgqda-transcript-insert-newline-ts-other-speaker ()
  "Inserts newline, timestamp and other speaker name.

Gets other speaker through ‘orgqda-transcript--get-other-name’."
  (interactive)
  (orgqda-transcript-insert-newline-ts-speaker (or (orgqda-transcript--get-other-name) "WHO")))

(defun orgqda-transcript-insert-parenthesis-other-speaker (&optional statement)
  "Inserts the other speaker in parenthesis.

Gets other speaker through ‘orgqda-transcript--get-other-name’.
Runs only if there are just two names in
‘orgqda-transcript-namelist’. Optional STATEMENT is passed to
‘orgqda-transcript-insert-parenthesis-speaker’"
  (interactive)
  (when-let ((name (orgqda-transcript--get-other-name)))
    (orgqda-transcript-insert-parenthesis-speaker name statement)))

(defun orgqda-transcript-insert-speaker (name)
  "Inserts NAME as bold (between *) with colon and space appended."
  (interactive "MName:")
  (insert "*" name "*: "))

(defun orgqda-transcript-insert-newline-ts-speaker (name)
  "insert new line and timestamp plus speaker NAME."
  (interactive "MName:")
  (when-let ((link (orgqda-transcript--get-link)))
    (newline)
    (insert link " ")
    (orgqda-transcript-insert-speaker name)))

(defun orgqda-transcript-insert-parenthesis-speaker (name &optional statement)
  "Insert NAME in curly brackets.
The optional STATEMENT in non-interactive calls prints this
string and exits the parenthesis"
  (interactive "MName:")
  (if statement
      (insert "{*" name "*: " statement "} ")
    (insert "{*" name "*: }")
    (backward-char)))

(defun orgqda-transcript-switch-speaker (&optional newname)
  "Replace speaker name at or close to point.

Search is done in order here, backward, forward.
NEWNAME is prompted for if a name to replace is found."
  (interactive)
  (save-excursion
    (let* ((p (point))
           (reg "\\*[[:word:]]\\{1,20\\}\\*")
           (found (or
                   (org-in-regexp reg)
                   (search-backward-regexp reg (- p 200) t)
                   (search-forward-regexp reg (+ p 200) t))))
      (if found
          (progn
            (if (consp found)
                (delete-region (car found) (cdr found))
              (delete-region (match-beginning 0) (match-end 0)))
            (insert "*"
                    (or newname
                        (completing-read "New name: " orgqda-transcript-namelist))
                    "*"))
        (message "Found no name to switch")))))

(defun orgqda-transcript-insert-inlinetask-coding (arg)
  "Insert inlinetask for coding after current line
Calls ‘orqda-insert-inlinetask-coding’ with inverted prefix arg."
  (interactive "P")
  (orgqda-insert-inlinetask-coding (not arg)))

(defun orgqda-transcript-seek-timestamp-backwards ()
  "Look for a ‘oqdats’ timestamp backwards in current paragraph
and follow it if one is found."
  (interactive)
  (let ((limit (save-excursion (backward-paragraph) (point))))
    (save-excursion
      (if (search-backward "[[oqdats:" limit t)
          (org-open-at-point)
        (message "No timestamp found")))))

;; List speaker time
(defun orgqda-transcript-list-speaker-time ()
  (interactive)
  (let* ((range (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (cons (point-min) (point-max))))
         (tlist (orgqda-transcript--count-time (car range) (cdr range))))
	(switch-to-buffer-other-window (generate-new-buffer "*orqda-speaker-time*"))
	(mapc (lambda (x)
            (insert (format "- %s :: %s\n" (car x) (mplayer--format-time (cdr x) "%H:%M:%S"))))
		  tlist)
	(org-mode)))

;;; New beginning-of-line function, not bound by default
(defvar orgqda-transcript-boi-forward nil
  "non-nil for forward, nil for backward.")

(defun orgqda-transcript-beginning-or-indentation ()
  "Move cursor to beginning of this line or after first org-link.
If after first link, move to beginning of line.
If at beginning of line, move to beginning of previous line.
Else, move to indentation position of this line."
  (interactive)
  (cond ((bolp)
         (orgqda-transcript--go-to-first-link) (setq orgqda-transcript-boi-forward t))
        ((looking-back (concat "^" org-link-any-re) (point-at-bol))
         (if (and (eq last-command this-command)
                  orgqda-transcript-boi-forward)
             (orgqda-transcript--goto-after-speaker)
           (beginning-of-line)))
        ((looking-back (orgqda-transcript--speaker-name-re) (point-at-bol))
         (orgqda-transcript--go-to-first-link) (setq orgqda-transcript-boi-forward nil))
        (t
         (push-mark nil t) (orgqda-transcript--goto-after-speaker) (setq orgqda-transcript-boi-forward nil))))

(defun orgqda-transcript--goto-after-speaker ()
  (let ((p (point)))
    (beginning-of-line)
    (unless
        (search-forward-regexp (orgqda-transcript--speaker-name-re) (point-at-eol) t)
      (goto-char p))))


;;; Timestamp link, definitions.
;;;###autoload
(defun orgqda-transcript-insert-link ()
  "Insert a timestamp link which can be followed in ‘mplayer-mode’."
  (interactive)
  (when-let (ln (orgqda-transcript--get-link)) (insert ln " ")))

;;;###autoload
(defface oqdats-face
  `((t (:inherit org-link
                 :weight bold)))
  "Face for oqdats links in org-mode.")

(org-link-set-parameters "oqdats"
                         :follow #'orgqda-transcript-follow-link
                         :export #'orgqda-transcript-export-link
                         :store #'orgqda-transcript-store-link
                         :face 'oqdats-face)

(defun orgqda-transcript-follow-link (filetime)
  "Follow ‘oqdats’ timestamps with ‘mplayer-seek-position’."
  (let* ((ft (split-string filetime ":"))
         (pos (car ft))
         (file (cadr ft))
         (mp (process-live-p mplayer--process)))
    (if file
        (if mp
            (unless (string= file (mplayer--get-filename))
              (mplayer-quit-mplayer)
              (sleep-for 0.1)
              (mplayer-find-file file))
          (mplayer-find-file file))
      (unless mp
        (error "No file in link and no mplayer--process")))
    (mplayer-seek-position (string-to-number pos) t)))

;;;###autoload
(defun orgqda-transcript-export-link (path desc format)
  "Export ‘oqdats’ timestamps."
  (if (eq format 'latex)
      (format "\\ajts[%s]{%s}" path desc)
    (format "[%s]" desc)))

;;;###autoload
(defun orgqda-transcript-store-link ()
  "Store a link to a mplayer-mode position

Activates when ‘mplayer-mode’ and ‘orgqda-transcript-mode’ are
active"
  (when-let (lpl (orgqda-transcript--get-link-plist))
    (apply #'org-link-store-props lpl)))

(defun orgqda-transcript--get-link-plist ()
  "Returns the plist for an ‘oqdats’ link"
  (when (and mplayer-mode orgqda-transcript-mode)
    (cl-loop with time = nil with file = nil ;; with iter = 0
             repeat 3 do
             (setq time (mplayer--get-time)
                   file (mplayer--get-filename)
                   ;; iter (1+ iter)
                   )
             if (and time file)
             ;;do  (message "On try %d" iter) and
             return (list
                     :type "oqdats"
                     :link (format "oqdats:%s:%s" time file)
                     :description (mplayer--format-time time "%H:%M:%S"))
             else do (sit-for 0.05))))

(defun orgqda-transcript-clear-links ()
  "Clear oqdats links in region or whole buffer."
  (interactive)
  (let* ((region? (use-region-p))
         (beg (if region? (region-beginning) (point-min)))
         (end (if region? (region-end) (point-max))))
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (search-forward-regexp org-link-bracket-re end t)
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (when (string-match "^oqdats:" (match-string 1))
              (delete-region beg end)
              (when (looking-at "^[[:space:]]+")
                (replace-match "")))))))))


;;; Internal functions
;;;###autoload
(defun orgqda-transcript--make-speaker-fn (speaker fn)
  "Define an interactive function called SPEAKER-FN which calls
FN with SPEAKER as single argument."
  (let ((fname (concat (symbol-name fn) "-" (downcase speaker))))
    (unless (fboundp (intern fname))
      (fset (intern fname)
            (list 'lambda nil
                  (concat "Call `" (symbol-name fn) "' with \"" speaker "\" as single argument.")
                  '(interactive)
                  `(,fn ,speaker))))
    (intern fname)))

(defun orgqda-transcript--count-time (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((timec (make-hash-table :test 'equal))
            (tsreg "^\\[\\[oqdats:\\([0-9\\.]+\\)\\(:[^]]+\\)?]\\[[^]]+\\]\\]"))
        (while (search-forward-regexp tsreg nil t)
          (let* ((t1 (string-to-number (match-string 1)))
                 (p0 (point))
                 (pb (+ 50 (save-excursion (forward-paragraph) (point))))
                 (namn (if (search-forward-regexp " \\*\\([[:alpha:]]+\\)\\*:" (+ p0 25) t)
                           (match-string-no-properties 1) "OKLART")))
            (save-excursion
              (when (search-forward-regexp tsreg pb t)
                (let ((td (- (string-to-number (match-string 1)) t1))
                      (ov (gethash namn timec 0)))
                  (puthash namn (+ td ov) timec))))))
        (cl-sort (orgqda--hash-to-alist timec) '> :key 'cdr)))))

(defun orgqda-transcript--go-to-first-link ()
  (beginning-of-line 1)
  (when (looking-at (concat org-link-any-re))
    (goto-char (match-end 0))))

(defun orgqda-transcript--get-link ()
  (when-let ((linkpl (orgqda-transcript--get-link-plist)))
    (org-link-make-string
     (plist-get linkpl :link)
     (plist-get linkpl :description))))

(defun orgqda-transcript--get-other-name ()
  "Get the name of the next, or other speaker.
Checks the speaker name at beginning of line and returns the
other name if ‘orgqda-transcript-namelist’ contains only two
names, otherwise returns nil"
  (when (eq 2 (safe-length orgqda-transcript-namelist))
    (let ((prev-name
           (save-excursion
             (when (search-backward-regexp (orgqda-transcript--speaker-name-re)
                                           (point-at-bol -3) t)
               (match-string 9)))))
      (when-let ((prevpos (cl-position prev-name orgqda-transcript-namelist :test #'equal)))
        (nth (mod (1+ prevpos) 2) orgqda-transcript-namelist)))))

(defun orgqda-transcript--speaker-name-re ()
  "Return regexp for speaker name at beginning of line.

 Name is placed in match group 9."
  (concat "^" org-link-bracket-re
          " +\\*\\(?9:[[:word:]]+\\)\\*:"))

(provide 'orgqda-transcript)

;;; orgqda-transcript.el ends here
