;;!emacs
;;
;; FILE:         rolo.el
;; SUMMARY:      Retrieves and sorts entries from a list of rolodex files
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., Communications Sector, Applied Research
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;;
;; ORIG-DATE:     7-Jun-89 at 22:08:29
;; LAST-MOD:     22-Feb-90 at 19:24:38 by Bob Weiner
;;
;; Copyright (C) 1991 Bob Weiner
;; WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
;; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;;
;; Permission to use, copy, modify, distribute, and sell this software and
;; its documentation for any purpose is hereby granted without fee,
;; provided that the above copyright notice appear in all copies and that
;; both that copyright notice and this permission notice appear in
;; supporting documentation, and that the name of Hewlett-Packard, Niels
;; Mayer, Brown University and Bob Weiner not be used in advertising or
;; publicity pertaining to distribution of the software without specific,
;; written prior permission.  Hewlett-Packard, Niels Mayer, Brown University
;; and Bob Weiner makes no representations about the suitability of this
;; software for any purpose.  It is provided "as is" without express or
;; implied warranty.
;;
;; This file is not part of GNU Emacs.
;;

;; This could use a key or field limited searching capability.
;;
;; DESCRIPTION:  
;;
;;  All I wanted to do was look up a phone number quickly . . .
;;
;;  FEATURES:
;;
;;   1.  Multiple rolodex files.
;;
;;   2.  Hierarchical rolodex entries as in:
;;        *    Company
;;        **     Manager
;;        ***      Underlings
;;
;;       Searching for Manager turns up all Underlings.  Searching for
;;       Company retrieves all listed employees.
;;
;;       This hierarchical system has proved very effective for retrieving
;;       computer system administration problem reports by vendor name,
;;       problem number or by subject area without having to resort to a
;;       database system.
;;
;;   4.  String and regular expression searching capabilities.  Ability to
;;       restrict number of matches or to report number of matches without
;;       displaying entries.
;;
;;   5.  Smart sorting of entries by hierarchy level.
;;
;;   See "rolo-logic.el" for logical search functions (and, or, not, xor).
;;
;;
;;  FOR NON-PROGRAMMERS:
;;
;;   Modify the second file entry in the definition of 'rolo-file-list'
;;   before using this package.
;;
;;   To add personal files to rolo-file-list--when you find these functions are
;;   useful for any sort of list lookup--add the following to your ~/.emacs
;;   file (substituting where you see <fileN>):
;;
;;      (require 'rolo)
;;      (setq rolo-file-list (append rolo-file-list '("<file1>" "<file2>")))
;;
;;   The only command you absolutely need that is defined here is
;;   'rolo-fgrep'; it locates any matching entries in a set of rolodex files.
;;   I recommend that you add the following key binding to one of your site
;;   specific Emacs initialization files:
;;
;;         (global-set-key "\C-x4r" 'rolo-fgrep)
;;
;;   Calling 'rolo-fgrep' with a prefix argument limits the number of matches
;;   to the specified number of entries.
;;
;;   The following commands are also provided:
;;     'rolo-grep' finds all entries matching a regular expression in a set
;;       of rolodex files;
;;     'rolo-edit' edits one's personal rolodex file;
;;     'rolo-sort' sorts all levels of entries in a rolodex file.
;;
;;   To make the 'rolo' library load whenever you initially call any of these
;;   functions, add the following to any of your Emacs init files:
;;
;;     (autoload 'rolo-fgrep "rolo"
;;       "Find entries in rolodex."
;;       t)	 
;;     (autoload 'rolo-grep "rolo"
;;       "Find entries in rolodex."
;;       t)	 
;;     (autoload 'rolo-edit "rolo"
;;       "Edit personal rolodex file."
;;       t)
;;     (autoload 'rolo-sort "rolo"
;;       "Sort rolodex file."
;;       t)
;;     
;;     
;;   Entries in rolodex files are separated by patterns matching
;;   'rolo-entry-regexp'.  Each entry may have any number of sub-entries
;;   which represent the next level down in the entry hierarchy.
;;   Sub-entries' separator patterns are always longer than their parents'.
;;   For example, if an entry began with '*' then its sub-entries would begin
;;   with '**' and so on.  Blank lines in rolodex files will not end up where
;;   you want them if you use the rolo-sort commands; therefore, blank lines
;;   are not recommended.
;;
;;   The reasons that the entries in 'rolo-file-list' have ".otl" suffixes
;;   are so that they do not conflict with file names that other rolodex
;;   programs might use and so that they are edited in 'outline-mode' by
;;   default.  If you want the latter behavior, uncomment and add something
;;   like the following to one of your GNU Emacs initialization files:
;;
;;     ;; Add to the list of suffixes that causes automatic mode invocation
;;     (setq auto-mode-alist
;;        (append '(("\\.otl$" . outline-mode)) auto-mode-alist))
;;
;;   The buffers containing the rolodex files are not killed after a search
;;   on the assumption that another search is likely to follow within this
;;   Emacs session.  You may wish to change this behavior with the following
;;   setting (after loading this file):
;;
;;     (setq rolo-kill-buffers-after-use t)
;;
;;   Here is a snippet from our group rolodex file (the ';'s should be
;;   removed of course and the '*'s should begin at the start of the line):
;;
;;=============================================================================
;;			      GROUP ROLODEX
;;    <Last Name>, <First Name>  <Co/Categ>   W<Work #>   H<Home #>  P<Pager #>
;;					      F<Fax #>    M<Modem #> C<Cellular #>
;;					      R<Other-radio #>
;;        <Address>	   <Miscellaneous Info, Key Words>
;;=============================================================================
;;*   EX594, Digital-Systems-Research
;;**  Weiner, Bob		  Motorola    W2087	             P7-7489
;;	  FL19, L-1035
;;
;;
;;  FOR PROGRAMMERS:
;;
;;   If you change the value of 'rolo-entry-regexp', you will have to modify
;;   'rolo-sort'.
;;
;;   The following additional functions are provided:
;;     'rolo-sort-level' sorts a specific level of entries in a rolodex file;
;;     'rolo-map-level' runs a user specified function on a specific level of
;;       entries in a rolodex file;
;;     'rolo-fgrep-file', same as 'rolo-fgrep' but operates on a single file;
;;     'rolo-grep-file', same as 'rolo-grep' but operates on a single file;
;;     'rolo-display-matches', display last set of rolodex matches, if any;
;;     'rolo-toggle-narrow-to-entry' toggles between display of current entry
;;       and display of all matching entries.
;;
;;   This code works fine on properly formatted rolodex files but probably
;;   will fail on certain improperly formatted ones.
;;
;;
;;  MOD HISTORY:
;;
;;   12/17/89
;;     Added internal 'rolo-shrink-window' function for use in
;;     compressing/uncompressing the rolo view window to/from a size just
;;     large enough for the selected entry.  This is useful when a search
;;     turns up more entries than desired.
;;
;;   02/21/90
;;     Modified 'rolo-grep-file' and 'rolo-map-level' so they only set buffers
;;     read-only the first time they are read in.  This way, if someone edits a
;;     rolodex file and then does a rolo-fgrep or other function, the buffer
;;     will not be back in read-only mode.
;;
;;
;; DESCRIP-END.

(defconst rolo-file-list '("~/.rolodex.otl")
  "List of files containing rolodex entries.
The first file should be a user-specific rolodex file, typically in the home
directory.  The second file is often a shared, group-specific rolodex file.

A rolo-file consists of:
   (1) an optional header beginning with and ending with a line which matches
       rolo-hdr-regexp;
   (2) one or more rolodex entries beginning with rolo-entry-regexp, which
       may be nested.")

(defconst rolo-kill-buffers-after-use nil
  "Non-nil means kill rolodex file buffers after searching them for entries.
Only unmodified buffers are killed.")

(defconst rolo-display-buffer "*Rolodex*"
  "Buffer used to display set of last matching rolodex entries.")

(defconst rolo-entry-regexp "^\*+"
  "Regular expression to match the beginning of a rolodex entry.
This pattern must match the beginning of the line.  Entries may be nested
through the use of increasingly longer beginning patterns.")

(defconst rolo-hdr-regexp "^==="
  "Regular expression to match the first and last lines of rolodex file headers.
This header is inserted into rolo-display-buffer before any entries from the
file are added.")

(defun rolo-fgrep (string &optional max-matches rolo-file count-only)
  "Display rolodex entries matching STRING, to a maximum of prefix arg MAX-MATCHES,
in file(s) from optional ROLO-FILE or rolo-file-list.  Default is to find all
matching entries.  Each entry is displayed with all of its sub-entries.
Optional COUNT-ONLY non-nil means don't display matching entries.
Returns number of entries matched.  See also documentation for the variable
rolo-file-list."
  (interactive "sRolodex string to match: \nP")
  (let ((total-matches
	  (rolo-grep (regexp-quote string) max-matches rolo-file count-only)))
    (if (interactive-p)
	(message (concat (if (= total-matches 0) "No" total-matches)
			 " matching entr"
			 (if (= total-matches 1) "y" "ies")
			 " found in rolodex.")))
    total-matches))

(defun rolo-grep (regexp &optional max-matches rolo-bufs count-only)
  "Display rolodex entries matching REGEXP, to a maximum of prefix arg MAX-MATCHES,
in buffer(s) from optional ROLO-BUFS or rolo-file-list.  Default is to find all
matching entries.  Each entry is displayed with all of its sub-entries.
Optional COUNT-ONLY non-nil means don't display matching entries.
Returns number of entries matched.  See also documentation for the variable
rolo-file-list."
  (interactive "sRolodex regular expression to match: \nP")
  (let ((rolo-file-list
	  (cond ((null rolo-bufs) rolo-file-list)
		((listp rolo-bufs) rolo-bufs)
		((list rolo-bufs))))
	(obuf (current-buffer))
	(display-buf (if count-only
			 nil
		       (set-buffer (get-buffer-create rolo-display-buffer))))
	(total-matches 0))
    (if count-only nil (setq buffer-read-only nil) (erase-buffer))
    (mapcar '(lambda (file)
	       (if (or (null max-matches) (> max-matches 0))
		   (let ((num-matched
			   (rolo-grep-file file regexp max-matches count-only)))
		     (setq total-matches (+ total-matches num-matched))
		     (or (null max-matches)
			 (setq max-matches (- max-matches num-matched))))))
	    rolo-file-list)
    (if (or count-only (= total-matches 0))
	nil
      (rolo-display-matches))
    (if (interactive-p)
	(message (concat (if (= total-matches 0) "No" total-matches)
			 " matching entr"
			 (if (= total-matches 1) "y" "ies")
			 " found in rolodex.")))
    total-matches))

(defun rolo-edit ()
  "Display personal rolodex file for editing."
  (interactive)
  (find-file (car rolo-file-list))
  (setq buffer-read-only nil))

(defun rolo-sort (&optional rolo-file)
  "Sort up to 14 levels of entries in ROLO-FILE (default is personal rolodex file).
Uses default rolo-entry-regexp for sort.  Returns list of number of groupings
at each entry level." 
  (interactive "fRolodex file to sort: ")
  (if (not rolo-file) (setq rolo-file (car rolo-file-list)))
  (let ((level-regexp (regexp-quote "**************"))
	(entries-per-level-list)
	(n))
    (while (not (equal level-regexp ""))
      (setq n (rolo-sort-level rolo-file level-regexp))
      (if (or (/= n 0) entries-per-level-list)
	  (setq entries-per-level-list
		(append (list n) entries-per-level-list)))
      (setq level-regexp (substring level-regexp 0 (- (length level-regexp) 2))))
    entries-per-level-list))

(defun rolo-sort-level (rolo-file level-regexp &optional max-groupings)
  "Sort groupings of entries in ROLO-FILE at hierarchy level given by LEVEL-REGEXP
to a maximum of optional MAX-GROUPINGS.  Nil value of MAX-GROUPINGS means all
groupings at the given level.  LEVEL-REGEXP should simply match the text of
any rolodex entry of the given level, not the beginning of a line (^); an
example, might be (regexp-quote \"**\") to match level two.  Returns number
of groupings sorted."
  (interactive "sRolodex file to sort: \nRegexp to match text of level's entries: \nP")
  (rolo-map-level
    '(lambda (start end) (sort-lines nil start end))
    rolo-file
    level-regexp
    max-groupings))

(defun rolo-map-level (func rolo-buf level-regexp &optional max-groupings)
  "Perform FUNC on each grouping of ROLO-BUF entries at hierarchy level LEVEL-REGEXP
to a maximum of optional argument MAX-GROUPINGS.  Nil value of MAX-GROUPINGS
means all groupings at the given level.  FUNC should take two arguments, the
start and the end of the region that it should manipulate.  LEVEL-REGEXP
should simply match the text of any rolodex entry of the given level, not the
beginning of a line (^); an example, might be (regexp-quote \"**\") to match
level two.  Returns number of groupings matched."
  (let ((new-buf-p) (actual-buf))
    (if (and (or (null max-groupings) (< 0 max-groupings))
	     (or (setq actual-buf (rolo-buffer-exists-p rolo-buf))
		 (if (file-exists-p rolo-buf)
		     (setq actual-buf (find-file-noselect rolo-buf t)
			   new-buf-p t))))
	(let ((num-found 0)
	      (exact-level-regexp (concat "^\\(" level-regexp "\\)[ \t\n]"))
	      (outline-regexp rolo-entry-regexp)
	      (level-len))
	  ;; Load 'outline' library since its functions are used here.
	  (if (not (boundp 'outline-mode-map))
	      (load-library "outline"))
	  (set-buffer actual-buf)
	  (if new-buf-p (setq buffer-read-only t))
	  (goto-char (point-min))
	  ;; Pass buffer header if it exists
	  (if (re-search-forward rolo-hdr-regexp nil t 2)
	      (forward-line))
	  (while (and (or (null max-groupings) (< num-found max-groupings))
		      (re-search-forward exact-level-regexp nil t))
	    (setq num-found (1+ num-found))
	    (let* ((opoint (prog1 (point) (beginning-of-line)))
		   (grouping-start (point))
		   (start grouping-start)
		   (level-len (or level-len (- (1- opoint) start)))
		   (next-level-len)
		   (next-entry-exists)
		   (grouping-end)
		   (no-subtree))
	      (while (and (progn
			    (if (setq next-entry-exists
				      (re-search-forward rolo-entry-regexp nil t 2))
				(setq next-level-len (- (point)
							(progn (beginning-of-line)
							       (point)))
				      grouping-end (< next-level-len level-len)
				      no-subtree (<= next-level-len level-len))
			      (setq grouping-end t no-subtree t)
			      (goto-char (point-max)))
			    (let ((end (point)))
			      (goto-char start)
			      (hide-subtree) ; And hide multiple lines of entry
			      ;; Move to start of next entry at equal or higher level
			      (setq start
				    (if no-subtree
					end
				      (if (re-search-forward rolo-entry-regexp
							     nil t)
					  (progn (beginning-of-line) (point))
					(point-max))))
			      ;; Remember last expression in 'progn' must always
			      ;; return non-nil
			      (goto-char start)))
			  (not grouping-end)))
	      (let ((end (point)))
		(goto-char grouping-start)
		(funcall func grouping-start end)
		(goto-char end))))
	  (show-all)
	  (rolo-kill-buffer actual-buf)
	  num-found)
      0)))

(defun rolo-fgrep-file (rolo-buf string &optional max-matches count-only)
  "Retrieve entries in ROLO-BUF matching STRING to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.  Returns number of matching entries
found."
  (rolo-grep-file rolo-buf (regexp-quote string) max-matches count-only))

(defun rolo-grep-file (rolo-buf regexp &optional max-matches count-only)
  "Retrieve entries in ROLO-BUF matching REGEXP to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.  Returns number of matching entries
found."
  (let ((new-buf-p) (actual-buf))
    (if (and (or (null max-matches) (< 0 max-matches))
	     (or (setq actual-buf (rolo-buffer-exists-p rolo-buf))
		 (if (file-exists-p rolo-buf)
		     (setq actual-buf (find-file-noselect rolo-buf t)
			   new-buf-p t))))
	(let ((hdr-pos) (num-found 0) (curr-entry-level))
	  (set-buffer actual-buf)
	  (if new-buf-p (setq buffer-read-only t))
	  (goto-char (point-min))
	  (if (re-search-forward rolo-hdr-regexp nil t 2)
	      (progn (forward-line)
		     (setq hdr-pos (cons (point-min) (point)))))
	  (re-search-forward rolo-entry-regexp nil t)
	  (while (and (or (null max-matches) (< num-found max-matches))
		      (re-search-forward regexp nil t))
	    (re-search-backward rolo-entry-regexp nil t)
	    (let ((start (point))
		  (next-entry-exists))
	      (re-search-forward rolo-entry-regexp nil t)
	      (rolo-to-entry-end
		t (setq curr-entry-level (buffer-substring start (point))))
	      (or count-only
		  (and (= num-found 0) hdr-pos
		       (progn (append-to-buffer rolo-display-buffer
						(car hdr-pos) (cdr hdr-pos)))))
	      (setq num-found (1+ num-found))
	      (or count-only
		  (append-to-buffer rolo-display-buffer start (point)))))
	  (rolo-kill-buffer actual-buf)
	  num-found)
      0)))

(defun rolo-display-matches ()
  "Re-display buffer of previously found rolodex matches."
  (interactive)
  (let ((obuf (if (boundp 'obuf) obuf (current-buffer)))
	(display-buf (if (boundp 'display-buf)
			 display-buf (get-buffer rolo-display-buffer))))
    (pop-to-buffer display-buf)
    (rolo-shrink-window)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (let ((buf (get-buffer-window obuf)))
	(if buf (select-window buf) (switch-to-buffer buf)))))

(defun rolo-toggle-narrow-to-entry ()
  "Toggle between display of entry point is in and display of all matched entries."
  (interactive)
  (if (rolo-narrowed-p)
      (widen)
    (if (or (looking-at rolo-entry-regexp)
	    (re-search-backward rolo-entry-regexp nil t))
	(progn (forward-char)
	       (narrow-to-region (1- (point)) (rolo-display-to-entry-end)))))
  (rolo-shrink-window)
  (goto-char (point-min)))

;;
;; INTERNAL FUNCTIONS.
;;

(defun rolo-buffer-exists-p (rolo-buf)
  "Returns buffer given by ROLO-BUF or nil.
ROLO-BUF may be a file-name, buffer-name, or buffer."
  (car (memq (get-buffer (or (and (stringp rolo-buf)
				  (get-file-buffer rolo-buf))
			     rolo-buf))
	     (buffer-list))))

(defun rolo-kill-buffer (rolo-buf)
  (and rolo-kill-buffers-after-use (not (buffer-modified-p rolo-buf))
       (kill-buffer rolo-buf)))

(defun rolo-to-entry-end (&optional include-sub-entries curr-entry-level)
"Go to end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
CURR-ENTRY-LEVEL is a string whose length is the same as the last found entry
header.  If INCLUDE-SUB-ENTRIES is nil, CURR-ENTRY-LEVEL is not needed."
  (while (and (setq next-entry-exists
		    (re-search-forward rolo-entry-regexp nil t))
	      include-sub-entries
	      (> (- (point) (save-excursion
			      (beginning-of-line)
			      (point)))
		 (length curr-entry-level))))
  (if next-entry-exists
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

(defun rolo-display-to-entry-end ()
  "Go to end of entry current entry, ignoring sub-entries."
  (if (re-search-forward (concat rolo-hdr-regexp "\\|"
				 rolo-entry-regexp) nil t)
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

	  
(defun rolo-shrink-window ()
  (let* ((lines (count-lines (point-min) (point-max)))
	 (height (window-height))
	 (window-min-height 2)
	 (desired-shrinkage (1- (min (- height lines)))))
    (and (>= lines 0)
	 (/= desired-shrinkage 0)
	 (> (screen-height) (1+ height))
	 (shrink-window 
	   (if (< desired-shrinkage 0)
	       (max desired-shrinkage (- height (/ (screen-height) 2)))
  (min desired-shrinkage (- height window-min-height)))))))


(defun rolo-narrowed-p ()
  (or (/= (point-min) 1) (/= (1+ (buffer-size)) (point-max))))

(provide 'rolo)
