;; PostScript mode.
;;
;; Copyright (c) 1988
;;      by
;; JTS Computer Systems Ltd. 
;; 5000 Dufferin Street, Unit G,
;; Downsview, Ontario,
;; CANADA M3H 5T5
;; (416) 665-8910
;; All rights reserved.
;; Permission to use, copy, modify, and distribute this software and its
;; documentation for any purpose and without fee is hereby granted, provided
;; that the above copyright notice appear in all copies and that both that
;; copyright notice and this permission notice appear in supporting
;; documentation.  This software is provided "as is" without express or
;; implied warranty.
;;
;;
;;
;; Orig: March 2, 1988.
;; Author: Steve Rosenberg
;;

(defun rassoc (e l)
  "Returns non-nil if ELT is the cdr of an element of LIST.
Comparison done with equal.  The value is actually the
element of LIST whose cdr is ELT."
  (car (delq nil
	     (mapcar (function (lambda (i)
				 (if (equal e (car (cdr i)))
				     i nil)))
		     l))))

(defvar postscript-mode-syntax-table nil
  "Syntax table used in PostScript mode buffers.")
(defvar postscript-mode-abbrev-table nil
  "Abbrev table used in PostScript mode buffers.")

(if (not postscript-mode-syntax-table)
    (let ((i 0))
      (setq postscript-mode-syntax-table (make-syntax-table))
      (set-syntax-table postscript-mode-syntax-table)
      (while (< i ?0)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (modify-syntax-entry ?  "    ")
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n ">   ")
      (modify-syntax-entry ?\f "    ")
      (modify-syntax-entry ?\% "<   ")
      (modify-syntax-entry ?` "w   ")
      (modify-syntax-entry ?' "w   ")
      (modify-syntax-entry ?, "w   ")
      (modify-syntax-entry ?. "w   ")
      (modify-syntax-entry ?# "w   ")
      (modify-syntax-entry ?\" "w   ")
      (modify-syntax-entry ?\\ "\\   ")
      (modify-syntax-entry ?{ "(}  ")
      (modify-syntax-entry ?} "){  ")
      (modify-syntax-entry ?[ "(]  ")
      (modify-syntax-entry ?] ")[  ")
      (modify-syntax-entry ?< "(>  ")
      (modify-syntax-entry ?> ")<  ")
      ;; The next 2 lines define the PostScript string quote characters ().
      ;; They really should be defined as such, but gnu-emacs thinks that
      ;; strings are quoted by a single char, not pairs.  This causes all
      ;; characters following the ( char to be quoted until the next ( char,
      ;; which is not what we want.  For now I have defined the ( and the )
      ;; to be in the class of symbol names/not words.
      (modify-syntax-entry ?\( "_    ")
      (modify-syntax-entry ?\) "_    ")))

(define-abbrev-table 'postscript-mode-abbrev-table ())

(defun postscript-mode-variables ()
  (set-syntax-table postscript-mode-syntax-table)
  (setq local-abbrev-table postscript-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'postscript-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  )

(defvar postscript-mode-map (make-sparse-keymap))
(define-key postscript-mode-map "\177" 'backward-delete-char-untabify)
(define-key postscript-mode-map "\t" 'postscript-indent-line)
(define-key postscript-mode-map "\n" 'postscript-indent-and-newline)
(define-key postscript-mode-map "\e;" 'query-replace)
(define-key postscript-mode-map "\e%" 'postscript-indent-for-comment)

(defvar postscript-pairs
  '(("{" "}")
    ("[" "]")
    ("gsave" "grestore")
    ("begin" "end")
    ("dictbegin" "dictend")
    ("classbegin" "classend")
    ("save" "restore"))
  "Matching tokens that cause enclosed code to be indented.")


(defun postscript-mode ()
  "Major mode for editing PostScript language programs.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Percent symbols start comments.
\\{postscript-mode-map}
Entry to this mode calls the value of postscript-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map postscript-mode-map)
  (setq major-mode 'postscript-mode)
  (setq mode-name "PostScript")
  (postscript-mode-variables)
  (run-hooks 'postscript-mode-hook))

(defconst postscript-indent-offset 4 "")
(defconst postscript-indent-hook 'postscript-indent-hook "")

(defun ps-tokenize (instr)
  "Return tokenized version of STRING in the form:
  ( ( non-comment tokens ) ( comment string ) )
The tokens are a list of strings."
  (let ((tokenl '())
	(current-token "")
	(pos 0)
	(prior ? )
	current-char comment quoted hex building)
    (while (< pos (length instr))
      (setq current-char (string-to-char(substring instr pos)))
      (cond
       ;; quoted mode (currently processing quoted string)
       ;; escape char in quoted mode? - append it and next char to token
       ((and quoted (= current-char ?\\))
	(setq current-token
	      (concat current-token
		      (substring instr pos (1+ pos))))
	(setq pos (1+ pos)))
       ;; closing \) in quoted mode - finish this token, finish quoted mode
       ((and quoted (= current-char ?\)))
	(setq current-token
	      (concat current-token "\)"))
	(setq tokenl (cons current-token tokenl))
	(setq current-token "")
	(setq quoted nil))
       ;; normal char in quoted mode - append to token
       ( quoted
	 (setq current-token
	       (concat current-token (char-to-string current-char))))
       ;; opening quote - finish last token (if any) and start new one
       ;;		- enter quoted mode
       ((= current-char ?\()
	(if (> (length current-token) 0)
	    (setq tokenl (cons current-token tokenl)))
	(setq current-token "(")
	(setq quoted t))
       ;; comment char - set comment list to remainder of string
       ((= current-char ?%)
	(setq comment (list (substring instr pos)))
	(setq pos (length instr)))
       ;; hex string mode (currently processing hex string)
       ;; closing > in hex string mode - finsh this token, finish hex mode
       ((and hex (= current-char ?>))
	(setq current-token
	      (concat current-token ">"))
	(setq tokenl (cons current-token tokenl))
	(setq current-token "")
	(setq hex nil))
       ;; normal char in hex mode - append to token
       ( hex
	 (setq current-token
	       (concat current-token (char-to-string current-char))))
       ;; opening hex quote - finish last token (if any) and start new one
       ;;		    - enter hex mode
       ((= current-char ?\<)
	(if (> (length current-token) 0)
	    (setq tokenl (cons current-token tokenl)))
	(setq current-token "<")
	(setq hex t))
       ;; delimiter char - a token
       ((or (= (char-syntax current-char) ?\()
	    (= (char-syntax current-char) ?\)))
	(if (> (length current-token) 0)
	    (setq tokenl (cons current-token tokenl)))
	(setq tokenl (cons (char-to-string current-char) tokenl))
	(setq current-token ""))
       ;; symbol-constituent char - append to current token
       ((or (= (char-syntax current-char) ?w)
	    (= (char-syntax current-char) ?_))
	(setq current-token
	      (concat current-token (char-to-string current-char))))
       ;; whitespace syntax char - flush current token if it exists
       ((= (char-syntax current-char) ? )
	(if (> (length current-token) 0)
	    (progn
	      (setq tokenl (cons current-token tokenl))
	      (setq current-token ""))))
       ;; problem! - unanticipated char type
       ( t (error "Error during PostScript tokenizing")))
      (setq pos (1+ pos)))
    (if (> (length current-token) 0)
	(setq tokenl (cons current-token tokenl)))
    (list (reverse tokenl) comment)))

(defun postscript-indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  (interactive "*")
  (let* ((start (save-excursion (beginning-of-line 1) (point)))
	 (end (save-excursion (end-of-line) (point)))
	 (comment
	  (car (nth 1 (ps-tokenize (buffer-substring start end)))))
	 indent comment-col)
    (beginning-of-line 1)
    (if comment
	(progn
	  (re-search-forward (regexp-quote comment) end)
	  (delete-region (match-beginning 0) (match-end 0))))
    (end-of-line)
    (delete-horizontal-space)
    (setq indent (max comment-column (1+ (current-column))))
    (indent-to indent)
    (setq comment-col (point))
    (if comment
	(progn
	  (insert comment)
	  (goto-char (+ 2 comment-col)))
	(insert "% " ))))


(defun postscript-indent-line ()
  "Indent current line following PostScript coding conventions."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (calculate-postscript-indent))))

(defun postscript-indent-and-newline ()
  "Indent the current line then do a newline."
  (interactive)
  (postscript-indent-line)
  (newline 1))


(defun calculate-postscript-indent ()
  "Return the column to indent to."
  (save-excursion
    (beginning-of-line)
    (let ((begin-point (point)) start-point
	  end-point this-line prior-line tokens
	  opener closer done test-line opens-to-match)
      (end-of-line)
      (setq this-line (buffer-substring begin-point (point)))
      (setq closer (rassoc
		    (car (car (ps-tokenize this-line)))
		    postscript-pairs))
      (setq opener (car closer))
      (setq closer (car (cdr closer)))
      (if closer			; we have a block closer at the
					; start of this line
	  (progn			; find line with matching opener
	    (setq opens-to-match 1)
	    (beginning-of-line)
	    (while (and (> opens-to-match 0)
			(not (bobp)))
	      (previous-line 1)
	      (setq start-point (point))
	      (end-of-line)
	      (setq end-point (point))
	      (setq test-line (buffer-substring start-point (point)))
	      (beginning-of-line)
	      (mapcar
	       (function
		(lambda (e)
		  (if (> opens-to-match 0)
		      (cond ((equal e closer)
			     (setq opens-to-match (1+ opens-to-match)))
			    ((equal e opener)
			     (setq opens-to-match (1- opens-to-match)))))))
	       (reverse (car (ps-tokenize test-line)))))
	    (beginning-of-line)
	    (re-search-forward "\\S " end-point t)
	    (backward-char 1)
	    (current-column))
	(progn				; case of no closer, move up to
	  (beginning-of-line)		; last line with code (no-comment)
	  (while (and (not done)	
		      (not (bobp)))
	    (previous-line 1)
	    (setq start-point (point))
	    (end-of-line)
	    (setq end-point (point))
	    (setq done			; done will contain non-comment
		  (car			; tokens or nil if there were none
		   (setq tokens
			 (ps-tokenize
			  (buffer-substring start-point (point))))))
	    (beginning-of-line))
	  (if (and (not done)
		   (bobp))		;if we are at bob then there were no
	      (progn			;lines with code above
		(goto-char begin-point)
		(current-column)))
	    ; if not, then check for opening delimiter as last token,
	    ; and add indent if found, else indent same
	  (progn
	    (re-search-forward "\\S " end-point t)
	    (backward-char 1)
	    (+ (current-column)
	       (if (assoc (car (reverse (car tokens)))
			  postscript-pairs)
		   postscript-indent-offset 0))))))))
		    

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings,
assuming that the start of the region is not inside them.
Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (point)
					  (progn
					    (forward-line 1) (point))
					  nil nil state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					nil nil state))))))

  
