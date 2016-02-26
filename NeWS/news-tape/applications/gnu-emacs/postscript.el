;; Major mode for editing PostScript programs.
;;
;; Author:	Chris Maio
;; Last edit:	4 Sep 1988
;;
;; The following two statements, placed in your .emacs file or site-init.el,
;; will cause this file to be autoloaded, and postscript-mode invoked, when
;; visiting .ps or .cps files:
;;
;;	(autoload 'postscript-mode "postscript.el" "" t)
;;	(setq auto-mode-alist
;;	      (cons '("\\.c?ps$".postscript-mode) auto-mode-alist))
;;

(provide 'postscript)

(defconst ps-indent-level 4
  "*Indentation to be used inside of PostScript blocks or arrays")

(defconst ps-tab-width 4
  "*Tab stop width for PostScript mode")

(defun ps-make-tabs (stop)
  (and (< stop 132) (cons stop (ps-make-tabs (+ stop ps-tab-width)))))

(defconst ps-tab-stop-list (ps-make-tabs ps-tab-width)
  "*Tab stop list for PostScript mode")

(defconst ps-postscript-command "psh"
  "*Command used to invoke with a printer spooler or NeWS server.")

(defvar ps-mode-map nil
  "Keymap used in PostScript mode buffers")

(defvar ps-mode-syntax-table nil
  "PostScript mode syntax table")

(defun postscript-mode nil
  "Major mode for editing PostScript files.

\\[ps-execute-buffer] will send the contents of the buffer to the NeWS
server using psh(1).  \\[ps-execute-region] sends the current region.
\\[ps-shell] starts an interactive psh(1) window which will be used for
subsequent \\[ps-execute-buffer] or \\[ps-execute-region] commands.

In this mode, TAB and \\[indent-region] attempt to indent code
based on the position of {}, [], and begin/end pairs.  The variable
ps-indent-level controls the amount of indentation used inside
arrays and begin/end pairs.  

\\{ps-mode-map}

\\[postscript-mode] calls the value of the variable ps-mode-hook with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ps-mode-map)
  (if ps-mode-syntax-table
      (set-syntax-table ps-mode-syntax-table)
      (progn
	(setq ps-mode-syntax-table (make-syntax-table))
	(set-syntax-table ps-mode-syntax-table)
	(modify-syntax-entry ?\( "<")
	(modify-syntax-entry ?\) ">")
	(modify-syntax-entry ?\[ "(\]")
	(modify-syntax-entry ?\] ")\[")
	(modify-syntax-entry ?\% "<")
	(modify-syntax-entry ?\n ">")))
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'tab-stop-list)
  (setq comment-start "% "
	comment-start-skip "% *"
	comment-column 40
	indent-line-function 'ps-indent-line
	tab-stop-list ps-tab-stop-list)
  (setq mode-name "PostScript")
  (setq major-mode 'postscript-mode)
  (run-hooks 'ps-mode-hook))

(defun ps-tab nil
  "Command assigned to the TAB key in PostScript mode."
  (interactive)
  (if (save-excursion (skip-chars-backward " \t") (bolp))
      (ps-indent-line)			; fancy indent if beginning of line
    (indent-relative)))			; otherwise indent relative

(defun ps-indent-line nil
  "Indents a line of PostScript code."
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (if (not (or (looking-at "%%")	; "%%" comments stay at left margin
	       (ps-top-level-p)))
      (if (and (< (point) (point-max))
	       (eq ?\) (char-syntax (char-after (point)))))
	  (ps-indent-close)		; indent close-delimiter
	(if (looking-at "\\(dict\\|class\\)?end\\|cdef")
	    (ps-indent-end)		; indent end token
	  (ps-indent-in-block)))))	; indent line after open delimiter
  
(defun ps-open nil
  (interactive)
  (insert last-command-char))

(defun ps-insert-d-char (arg)
  "Awful hack to make \"end\" and \"cdef\" keywords indent themselves."
  (interactive "p")
  (insert-char last-command-char arg)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]*\\(\\(dict\\|class\\)?end\\|cdef\\)")
	(progn
	  (delete-horizontal-space)
	  (ps-indent-end)))))

(defun ps-close nil
  "Inserts and indents a close delimiter."
  (interactive)
  (insert last-command-char)
  (backward-char 1)
  (ps-indent-close)
  (forward-char 1)
  (blink-matching-open))

(defun ps-indent-close nil
  "Internal function to indent a line containing a an array close delimiter."
  (if (save-excursion (skip-chars-backward " \t") (bolp))
      (let (x (oldpoint (point)))
	(forward-char) (backward-sexp)	;XXX
	(if (and (eq 1 (count-lines (point) oldpoint))
		 (> 1 (- oldpoint (point))))
	    (goto-char oldpoint)
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (setq x (current-column))
	  (goto-char oldpoint)
	  (delete-horizontal-space)
	  (indent-to x)))))

(defun ps-indent-end nil
  "Indent an \"end\" token or array close delimiter."
  (let ((goal (ps-block-start)))
    (if (not goal)
	(indent-relative)
      (setq goal (save-excursion
		   (goto-char goal) (back-to-indentation) (current-column)))
      (indent-to goal))))

(defun ps-indent-in-block nil
  "Indent a line which does not open or close a block."
  (let ((goal (ps-block-start)))
    (setq goal (save-excursion
		 (goto-char goal)
		 (back-to-indentation)
		 (if (bolp)
		     ps-indent-level
		   (back-to-indentation)
		   (+ (current-column) ps-indent-level))))
    (indent-to goal)))

;;; returns nil if at top-level, or char pos of beginning of current block

(defun ps-block-start nil
  "Returns the character position of the character following the nearest
enclosing `[' `{' or `begin' keyword."
  (save-excursion
    (let (open (skip 0))
      (setq open (condition-case nil
		     (save-excursion
		       (backward-up-list 1)
		       (1+ (point)))
		   (error nil)))
      (ps-begin-end-hack open))))

(defun ps-begin-end-hack (start)
  "Search backwards from point to START for enclosing `begin' and returns the
character number of the character following `begin' or START if not found."
  (save-excursion
    (let ((depth 1) match)
      (while (and (> depth 0)
		  (or (re-search-backward
		       "^[ \t]*\\(dict\\|class\\)?end\\|begin[ \t]*\\(%.*\\)*$" start t)
		      (re-search-backward "^[ \t]*cdef.*$" start t)))
	(setq depth (if (looking-at "[ \t]*\\(dict\\|class\\)?end")
			(1+ depth) (1- depth))))
      (if (not (eq 0 depth))
	  start
	(forward-word 1)
	(point)))))

(defun ps-top-level-p nil
  "Awful test to see whether we are inside some sort of PostScript block."
  (and (condition-case nil
	   (not (scan-lists (point) -1 1))
	 (error t))
       (not (ps-begin-end-hack nil))))

;;; initialize the keymap if it doesn't already exist
(if (null ps-mode-map)
    (progn
      (setq ps-mode-map (make-sparse-keymap))
      (define-key ps-mode-map "d" 'ps-insert-d-char)
      (define-key ps-mode-map "f" 'ps-insert-d-char)
      (define-key ps-mode-map "{" 'ps-open)
      (define-key ps-mode-map "}" 'ps-close)
      (define-key ps-mode-map "[" 'ps-open)
      (define-key ps-mode-map "]" 'ps-close)
      (define-key ps-mode-map "\t" 'ps-tab)
      (define-key ps-mode-map "\C-c\C-c" 'ps-execute-buffer)
      (define-key ps-mode-map "\C-c|" 'ps-execute-region)
      (define-key ps-mode-map "\C-c!" 'ps-shell)))

(defun ps-execute-buffer nil
  "Send the contents of the buffer to a printer or NeWS server."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (ps-execute-region (point-min) (point-max))))

(defun ps-execute-region (start end)
  "Send the region between START and END to a printer or NeWS server.
You should kill any existing *PostScript* buffer unless you want the
PostScript text to be executed in that process."
  (interactive "r")
  (let ((start (min (point) (mark)))
	(end (max (point) (mark))))
    (condition-case nil
	(process-send-string "PostScript" (buffer-substring start end))
      (error (shell-command-on-region start end ps-postscript-command nil)))))

(defun ps-shell nil
  "Start a shell communicating with a PostScript printer or NeWS server."
  (interactive)
  (require 'shell)
  (switch-to-buffer-other-window
    (make-shell "PostScript" ps-postscript-command))
  (make-local-variable 'shell-prompt-pattern)
  (setq shell-prompt-pattern "PS>")
  (process-send-string "PostScript" "executive\n"))
