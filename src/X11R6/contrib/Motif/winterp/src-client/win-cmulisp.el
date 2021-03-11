; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         win-cmulisp.el
; RCS:          $Header: /users/npm/src/winterp/src-client/RCS/win-cmulisp.el,v 2.6 1994/06/06 15:14:40 npm Exp $
; Description:  Run WINTERP in an emacs subshell. Under emacs 18.XX, this uses
;               the non-standard CMULISP/COMINT-2.03 package. Under emacs
;		19.XX, CMULISP/COMINT is now the standard package for running
;		lisp subshells. This file should work with the default emacs
;		19.XX; for emacs 18.XX, you'll have to procure the
;		COMINT/CMULISP package via ftp. See notes below for details.
; Author:       Niels Mayer
; Created:      Sat Sep 19 05:36:30 1992
; Modified:     Sun Jun  5 16:15:25 1994 (Niels Mayer) npm@indeed
; Language:     Emacs-Lisp
; Package:      N/A
; Status:       X11r6 contrib release
; 
; Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
; WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
; WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
; 
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Enterprise Integration Technologies,
; Hewlett-Packard Company, or Niels Mayer not be used in advertising or
; publicity pertaining to distribution of the software without specific,
; written prior permission. Enterprise Integration Technologies, Hewlett-Packard
; Company, and Niels Mayer makes no representations about the suitability of
; this software for any purpose.  It is provided "as is" without express or
; implied warranty.
; 
; ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
; DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
; INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
; RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
; CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------------------------------------------------------------------------
; See ./winterp/COPYRIGHT for information on contacting the authors.
; Please e-mail comments, modifications, questions, improvements and
; bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
; mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
; Post XLISP-specific questions/information to the USENET newsgroup
; comp.lang.lisp.x.
;------------------------------------------------------------------------------

;;
;; NOTE: cmulisp package may not be a standard part of your gnu-emacs
;; distribution; supposedly, this will be included as a standard part
;; of gnu-emacs in some future version of emacs (18.58, v. 19??).
;; If not part of your emacs distribution, you may pick the files up
;; from the ohio-state-university emacs archives (see comp.emacs or 
;; gnu.emacs frequently asked questions list (FAQ)), or you may
;; pick them up by anonymous ftp by following these instructions:
;;|| You can now anonymously ftp the source files from CMU:
;;|| - Ftp to a.gp.cs.cmu.edu (128.2.242.7) or any other machine that mounts
;;||   the AFS file tree.
;;|| - cd /afs/cs.cmu.edu/user/shivers/lib/emacs
;;||   Don't try to cd to or ls any of the intermediate directories;
;;||   go straight to the .../emacs subdirectory. The CMU security mechanisms
;;||   will block you from accessing any of the other directories while you
;;||   are doing anonymous accesses.
;;|| - ls *.el
;;|| - get the files you want.
;;|| - Release notes are in the notes subdirectory.
;; 
;; The files cmulisp.{el,elc} and comint.{el,elc} must be on your emacs
;; load-path (an emacs global variable).
;; e.g.
;; (setq load-path
;;      (append
;;       '("/users/mayer/src/gnuemacs/cmushell")
;;       load-path
;;       ))
;;
;;==============================================================================
;;
;; Notes:
;; 
;; * I've only tried this with comint.el versions 2.01 and 2.03 (emacs 18.XX)
;;   and standard emacs 19.19. This hasn't been fully tested under emacs 19.
;;
;; * To start up winterp as an emacs subprocess, do "M-X run-winterp", this
;; will create a buffer "*cmulisp*" (or *inferior-lisp* for emacs 19)
;; which displays winterp's output and can also be used to edit and
;; send lisp expressions to the winterp subprocess.
;;
;; * If in breakloop, typing ^D in *cmulisp* (or *inferior-lisp* for
;; emacs 19) buffer will exit breaklevel, this will not exit WINTERP's
;; top-level, to do that, evaluate (exit) or use commands C-c C-c
;; (comint-interrupt-subjob), or C-c C-\ (comint-quit-subjob).
;;
;; * BUG: in buffer *cmulisp* (or *inferior-lisp* for emacs 19)
;; (the buffer in which WINTERP is running), do not issue the following
;; command:
;; 	C-c C-\		comint-quit-subjob
;; as this will cause WINTERP to core dump, at least on HPUX 8.0.
;; (I don't know, understand, or have time to figure out why). To quit the
;; WINTERP subprocess, issue "(exit)", quit by closing the w_ctrlpnl.lsp window,
;; or do
;;	C-c C-c		comint-interrupt-subjob
;;
;; * BUG-WORKAROUND: on my test system (HPUX 8.0), the default comint/cmulisp
;; setup didn't work correctly for sending large chunks of lisp to the
;; subprocess. To work around this problem, I use pipes rather than the 
;; default ptys for communicating between emacs and the winterp subprocess.
;; This is done by setting process-connection-type == NIL...
;; See "N O T E   O N   C O M I N T / C M U L I S P   B U G" and function
;; 'cmulisp' below.
;;
;;==============================================================================
;;
;; other interesting commands:

;;; Brief Command Documentation:
;;;============================================================================
;;; Comint Mode Commands: (common to cmulisp and all comint-derived modes)
;;;
;;; m-p	    comint-previous-input    	    Cycle backwards in input history
;;; m-n	    comint-next-input  	    	    Cycle forwards
;;; m-c-r   comint-previous-input-matching  Search backwards in input history
;;; return  comint-send-input
;;; c-a     comint-bol                      Beginning of line; skip prompt.
;;; c-d	    comint-delchar-or-maybe-eof	    Delete char unless at end of buff.
;;; c-c c-u comint-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c comint-interrupt-subjob 	    ^c
;;; c-c c-z comint-stop-subjob	    	    ^z
;;; c-c c-\ comint-quit-subjob	    	    ^\
;;; c-c c-o comint-kill-output		    Delete last batch of process output
;;; c-c c-r comint-show-output		    Show last batch of process output
;;;         send-invisible                  Read line w/o echo & send to proc
;;;         comint-continue-subjob	    Useful if you accidentally suspend
;;;					        top-level job.
;;; comint-mode-hook is the comint mode hook.

;;; CMU Lisp Mode Commands:
;;; c-m-x   lisp-send-defun     This binding is a gnu convention.
;;; c-c c-l lisp-load-file  	Prompt for file name; tell Lisp to load it.
;;; c-c c-k lisp-compile-file	Prompt for file name; tell Lisp to kompile it.
;;; 	    	    	    	Filename completion is available, of course.
;;;
;;; Additionally, these commands are added to the key bindings of Lisp mode:
;;; c-m-x   lisp-eval-defun         This binding is a gnu convention.
;;; c-c c-e lisp-eval-defun 	    Send the current defun to Lisp process.
;;; c-x c-e lisp-eval-last-sexp     Send the previous sexp to Lisp process.
;;; c-c c-r lisp-eval-region        Send the current region to Lisp process.
;;; c-c c-c lisp-compile-defun      Compile the current defun in Lisp process.
;;; c-c c-z switch-to-lisp          Switch to the Lisp process buffer.
;;; c-c c-l lisp-load-file          (See above. In a Lisp file buffer, default
;;; c-c c-k lisp-compile-file        is to load/compile the current file.)
;;; c-c c-d lisp-describe-sym	    Query Lisp for a symbol's description.
;;; c-c c-a lisp-show-arglist	    Query Lisp for function's arglist.
;;; c-c c-f lisp-show-function-documentation Query Lisp for a function's doc.
;;; c-c c-v lisp-show-variable-documentation Query Lisp for a variable's doc.
;;;=============================================================================



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; N O T E   O N   C O M I N T / C M U L I S P   B U G ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Work around a nasty bug/problem in comint which seems to be present on
;; HPUX 8.0 (HP9000s300), (but may not be a problem on other OS's):
;; when 'comint-input-chunk-size' is set to default 512, the
;; comint-to-subprocess interface loses characters. In a test of sending
;; (via C-M-X) a lisp expression of the form:
;;	(progn
;;	  (print "xxxxxxxxxxxxx1")
;;	  (print "xxxxxxxxxxxxx2")
;;	  (print "xxxxxxxxxxxxx3")
;;	  .
;;	  . 
;;	  .
;;	  (print "xxxxxxxxxxxxx98")
;;	  (print "xxxxxxxxxxxxx99")
;;	  (print "xxxxxxxxxxxxx100")
;;	  )
;; I noticed that lossage would occur for comint-input-chunk-size set to
;; 13, 14, 25, 26, 27, 128, 256, 512. A value of 5 seemed to work, but
;; also results in the emacs/subprocess interface running very slowly.
;; This is only a problem when using C-M-X to send large chunks of code,
;; not a problem when typing short s-exprs directly into *cmulisp*
;; (or *inferior-lisp* for emacs 19) buffer.
;;
;; This setting results in fewer errors, but ultimately does result in errors:
;; (setq comint-input-chunk-size 5)
;;
;; .. therefore, I went for a completely different approach, which is to wrap
;; the original 'cmulisp' function with something that sets variable
;; process-connection-type to NIL, thereby forcing the use of pipes rather
;; than ptys for communicating with the lisp subprocess (but not for other
;; comint modes, e.g. shell).

;;;;;
;; the following variables customize Emacs' lisp mode variables for using WINTERP.
;; For example, if you want to run WINTERP on another machine which has passwordless
;; rsh/rlogin permissions you might do 
;; (setq inferior-lisp-program "rsh odin /usr/local/bin/winterp -display collage:0.0 -enable_stdin_server -no_unix_server -no_inet_server")
;;;;;

(setq inferior-lisp-program "winterp -enable_stdin_server -no_unix_server -no_inet_server") ;Name of Lisp program run-lisp executes
(setq inferior-lisp-load-command "(load \"%s\" :verbose nil :print t)\n") ;Customises lisp-load-file
(setq inferior-lisp-prompt "^[X0-9]+> +") ;Initialises comint-prompt-regexp. Backwards compatibility.
;; (setq comint-prompt-regexp "^[X0-9]+> +") ;init'd to inferior-lisp-prompt
;; (setq cmulisp-filter-regexp "^[X0-9]+> +") ;Match this => don't get saved on input hist


(if (string-match "19" (substring emacs-version 0 2))
    ;; for emacs 19, optional cmulisp package has become official
    ;; inferior lisp package, "cmulisp" name has now been changed
    ;; to "inferior-lisp" (etc.). Here, we alias the old "cmulisp"
    ;; names to the new "inferior-lisp" such that the code below works
    ;; for both emacs 19 and emacs 18.
    (progn
      (require 'inf-lisp)
      (fset 'cmulisp-bak (symbol-function 'inferior-lisp))
      (fset 'cmulisp-proc (symbol-function 'inferior-lisp-proc))
      (setq cmulisp-buffer-name "*inferior-lisp*")
      )
  ;; for emacs 18.
  (progn
    (load "cmulisp")                    ;assume it's on emacs load-path.
    (fset 'cmulisp-bak (symbol-function 'cmulisp))
    (setq cmulisp-buffer-name "*cmulisp*")
    )
  )

;;
;; wrap original CMULISP function so that we force use of a pipe rather than
;; the default pty for communication between emacs and the lisp subprocess.
;;

;; (fset 'cmulisp-bak (symbol-function 'cmulisp))

(defun cmulisp (&optional cmd)
  "Run an inferior Lisp process, input and output via buffer *cmulisp*
(or *inferior-lisp* for emacs 19). If there is a process already running
in *cmulisp* (or *inferior-lisp*), just switch to that buffer.
With argument, allows you to edit the command line (default is value
of inferior-lisp-program).  Runs the hooks from cmulisp-mode-hook (after the
comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (and current-prefix-arg
			  (read-string "Run lisp: " inferior-lisp-program))))

  (if (null cmd)
      (setq cmd inferior-lisp-program))

  (let (
	(process-connection-type nil)	;Use a pipe.
	(comint-ptyp		 nil)	;tell comint we're using a pipe
	)
    (cmulisp-bak cmd)
    )
  )


(defun run-winterp (&optional cmd)
  "Runs winterp as subprocess under buffer *cmulisp* (or *inferior-lisp*)."
  (interactive (list (and current-prefix-arg
			  (read-string "Run lisp: " inferior-lisp-program))))
  (cmulisp cmd)
  (if (boundp 'epoch::version)
      (find-buffer-other-screen cmulisp-buffer-name)
    (switch-to-buffer cmulisp-buffer-name))
  )

(defun lisp-eval-buffer (&optional and-go)
  "Send the current region to the inferior Lisp process.
Prefix argument means switch-to-lisp afterwards."
  (interactive "P")
  (comint-send-region (cmulisp-proc) (point-min) (point-max))
  (comint-send-string (cmulisp-proc) "\n")
  (if and-go (switch-to-lisp t))
  )

(provide 'win-cmulisp)
