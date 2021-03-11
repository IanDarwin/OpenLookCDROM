; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         winterp2.el
; RCS:          $Header: winterp.el,v 1.2 89/12/15 17:48:27 mayer Exp $
; Description:  GNUEMACS lisp-mode interface to WINTERP.
; Author:       Niels Mayer, HPLabs. 
;		Modified by Richard Hess, cimshop!rhess@uunet.UU.NET.
; Created:      Tue Nov 14 23:14:54 1989
; Modified:     Fri Dec 15 17:46:07 1989 (Niels Mayer) mayer@hplnpm
; Language:     Emacs-Lisp
; Package:      N/A
; Status:       X11r5 contrib tape release
;
; WINTERP Copyright 1989-1991 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and David Betz not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and David Betz
; make no representations about the suitability of this software for any
; purpose. It is provided "as is" without express or implied warranty.
;
; HEWLETT-PACKARD AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
; SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
; IN NO EVENT SHALL HEWLETT-PACKARD NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
; INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
; OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.
;
; See ./winterp/COPYRIGHT for information on contacting the authors.
; 
; Please send modifications, improvements and bugfixes to mayer@hplabs.hp.com
; Post XLISP-specific questions/information to the newsgroup comp.lang.lisp.x
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar winterp2-client "wl"
  "The full path name of the client program (ie 'wl') that sends lisp
expressions to the winterp lisp server...")

(defvar winterp2-client-args nil
  "This sets the arguments sent to 'wl'... [ must be a list of strings ]")

;; --------------------------------------------------------------------------

(defun winterp-send-defun ()
  "[ NEW ]:  A version of lisp-send-defun that talks to WINTERP's lisp server
via the winterp client program 'wl'..."
  (interactive)
  (let ((loadfile (format "/tmp/wl%d.lsp" (user-real-uid))))
    (save-excursion (write-region
		     (progn (beginning-of-defun) (point))
		     (progn (end-of-defun) (point))
		     loadfile nil 'nomessage))
    (apply 'start-process "winterp2-client" nil
	   winterp2-client
	   (append winterp2-client-args
		   (list (format
			  "(load \"%s\" :verbose nil :print t)"
			  loadfile))))
    )
  )

;; --------------------------------------------------------------------------

(defun winterp-send-exit ()
  "[ NEW ]:  Calling this function will send WINTERP a command to exit the
current breaklevel. If not at a breaklevel, then WINTERP will exit..."
  (interactive)
  ;;sending wl with no args sends an EOF to WINTERP which signals XLISP to
  ;;exit the current breaklevel, or exit if not in the breakloop.
  (apply 'start-process "winterp2-client" nil
	 winterp2-client
	 winterp2-client-args)
  )

;; --------------------------------------------------------------------------

(defun winterp-send-buffer ()
  "[ NEW ]:  A version of winterp-send-defun that sends the entire buffer off
to WINTERP for evaluation. This routine talks to WINTERP's lisp server via the
winterp client program 'wl'..."
  (interactive)
  (let ((loadfile (format "/tmp/wl%d.lsp" (user-real-uid))))
    (save-excursion (write-region (point-min) (point-max)
				  loadfile nil 'nomessage))
    (apply 'start-process "winterp2-client" nil
	   winterp2-client
	   (append winterp2-client-args
		   (list (format
			  "(load \"%s\" :verbose nil :print t)"
			  loadfile))))
    )
  )

;; ----------------------------------------------------------------[ contrib ]
;;; Date: Mon, 17 Dec 90 16:46:39 -0500
;;; From: rsw@cs.brown.edu (Bob Weiner)
;;; Message-Id: <9012172146.AA06790@fiddle.cs.brown.edu>
;;; To: mayer@hplnpm.hpl.hp.com
;;; Subject: Generalization of winterp.el code.
;;; 
;;; The generalization I include here substitutes an eval-last-sexp type 
;;; function for your send-defun.  The send-defun function may be left around 
;;; but really is not needed since all one need do is move to the end of the
;;; function and evaluate it.  This makes the interface so much simpler when
;;; one wants to do bottom up testing via the interpreter.
;;; 
;;; I've included only the one function and the key binding, so you should
;;; substitute it into your winterp.el file.

(defun winterp-eval-last-sexp ()
  "[ NEW ]:  A version of eval-last-sexp that talks to WINTERP's lisp server
via the winterp client program 'wl'.  Evaluates sexp immediately preceding
point..."
  (interactive)
  (let ((loadfile (format "/tmp/wl%d.lsp" (user-real-uid)))
	(stab (syntax-table)))
    (write-region
     (unwind-protect
	 (save-excursion
	   (set-syntax-table emacs-lisp-mode-syntax-table)
	   (forward-sexp -1)
	   (point))
       (set-syntax-table stab))
     (point)
     loadfile nil 'nomessage)
    (apply 'start-process "winterp2-client" nil
	   winterp2-client
	   (append winterp2-client-args
		   (list (format
			  "(load \"%s\" :verbose nil :print t)"
			  loadfile))))
    )
  )

;; ----------------------------------------------------------------[ contrib ]
;;; Date: Mon, 17 Dec 90 16:56:32 -0500
;;; From: rsw@cs.brown.edu (Bob Weiner)
;;; Message-Id: <9012172156.AA06799@fiddle.cs.brown.edu>
;;; To: mayer@hplnpm.hpl.hp.com
;;; Subject: Further generalization of winterp.el code.
;;; 
;;; We can further simplify the interface by combinding the expression 
;;; evaluation command with the buffer evaluation command.  The latter is
;;; called when a prefix argument is sent to the 'winterp-eval' command.  
;;; Thus, only one key is needed, but both functions are accessible.  Try it.
;;;

(defun winterp-eval (&optional arg)
  "Evaluate last sexpression.  With prefix ARG, evaluate visible portion of buffer."
  (interactive "P")
  (if arg
      (winterp-send-buffer)
    (winterp-eval-last-sexp)))

;; ----------------------------------------------------------------[ contrib ]
;;; To: mayer@hplabs.hpl.hp.com
;;; Cc: gildea@alexander.bbn.com
;;; Subject: XLisp comments
;;; Date: Tue, 02 Oct 90 11:45:16 EDT
;;; From: Stephen Gildea <gildea@alexander.bbn.com>
;;; 
;;; I found I needed the function winterp-send-region, so I added it to
;;; winterp.el.  Here it is:

(defun winterp-send-region (start end)
  "[ NEW ]:  Send the current region off to WINTERP for evaluation.
This routine talks to WINTERP's lisp server via the winterp client
program 'wl'..."
  (interactive "r")
  (let ((loadfile (format "/tmp/wl%d.lsp" (user-real-uid))))
    (save-excursion (write-region start end loadfile nil 'nomessage))
    (apply 'start-process "winterp2-client" nil
	   winterp2-client
	   (append winterp2-client-args
		   (list (format
			  "(load \"%s\" :verbose nil :print t)"
			  loadfile))))
    ))

;; --------------------------------------------------------------------------
;; rebind some keys in lisp-mode-map (assumed preloaded in gnuemacs).
;; --------------------------------------------------------------------------

(define-key lisp-mode-map "\C-c\C-d" 'winterp-send-exit)
(define-key lisp-mode-map "\e\C-x"   'winterp-send-defun)
(define-key lisp-mode-map "\e\C-x"   'winterp-eval-last-sexp)
(define-key lisp-mode-map "\M-\C-x"  'winterp-eval)

;; NPM??? -- (if (eq window-system 'x)
;; NPM??? --     (progn (xterm-bind-key "L2"		'winterp-send-buffer)
;; NPM??? -- 	   (xterm-bind-key "L4"		'winterp-send-defun)
;; NPM??? -- 	   (xterm-bind-key "L3"		'winterp-eval-last-sexp)
;; NPM??? -- 	   (xterm-bind-key "L5"		'winterp-eval)
;; NPM??? -- 	   (xterm-bind-key "L6"		'winterp-send-region)
;; NPM??? -- 	   (xterm-bind-key "L10"	'winterp-send-exit)
;; NPM??? -- 	   ))

;; ---------------------------------------------------------------------<eof>
