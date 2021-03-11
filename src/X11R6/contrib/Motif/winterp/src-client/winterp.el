; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         winterp.el
; RCS:          $Header: /users/npm/src/winterp/src-client/RCS/winterp.el,v 2.5 1994/06/06 15:14:40 npm Exp $
; Description:  GNUEMACS lisp-mode interface to WINTERP. Works with both
;		emacs version 18.XX and 19.XX.
; Author:       Niels Mayer
; Created:      Tue Nov 14 23:14:54 1989
; Modified:     Sun Jun  5 16:17:36 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure shell.el is loaded, since this file uses stuff defined there.
;; None of this will work unless you load shell.el from emacs
;; versions >= 18.54. I also expect that lisp-mode has been preloaded into
;; gnuemacs (it is automatically loaded by default in "normal" gnuemacsen)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar winterp-client-shell "sh" 
  "A shell in which to run the winterp-server client 'wl' any shell will do
e.g. sh, csh, ksh....")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar winterp-client-program "wl"
  "The name of the client program that sends lisp expressions to the
winterp lisp server. You may want to change this if you use a different
client program, or if you  don't have 'wl' on your search path, you
may want to specify a full path to 'wl'.") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar winterp-client-program-args ""
  "This sets the arguments sent to 'wl'. You may want to set this to
\"-h <hostname> -p <portnum>\" if you are running  winterp on a different
host than gnuemacs, or if you want to run winterp on a TCP port other than
the default, which is 23751.")

;; for emacs 19, 'make-shell' became 'make-comint', so alias it here
;; in case it is missing. (Kludge-o-rama). This way, the same code
;; below will work for both emacs version 18.XX and emacs version 19.XX
(if (not (fboundp 'make-shell))
    (fset 'make-shell (symbol-function 'make-comint))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun winterp-send-defun ()
  "A version of lisp-send-defun that talks to WINTERP's lisp server via the
winterp client program 'wl' (which must be on your search path)."
  (interactive)
  (if (not (get-process "winterp-client-shell"))
      (make-shell "winterp-client-shell" winterp-client-shell)
    )
  (let ((loadfile (format "/tmp/wl%d.lsp"
			  (process-id (get-process "winterp-client-shell")))))
    (save-excursion (write-region
		     (progn (beginning-of-defun) (point))
		     (progn (end-of-defun) (point))
		     loadfile nil 'nomessage))
    (process-send-string "winterp-client-shell"
			 (format "%s %s '(load \"%s\" :verbose nil :print t)'\n"
				 winterp-client-program winterp-client-program-args
				 loadfile))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun winterp-send-exit ()
  "Calling this function will send WINTERP a command to exit the current
breaklevel. If not at a breaklevel, then WINTERP will exit."
  (interactive)
  (if (not (get-process "winterp-client-shell"))
      (make-shell "winterp-client-shell" winterp-client-shell)
    )
  ;;sending wl with no args sends an EOF to WINTERP which signals XLISP to
  ;;exit the current breaklevel, or exit if not in the breakloop.
  (process-send-string "winterp-client-shell" 
		       (format "%s %s\n"
			       winterp-client-program winterp-client-program-args))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun winterp-send-buffer ()
  "A version of winterp-send-defun that sends the entire buffer off to WINTERP
for evaluation. This routine talks to WINTERP's lisp server via the winterp client
program 'wl' (which must be on your search path)."
  (interactive)
  (if (not (get-process "winterp-client-shell"))
      (make-shell "winterp-client-shell" winterp-client-shell)
    )
  (let ((loadfile (format "/tmp/wl%d.lsp"
			  (process-id (get-process "winterp-client-shell")))))
    (save-excursion (write-region (point-min) (point-max)
		     loadfile nil 'nomessage))
    (process-send-string "winterp-client-shell"
			 (format "%s %s '(load \"%s\" :verbose nil :print t)'\n"
				 winterp-client-program winterp-client-program-args
				 loadfile))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rebind some keys in lisp-mode-map (assumed preloaded in gnuemacs).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key lisp-mode-map "\C-c\C-d" 'winterp-send-exit)
(define-key lisp-mode-map "\e\C-x" 'winterp-send-defun)
