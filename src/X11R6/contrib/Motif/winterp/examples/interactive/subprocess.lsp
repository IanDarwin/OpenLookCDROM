;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         subprocess.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/subprocess.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Demo of spawning an interactive subprocess and interacting
;		with the subrpocess through XT_ADD_INPUT. Subprocess can be
;		off doing a long calculation while WINTERP GUI remains active.
;		To use this example, WINTERP must be compiled with
;		"WANT_EXPECT_SUBPROCESS" enabled... If you load this file it
;		won't do much -- you should step through the s-expressions
;		interacively using w_ctrlpnl.lsp or emacs&winterp.el
; Author:       Niels Mayer
; Created:      Sat Oct  5 18:58:46 1991
; Modified:     Mon Jun  6 00:29:33 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "lib-utils/unixstuf")		;define read-exec-cmd, unix:get-uname, and other unixisms.

(cond
 ((equal (unix:get-uname) "HP-UX")
  (exp_stty_init "9600 echo")		;HPUX seems to require echoing characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
  )
 (T					;default -- most systems don't require "echo"
  (exp_stty_init "9600 -echo")		;don't echo characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
  )
 )

(setq input-f1 (exp_spawn "bc" "bc"))
(setq input-pid1 (exp_get_pid))
(setq input-cb1
      (xt_add_input			;XtAppAddInput()
       input-f1 :READ_LINE_TO_STRING
       '(;; READ_LINE_TO_STRING fires callback once per line, binding line to FDINPUTCB_STRING
	 (format T "f1: ~S\n" FDINPUTCB_STRING)
	 ))
      )

;; If the process gets killed, this exception callback fires.
;; Works correctly on:
;;	Irix 5.X, SunOS 4.3
;; Doesn't work on:
;;	Solaris 2.3 (completely ignores it),
;;	OSF1 (gets infinite "Error in read(2): I/O error")
;;	HPUX 9.0 (completely ignores it).
(setq input-ecb1
      (xt_add_input input-f1 :except
		    '(
		      (format T "xt_add_input/:except called on ~A\n" input-f1)
		      (xt_remove_input input-ecb1)
		      (xt_remove_input input-cb1)
		      (close input-f1)
		      (exp_wait)
		      (setq input-f1 NIL
			    input-pid1 NIL
			    input-cb1 NIL
			    input-ecb1 NIL)
		      )))

;; (exp_kill "INT" input-pid1)

(format input-f1 "scale=10\n")
(format input-f1 "731 / 223\n")
(format input-f1 "2\n")
(format input-f1 ". ^ 2\n")

;(xt_remove_input input-cb1)
;(xt_add_input input-cb1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
 ((equal (unix:get-uname) "HP-UX")
  (exp_stty_init "9600 echo")		;HPUX seems to require echoing characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
  )
 (T					;default -- most systems don't require "echo"
  (exp_stty_init "9600 -echo")		;don't echo characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
  )
 )

(setq input-f2 (exp_popen "bc"))
(setq input-pid2 (exp_get_pid))
(setq input-cb2
      (xt_add_input			;XtAppAddInput()
       input-f2 :READ_LINE_TO_STRING
       '(;; READ_LINE_TO_STRING fires callback once per line, binding line to FDINPUTCB_STRING
	 (format T "f2: ~S\n" FDINPUTCB_STRING)
	 ))
      )

;; If the process gets killed, this exception callback fires.
;; Works correctly on:
;;	Irix 5.X, SunOS 4.3
;; Doesn't work on:
;;	Solaris 2.3 (completely ignores it),
;;	OSF1 (gets infinite "Error in read(2): I/O error")
;;	HPUX 9.0 (completely ignores it).
(setq input-ecb2
      (xt_add_input input-f2 :except
		    '(
		      (format T "xt_add_input/:except called on ~A\n" input-f2)
		      (xt_remove_input input-ecb2)
		      (xt_remove_input input-cb2)
		      (close input-f2)
		      (exp_wait)
		      (setq input-f2 NIL
			    input-pid2 NIL
			    input-cb2 NIL
			    input-ecb2 NIL)
		      )))

;; must kill this with "HUP" since it was started w/ exp_popen,
;; which spawns a shell (sh) that seems to trap "INT" and "TERM"
;; (exp_kill "HUP" input-pid2) 

(format input-f2 "scale=2\n")
(format input-f2 "731 / 223\n")
(format input-f2 "2\n")
(format input-f2 ". ^ 2\n")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; You MUST call xt_remove_input before close'ing file, otherwise you get
;;; jillions of "error: X Toolkit Warning: Select failed; error code 9" (HPUX).
;;; on other systems (e.g. Irix) it'll just cause select(2) to infinite loop
;;; without outputting any error messages.
;;; In either case, if you call 'close' w/o removing the input callbacks,
;;; you'll have to forcibly kill -9 the WINTERP process.
;;;

(input_active_p input-cb1)
(xt_remove_input input-cb1)
(input_active_p input-cb1)
(xt_remove_input input-ecb1)
(exp_kill "INT" input-pid1)		;OSF1 seems to need this, otherwise 'exp_wait' hangs
(close input-f1)			;however, for most systems 'close' will stop the subprocess
(exp_wait)

(xt_remove_input input-cb2)
(input_active_p input-cb2)
(xt_remove_input input-ecb2)
(exp_kill "HUP" input-pid2)		;OSF1 seems to need this, otherwise 'exp_wait' hangs -- note "HUP" to kill exp_popen's sh.
(close input-f2)			;however, for most systems 'close' will stop the subprocess
(exp_wait)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
 ((equal (unix:get-uname) "HP-UX")
  (exp_stty_init "9600 echo")		;HPUX seems to require echoing characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
  )
 (T					;default -- most systems don't require "echo"
  (exp_stty_init "9600 -echo")		;don't echo characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
  )
 )

(setq bc_proc_io (exp_spawn "bc" "bc"))
(setq bc_proc_pid  (exp_get_pid))

(require "rc-shell")			;'rc_w' is now a XmRowColumn layout manager.

(setq bc_proc_cb
      (xt_add_input			;XtAppAddInput()
       bc_proc_io :READ_LINE_TO_STRING
       '(;; READ_LINE_TO_STRING fires callback once per line, binding line to FDINPUTCB_STRING
	 (send XM_LABEL_GADGET_CLASS :new :managed rc_w
	       :XMN_LABEL_STRING	FDINPUTCB_STRING
	       :XMN_ALIGNMENT		:ALIGNMENT_BEGINNING
	       )
	 ))
      )

;; If the process gets killed, this exception callback fires.
;; Works correctly on:
;;	Irix 5.X, SunOS 4.3
;; Doesn't work on:
;;	Solaris 2.3 (completely ignores it),
;;	OSF1 (gets infinite "Error in read(2): I/O error")
;;	HPUX 9.0 (completely ignores it).
(setq bc_proc_ecb
      (xt_add_input bc_proc_io :except
		    '(
		      (format T "xt_add_input/:except called on ~A\n" bc_proc_io)
		      (xt_remove_input bc_proc_ecb)
		      (xt_remove_input bc_proc_cb)
		      (close bc_proc_io)
		      (exp_wait)
		      (setq bc_proc_io NIL
			    bc_proc_pid NIL
			    bc_proc_cb NIL
			    bc_proc_ecb NIL)
		      )))

;; (exp_kill "INT" bc_proc_pid)

(send rc_w :set_callback :XMN_DESTROY_CALLBACK '()
      '(
	(format T "Destroyed -- widget with subprocess ~A\n" bc_proc_io)
	(xt_remove_input bc_proc_cb)
	(xt_remove_input bc_proc_ecb)
	(input_active_p bc_proc_cb)
	(exp_kill "INT" bc_proc_pid)	;OSF1 seems to need this, otherwise 'exp_wait' hangs
	(close bc_proc_io)		;however, for most systems 'close' will stop the subprocess
	(exp_wait)
	(setq bc_proc_cb NIL
	      bc_proc_ecb NIL
	      bc_proc_cb NIL
	      bc_proc_pid NIL)
	))

(format bc_proc_io "scale=20\n")

(format bc_proc_io "2\n")

(dotimes (i 20)
	 (format bc_proc_io ". ^ 2\n"))

(xt_remove_input bc_proc_cb)
(xt_remove_input bc_proc_ecb)
(input_active_p bc_proc_cb)
(exp_kill "INT" bc_proc_pid)		;OSF1 seems to need this, otherwise 'exp_wait' hangs
(close bc_proc_io)			;however, for most systems 'close' will stop the subprocess
(exp_wait)
