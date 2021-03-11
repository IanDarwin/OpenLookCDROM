; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         clock-disp.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/clock-disp.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Define Clock_Display_Widget_Class, a subclass of
;		XM_LABEL_GADGET_CLASS which displays the time in
;		Month/Date/Year Hour:Minute format. This makes use of the
;		Unix 'date' command "date '+\#(%y %m %d %H %M)'" -- if this
;		doesn't work on your system, it's because your unix isn't
;		SVID2, XPG2, XPG3, or POSIX.2 compliant and can't understand
;		the special '+' formatting option for 'date'.
;		Also, to use this widget, WINTERP must be compiled with
;		"WANT_EXPECT_SUBPROCESS" enabled...
; Author:       Niels P. Mayer
; Created:      Tue Oct 12 11:20:09 1993
; Modified:     Mon Jun  6 01:06:25 1994 (Niels Mayer) npm@indeed
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

(defvar Clock_Display_Widget_Class
#|
  nil)
  (setq Clock_Display_Widget_Class
|#
  (send Class :new
	'(				;new instance vars
	  date-array			;the date as an array -- #(<year> <month> <date> <hr> <min>)
	  timer_subproc_pty		;the bidirectional STREAM for communicating w/ subprocess
	  timer_subproc_pid		;the process i.d. of subprocess.
	  timer_subproc_cb		;the input callback processing subprocess' output
	  )
	'()				;no class vars
	XM_LABEL_GADGET_CLASS))		;superclass


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Clock_Display_Widget_Class :answer :ISNEW
      '(managed_k name parent_w &rest args)
      '(
	;; initialize the instance variables; start subprocess; callback...
	(setq date-array
	      #("Yr" "Mo" "Da" "Hr" "Mi")
	      )
	(setq timer_subproc_pty
	      (exp_spawn "sh" "sh" "-c"
			 ;; The subprocess periodically outputs the date/time formatted
			 ;; as an array -- #(<year> <month> <date> <hr> <min>).
			 ;; This output occurs once every 60 seconds and gets read by 
			 ;; xt_add_input/:read_sexp_to_ustream below.
			 "while date '+\#(%y %m %d %H %M)'\ndo\nsleep 60\ndone\n"
			 ))
	(setq timer_subproc_pid
	      (exp_get_pid))
	(setq timer_subproc_cb
	      (xt_add_input		;XtAppAddInput()
	       timer_subproc_pty :READ_SEXP_TO_USTREAM	
	       `(;; :READ_SEXP_TO_USTREAM fires callback once per s-expression, binding FDINPUTCB_USTREAM
		 (setq date-array (read FDINPUTCB_USTREAM))
		 (send ,self :set_values ;display the time...
			     :XMN_LABEL_STRING (send ,self :get-time-string))
		 )
	       ))

	;; initialize the widget...
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k name parent_w
	       :XMN_LABEL_TYPE		:string
	       :XMN_LABEL_STRING	(send self :get-time-string)
	       args
	       )

	;; quit the subprocess when this widget is destroyed
	(send-super :add_callback :XMN_DESTROY_CALLBACK '()
		    '(
		      (xt_remove_input timer_subproc_cb) ;must remove this before closing
		      (exp_kill "INT" timer_subproc_pid) ;OSF1 seems to need this, otherwise 'exp_wait' hangs
		      (close timer_subproc_pty)	;however, for most systems 'close' will stop the subprocess
		      (exp_wait)	;wait on the subprocess
		      ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Clock_Display_Widget_Class :answer :GET-TIME-STRING '()
      '(
	(format nil
		"~2,,,'0@A/~2,,,'0@A/~2,,,'0@A-~2,,,'0@A:~2,,,'0@A"
		(aref date-array 1)	;month
		(aref date-array 2)	;date
		(aref date-array 0)	;year
		(aref date-array 3)	;hour
		(aref date-array 4)	;minute
		)
	))

(send Clock_Display_Widget_Class :answer :GET-MONTH '()
      '(
	(aref date-array 1)
	))

(send Clock_Display_Widget_Class :answer :GET-DATE '()
      '(
	(aref date-array 2)
	))

(send Clock_Display_Widget_Class :answer :GET-YEAR '()
      '(
	(aref date-array 0)
	))

(send Clock_Display_Widget_Class :answer :GET-HOUR '()
      '(
	(aref date-array 3)
	))

(send Clock_Display_Widget_Class :answer :GET-MINUTE '()
      '(
	(aref date-array 4)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/clock-disp")
