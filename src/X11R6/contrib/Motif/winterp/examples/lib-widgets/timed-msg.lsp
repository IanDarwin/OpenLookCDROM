; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         timed-msg.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/timed-msg.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:	TIMED-MESSAGE-DISPLAY-WIDGET-CLASS, a subclass of
;		XM_TEXT_FIELD_WIDGET_CLASS which displays a message for
;		a predetermined amount of time. Useful for "status lines"...
; Author:       Niels Mayer
; Created:      Wed Sep 16 23:00:03 1992
; Modified:     Mon Jun  6 01:10:36 1994 (Niels Mayer) npm@indeed
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; TIMED-MESSAGE-DISPLAY-WIDGET-CLASS -- a subclass of XM_TEXT_FIELD_WIDGET_CLASS
;;;
(setq TIMED-MESSAGE-DISPLAY-WIDGET-CLASS ;name of the new subclass
      (send Class :new
            '(timeout)	;inst variables for subclass
            '()                         ;no class variables for subclass
            XM_TEXT_FIELD_WIDGET_CLASS ;name of the superclass
	    )) 
;;;
;;; Override instance initializer (method :isnew).
;;;
(send TIMED-MESSAGE-DISPLAY-WIDGET-CLASS :answer :isnew
      '(managed_k widget_name widget_parent &rest args)
      '(
	;; create 'self', an instance of XM_TEXT_FIELD_WIDGET_CLASS
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k widget_name widget_parent
	       :XMN_EDITABLE	nil
;;;	       :XMN_FOREGROUND	(car (send widget_parent :get_values :XMN_FOREGROUND nil))
;;;	       :XMN_BACKGROUND	(car (send widget_parent :get_values :XMN_BACKGROUND nil))
	       args
	       )

	(send-super :add_callback :XMN_DESTROY_CALLBACK '()
		    '(
		      (if timeout
			  (progn
			    (unwind-protect (xt_remove_timeout timeout) NIL)
			    (setq timeout NIL)
			    ))
		      ))
	))


;;;
;;; Method :DISPLAY-STRING -- temporarily display a string
;;;
(send TIMED-MESSAGE-DISPLAY-WIDGET-CLASS :answer :DISPLAY-STRING
      '(string &optional time)
      '(
	(if timeout
	    (progn
	      (unwind-protect (xt_remove_timeout timeout) NIL)
	      (setq timeout NIL)
	      ))

	(send-super :set_string string) ;display message
	(send-super :update_display)	;show it right away
	      
	(if (null time)
	    (setq time 3000)		;default display time is 3 seconds
	  )
	  
	(setq timeout
	      (xt_add_timeout
	       time
	       '(
		 (send-super :set_string "") ;clear it after timeout passes
		 (setq timeout NIL)
		 )
	       ))
	))


;;;
;;; Method :ERROR-DISPLAY-STRING
;;;
(send TIMED-MESSAGE-DISPLAY-WIDGET-CLASS :answer :ERROR-DISPLAY-STRING
      '(string &optional time)
      '(
	(if time
	    (send self :DISPLAY-STRING string time)
	  (send self :DISPLAY-STRING string)
	  )
	(X_BELL)			;SIGNAL ERROR -- BEEP
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/timed-msg")
