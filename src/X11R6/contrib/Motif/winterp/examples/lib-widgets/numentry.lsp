; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         numentry.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/numentry.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Cardinal_Number_Entry_Field_Widget_Class, a subclasses of
;		XM_TEXT_FIELD_WIDGET_CLASS.
; Author:       Niels P. Mayer
; Created:      Sun Apr 17 20:50:52 1994 (Niels Mayer) npm@indeed
; Modified:     Mon Jun  6 01:07:55 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Cardinal_Number_Entry_Field_Widget_Class NIL)
(setq Cardinal_Number_Entry_Field_Widget_Class
      (send Class :new
	    '(value)			;new instance vars
	    '()				;no class vars
	    XM_TEXT_FIELD_WIDGET_CLASS)) ;superclass

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Cardinal_Number_Entry_Field_Widget_Class :answer :ISNEW
      '(managed_k name parent_w &rest args)
      '(
	;; initialize the instance variables
	(setq value NIL)

	;; initialize the widget...
	(apply #'send-super :isnew
	       managed_k name parent_w
	       :XMN_EDITABLE			t
	       :XMN_CURSOR_POSITION_VISIBLE	t
	       :XMN_AUTO_SHOW_CURSOR_POSITION	t
	       :XMN_STRING			""
	       args
	       )
	(send-super :add_callback :XMN_ACTIVATE_CALLBACK
		    '(CALLBACK_WIDGET)
		    '(
		      (setq value
			    (unwind-protect
				(read
				 (make-string-input-stream
				  (send CALLBACK_WIDGET :get_string)))
			      NIL))
		      (cond
		       ((not (numberp value))
			(X_BELL)	;SIGNAL ERROR -- BEEP
			(send CALLBACK_WIDGET :set_string "")
			(setq value NIL)
			)
		       ((< value 0)
			(X_BELL)	;SIGNAL ERROR -- BEEP
			(send CALLBACK_WIDGET :set_string "")
			(setq value NIL)
			)
		       ((integerp value)
			(format T "user entered fixnum ~A\n" value)
			)
		       ((floatp value)
			(format T "user entered flonum ~A\n" value)
			)
		       )
		      ))
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Cardinal_Number_Entry_Field_Widget_Class :answer :GET_VALUE
      '()
      '(
	value
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/numentry")
