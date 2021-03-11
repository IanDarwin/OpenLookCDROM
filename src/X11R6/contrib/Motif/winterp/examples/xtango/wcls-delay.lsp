; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wcls-delay.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/wcls-delay.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  XTANGO-DELAY-SELECTION-WIDGET-CLASS -- dialog box for entering
;		animation delay factor for xtango.
; Author:       Niels P. Mayer
; Created:      Mon Jun  6 04:21:05 1994
; Modified:     Mon Jun  6 04:21:46 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XTANGO-DELAY-SELECTION-WIDGET-CLASS -- a subclass of XM_SELECTION_BOX_WIDGET_CLASS/:prompt_dialog
;;;
(setq XTANGO-DELAY-SELECTION-WIDGET-CLASS ;name of the new subclass
      (send Class :new
            '()				;no inst variables for subclass
            '()                         ;no class variables for subclass
            XM_SELECTION_BOX_WIDGET_CLASS ;name of the superclass
	    )) 
;;;
;;; Override instance initializer (method :isnew).
;;;
(send XTANGO-DELAY-SELECTION-WIDGET-CLASS :answer :isnew
      '(managed_k widget_name widget_parent
		  tango_w		;NOTE special extra widget creation arg, a tango:widget_class instance
		  help_w		;NOTE special extra widget creation arg, a TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
		  delay_fixnum		;NOTE special extra widget creation arg, a FIXNUM setting up the delay value for xtango animation
		  &rest args)
      '(
	;; create 'self', an instance of XM_SELECTION_BOX_WIDGET_CLASS/:prompt_dialog
	(apply #'send-super :isnew	;call superclass's init to create widget
	       :unmanaged		;don't manage till through twiddling geometry by unmanaging children		 
	       :prompt_dialog widget_name widget_parent
	       :XMN_DELETE_RESPONSE		:unmap
	       :XMN_AUTO_UNMANAGE		nil
	       :XMN_OK_LABEL_STRING		"Apply"	;since we don't automatically unmanage
	       :XMN_DIALOG_TITLE		"Set Tango Delay Value"
	       :XMN_SELECTION_LABEL_STRING	"Input Tango Delay Value (integer):"
	       args
	       )
	(send (send-super :get_child :DIALOG_APPLY_BUTTON) :unmanage) ;don't need this...
	(send (send-super :get_child :DIALOG_TEXT) :set_string
	      (format nil "~A" delay_fixnum))
	(if (eq :managed managed_k)
	    (send-super :manage)	;manage the parent only when finished removing children
	  )

	(send-super :add_callback :XMN_OK_CALLBACK '(CALLBACK_VALUE)
	     `(
	       (let ((value
		      (unwind-protect
			  (read (make-string-input-stream (xm_string_get_l_to_r CALLBACK_VALUE)))
			NIL)))
		 (if (integerp value)
		     (if (< value 0)
			 (send ,tango_w :SET_DELAY 0)
		       (send ,tango_w :SET_DELAY value))
		   (progn
		     (X_BELL)		;SIGNAL ERROR -- BEEP
		     (send (send-super :get_child :DIALOG_TEXT) :set_string "")
		     )
		   ))
	       ))

	(send-super :add_callback :XMN_CANCEL_CALLBACK '()
	    '(
	      (send-super :unmanage)
	      ))

	(send-super :add_callback :XMN_HELP_CALLBACK '()
	    '(
	      (send help_w :error-display-string "Help not implemented... (sorry!).")
	      ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/wcls-delay")
