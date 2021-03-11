;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         err-hook.lsp
; RCS:          $Header: err-hook.lsp,v 1.6 92/04/30 16:20:39 mayer Exp $
; Description:  Setup *errhook* for use during WINTERP prototyping or
;		application-test-deliveries. After loading this file, all XLISP
;		language errors occuring during execution while *breakenable*
;		is NIL will pop up a WINTERP dialogue box containing:
;			- an "ok" button to close the window
;			- the error message from XLISP/WINTERP
;			- a button to exit application in case of severe error
;			  or winterp use by novice user of application.
;			- the baktrace: display execution environment of error
;		-------------------------------------------------------------
;		WARNING-1: This routine may fail miserably if a nasty error
;		occurs that might prevent the execution of further
;		widget-creation calls (e.g. out-of-memory/out-of-swap).
;		-------------------------------------------------------------
;		WARNING-2: If something breaks in this *errhook*
;		to get an infinite loop to occur... Seems to work ok for me tho!
;		-------------------------------------------------------------
; Author:       Niels Mayer, HPLabs
; Created:      January 1992
; Modified:     Thu Apr 23 23:08:08 1992 (Niels Mayer) mayer@hplmmws
; Language:     Lisp
; Status:       X11r5 contrib tape release
;
; WINTERP Copyright 1989, 1990, 1991, 1992 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and Niels Mayer not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and Niels Mayer
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Icon stolen from HP Softbench
;;; Mwm*winterp-error-warn-shell*iconImage: usr/softbench/icons/action.h

(let* 
    (
     (fn-str "Function: ")		;needed to compute 'baktrace-str' below
     (fn-len (length fn-str))		;needed to compute 'baktrace-str' below
     (cl-str				;needed to compute 'baktrace-str' below
					;we compute value here because *errhook*
					;-special gets rebound to NIL in
					;-dynamic scope of error hook...
      (format
       nil "~A"
       (setq *errhook*
	     (lambda (hdr cmsg emsg &optional (arg NIL arg-supplied-p))
	       (winterp_show_busy nil)	;incase err occured after '(winterp_show_busy t)'
	       (if (not *breakenable*)	;don't call err-hook if XLISP breakloop enabled -- shouldn't create widgets in breakloop
		   (let* 
		       (
			(baktrace-str
			 ;; This is a hack which filters out all info from the baktrace caused by
			 ;; invoking baktrace from *errhook*. 
			 (let ((out-stream
				(progv	;locally bind *error-output* to get baktrace
				 '(*error-output*) (list (make-string-output-stream))
				 (baktrace) ;put baktrace in string-output-stream *error-output*
				 *error-output*	;return as <out-stream>
				 ))
			       (result "")
			       )
			   (do
			    ((line (read-line out-stream) (read-line out-stream)))
			    ((null line) ;stop at EOF or...
			     )

			    ;; (format t "line='~A', fn-str='~A', cl-str='~A'\n" line fn-str cl-str)

			    (if (and 
				 (>= (length line) fn-len)
				 (string= (subseq line 0 fn-len)   fn-str) ;"Function: "
				 (string= (subseq line fn-len NIL) cl-str) ;"#<Closure: #1b802e>"
				 )
				(progn
				  ;;remove "arguments" assocd w/ *errhook*
				  (read-line out-stream) ;"Arguments:"
				  (read-line out-stream) ;"error"
				  (read-line out-stream) ;NIL or "if continued..."
				  (setq result (get-output-stream-string out-stream))
				  ;; (format t "result=~A\n" result)
				  )
			      )
			    )		;end do
			   result	;return result
			   )
			 )
			(err-str
			 (if cmsg
			     (if arg-supplied-p
				 (format nil
					 "~A: ~A - '~A'\nif continued: ~A"
					 hdr emsg arg cmsg)
			       (format nil
				       "~A: ~A\nif continued: ~A"
				       hdr emsg cmsg)
			       )
			   (if arg-supplied-p
			       (format nil
				       "~A: ~A - '~A'"
				       hdr emsg arg)
			     (format nil
				     "~A: ~A"
				     hdr emsg)
			     )
			   ))
			(top-w
			 (send TOP_LEVEL_SHELL_WIDGET_CLASS :new
			       "winterp-error-warn-shell"
			       :XMN_GEOMETRY		"+100+100" ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
			       :XMN_TITLE		"Winterp: XLISP Error Warning" ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
			       :XMN_ICON_NAME		"W:XlErr" ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
;;;			       :XMN_ICON_PIXMAP		(xm_get_pixmap "/usr/local/include/X11/bitmaps-softbench/action.h" "white" "black") ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
;;;			       :XMN_ICON_MASK		(xm_get_pixmap "/usr/local/include/X11/bitmaps-softbench/actionMask.h" "white" "black")	;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
			       ))
			(form-bb-w
			 (send XM_FORM_WIDGET_CLASS :new :managed
			       "form-bb" top-w
			       ))
			(fix-form-fuckup-rc-w
			 (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
			       "fix-form-fuckup-rc" form-bb-w
			       :XMN_ORIENTATION		:horizontal
			       :XMN_PACKING		:pack_tight
			       :XMN_ENTRY_BORDER	0
			       :XMN_MARGIN_HEIGHT	0
			       :XMN_MARGIN_WIDTH	0
			       :XMN_TOP_ATTACHMENT	:attach_form
			       :XMN_LEFT_ATTACHMENT	:attach_form
			       :XMN_RIGHT_ATTACHMENT	:attach_form
			       ))
			(ok-button-w
			 (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed ;NOTE--THIS MUST BE A WIDGET, else :call_action_proc "ArmAndActivate" below won't work
			       "ok-button" fix-form-fuckup-rc-w
			       :XMN_LABEL_STRING	"Close Window"
			       :XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
			       :XMN_FILL_ON_ARM		t
			       :XMN_SHOW_AS_DEFAULT	t
			       :XMN_ALIGNMENT		:alignment_center
			       ))
			(msg-frame-w
			 (send XM_FRAME_WIDGET_CLASS :new :managed
			       "dia-frame" fix-form-fuckup-rc-w
			       :XMN_MARGIN_HEIGHT	0
			       :XMN_MARGIN_WIDTH	0
			       ))
			(msg-rc-w
			 (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
			       "dia-rc" msg-frame-w
			       :XMN_ORIENTATION		:horizontal
			       :XMN_PACKING		:pack_tight
			       :XMN_ENTRY_BORDER	0
			       :XMN_MARGIN_HEIGHT	0
			       :XMN_MARGIN_WIDTH	0
			       ))
			(msg-sym-w
			 (send XM_LABEL_GADGET_CLASS :new :managed
			       "msg-sym" msg-rc-w
			       :XMN_LABEL_TYPE		:pixmap
			       :XMN_LABEL_PIXMAP	"default_xm_error"
			       :XMN_ALIGNMENT		:alignment_center
			       ))
			(msg-label-w
			 (send XM_LABEL_GADGET_CLASS :new :managed
			       "msg-label" msg-rc-w
			       :XMN_LABEL_TYPE		:STRING
			       :XMN_LABEL_STRING	err-str
			       :XMN_ALIGNMENT		:alignment_center
			       ))
			(exit-button-w
			 (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
			       "exit-button" fix-form-fuckup-rc-w
			       :XMN_LABEL_STRING	"Exit Application"
			       :XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
			       :XMN_ALIGNMENT		:alignment_center
			       ))
			(sep-w
			 (send XM_SEPARATOR_GADGET_CLASS :new :managed
			       "sep" form-bb-w
			       :XMN_ORIENTATION		:horizontal
			       :XMN_TOP_ATTACHMENT	:attach_widget
			       :XMN_TOP_WIDGET		fix-form-fuckup-rc-w
			       :XMN_LEFT_ATTACHMENT	:attach_form
			       :XMN_RIGHT_ATTACHMENT	:attach_form
			       ))
			(baktrace-w
			 (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled
			       "baktrace" form-bb-w
			       :XMN_EDIT_MODE		:MULTI_LINE_EDIT
			       :XMN_EDITABLE		nil
			       :XMN_STRING		baktrace-str
			       :XMN_ROWS		6 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
			       :XMN_COLUMNS		80 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
			       :XMN_TOP_ATTACHMENT	:attach_widget
			       :XMN_TOP_WIDGET		sep-w
			       :XMN_LEFT_ATTACHMENT	:attach_form
			       :XMN_RIGHT_ATTACHMENT	:attach_form
			       :XMN_BOTTOM_ATTACHMENT	:attach_form
			       ))
			)

		     ;; this causes the ok button to be pressed when <return> is entered within 
		     ;; XmForm-->XmBulletinBoard 
		     ;;
		     (send form-bb-w :set_values :xmn_default_button ok-button-w)
		     (send ok-button-w :add_callback :XMN_ACTIVATE_CALLBACK '()
			   '(
			     (send top-w :destroy)
			     ))

		     (send exit-button-w :add_callback :XMN_ACTIVATE_CALLBACK '()
			   '(
			     (exit)
			     ))

		     ;; The following two statements cause the ok button to be pressed when
		     ;; <return> is entered in the text widget Return doesn't get set in XmText despite
		     ;; XmNdefaultButton setting in XmForm-->XmBulletinBoard parent...
		     ;;
		     (send baktrace-w :override_translations "<Key>Return: activate()")
		     (send baktrace-w :add_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
			   '(
			     (send ok-button-w :call_action_proc "ArmAndActivate" CALLBACK_XEVENT)
			     ))
		      
		     (send top-w :realize)

		     ;; return NIL if you want to see a normal err printout to stderr...
		     NIL
		     )))
	     )
       ))
     )
  )
