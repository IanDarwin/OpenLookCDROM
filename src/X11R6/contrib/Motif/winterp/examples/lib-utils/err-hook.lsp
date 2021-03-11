;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         err-hook.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-utils/RCS/err-hook.lsp,v 2.1 1994/06/06 14:54:46 npm Exp $
; Description:  Setup *errhook* for use during WINTERP prototyping or
;		application-test-deliveries. After loading this file, all XLISP
;		language errors occuring during execution while *breakenable*
;		is NIL will pop up a WINTERP dialogue box containing:
;			- an "ok" button to close the window
;			- the error message from XLISP/WINTERP
;			- a button to exit application in case of severe error
;			  or winterp use by novice user of application.
;			- the baktrace: display execution environment of error
;		NOTE: the code in this file (err-hook.lsp) works only with
;		Motif versions >= 1.2; for older versions of Motif (1.0, 1.1),
;		the file err-hook0.lsp is automatically loaded from this file.
;		-------------------------------------------------------------
;		WARNING-1: This routine may fail miserably if a nasty error
;		occurs that might prevent the execution of further
;		widget-creation calls (e.g. out-of-memory/out-of-swap).
;		-------------------------------------------------------------
;		WARNING-2: If something breaks in this *errhook*
;		to get an infinite loop to occur... Seems to work ok for me tho!
;		-------------------------------------------------------------
; Author:       Niels Mayer
; Created:      January 1992
; Modified:     Mon Jun  6 00:35:17 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
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

;; load the contents of this file only if using Motif versions >= 1.2
;; otherwise, load err-hook0.lsp, which works with Motif 1.1 and 1.0.
(cond
 ((and (<= *MOTIF_VERSION* 1) (<= *MOTIF_REVISION* 1))
  (require "lib-utils/err-hook0")
  )
 (t

(require "lib-widgets/text-view")	;define Text_Display_Widget_Class

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
			 (send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :template_dialog
			       "winterp-error-warn-shell" *toplevel_widget*
			       :XMN_MESSAGE_STRING	err-str
			       :XMN_SYMBOL_PIXMAP	"default_xm_error"
			       :XMN_DIALOG_TITLE	"Winterp: XLISP Error Warning"
			       :XMN_DELETE_RESPONSE	:destroy
			       :XMN_AUTO_UNMANAGE	nil
			       :XMN_DEFAULT_POSITION	nil ;need to set this to NIL for XmNx,XmNy settings to work
			       :XMN_X			100 ;!!should PROBABLY BE SET IN APP-DEFAULT!!
			       :XMN_Y			100 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
			       ))
			(ok-button-w
			 (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed ;NOTE--THIS MUST BE A WIDGET, else :call_action_proc "ArmAndActivate" below won't work
			       "ok-button" top-w
			       :XMN_LABEL_STRING	"Close Window"
			       :XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
			       :XMN_FILL_ON_ARM		t
			       :XMN_SHOW_AS_DEFAULT	t
			       :XMN_ALIGNMENT		:alignment_center
			       ))
			(save-button-w
			 (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
			       "save-button" top-w
			       :XMN_LABEL_STRING	"Save Backtrace"
			       :XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
			       :XMN_FILL_ON_ARM		t
			       :XMN_SHOW_AS_DEFAULT	nil
			       :XMN_ALIGNMENT		:alignment_center
			       ))
			(exit-button-w
			 (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
			       "exit-button" top-w
			       :XMN_LABEL_STRING	"Exit Application"
			       :XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
			       :XMN_FILL_ON_ARM		t
			       :XMN_SHOW_AS_DEFAULT	nil
			       :XMN_ALIGNMENT		:alignment_center
			       ))
			(baktrace-w
			 (send Text_Display_Widget_Class :new :managed
			       "baktrace" 
			       (send XM_FRAME_WIDGET_CLASS :new :managed "frame" top-w)
			       :XMN_STRING		baktrace-str
			       :XMN_ROWS		10 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
			       :XMN_COLUMNS		80 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
			       ))
			)

		     ;; this causes the ok button to be pressed when <return> 
		     ;; is entered within XmForm-->XmBulletinBoard 
		     (send top-w :set_values :XMN_DEFAULT_BUTTON ok-button-w)

		     (send ok-button-w :add_callback :XMN_ACTIVATE_CALLBACK '()
			   '(
			     (send top-w :destroy)
			     ))
		     (send save-button-w :add_callback :XMN_ACTIVATE_CALLBACK '()
			   '(
			     (send baktrace-w :save-in-file-dialog
				   "Input filename for backtrace:")
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

		     (send top-w :manage)
		      
		     ;; return NIL if you want to see a normal err printout to stderr...
		     NIL
		     )))
	     )
       ))
     )
  )

(provide "lib-utils/err-hook")

))

