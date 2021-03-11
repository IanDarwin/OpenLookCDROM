;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         redir-out.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-utils/RCS/redir-out.lsp,v 2.1 1994/06/06 14:54:46 npm Exp $
; Description:  After loading this file, all output to stdout from WINTERP and
;		it's subprocesses (via exp_popen exp_spawn system popen) gets 
;		output to a XmText widget. If popped-down, this window pops
;		up upon new activity on stdout.
; Author:       Niels Mayer
; Created:      January 1992
; Modified:     Mon Jun  6 00:51:51 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.2-OR-LATER-P*

;; load the contents of this file only if using Motif versions >= 1.2
;; otherwise, load redir-err0.lsp, which works with Motif 1.1 (and perhaps 1.0?).
(cond
 ((not *MOTIF-1.2-OR-LATER-P*)
  (require "lib-utils/redir-out0")
  )
 (t

(require "lib-widgets/text-view")	;define Text_Display_Widget_Class

;; global holding the stdout redirection input callback. This is used
;; as a hook for people that want to kill the input callback, and also
;; a predictate preventing multiple redirections of stdout when the file
;; is loaded multiple times.
(defvar *STDOUT-REDIRECT-INPUT-CB* NIL)
(if (null *STDOUT-REDIRECT-INPUT-CB*)
    (let*
	(;; loc vars
	 (top-w
	  (send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :template_dialog
		"winterp-stdout-warn-shell" *toplevel_widget*
		:XMN_MESSAGE_STRING	"Warning -- messages on stdout:"
		:XMN_SYMBOL_PIXMAP	"default_xm_warning"
		:XMN_DIALOG_TITLE	"Winterp: Stdout Output Warning"
		:XMN_DELETE_RESPONSE	:unmap
		:XMN_AUTO_UNMANAGE	nil
		:XMN_DEFAULT_POSITION	nil ;need to set this to NIL for XmNx,XmNy settings to work
		:XMN_X			100 ;!!should PROBABLY BE SET IN APP-DEFAULT!!
		:XMN_Y			100 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
		))
	 (closbut-w
	  (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed ;NOTE--THIS MUST BE A WIDGET, else :call_action_proc "ArmAndActivate" below won't work
		"close-but" top-w
		:XMN_LABEL_STRING	"Close Window"
		:XMN_ALIGNMENT		:alignment_center
		:XMN_FILL_ON_ARM	t
		:XMN_SHOW_AS_DEFAULT	t
		:XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
		))
	 (savebut-w
	  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		"save-but" top-w
		:XMN_LABEL_STRING	"Save messages"
		:XMN_ALIGNMENT		:alignment_center
		:XMN_FILL_ON_ARM	t
		:XMN_SHOW_AS_DEFAULT	nil
		:XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
		))
	 (clearbut-w
	  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		"clear-but" top-w
		:XMN_LABEL_STRING	"Clear messages"
		:XMN_ALIGNMENT		:alignment_center
		:XMN_FILL_ON_ARM	t
		:XMN_SHOW_AS_DEFAULT	nil
		:XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
		))
	 (te-w
	  (send Text_Display_Widget_Class :new :managed
		"text"
		(send XM_FRAME_WIDGET_CLASS :new :managed "frame" top-w)
		:XMN_ROWS		6 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
		:XMN_COLUMNS		80 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
		))
	 (popped-up-p nil)
	 )

      (send top-w :add_callback :XMN_UNMAP_CALLBACK '()
	    '(
	      (setq popped-up-p nil)
	      ))

      (send top-w :add_callback :XMN_MAP_CALLBACK '()
	    '(
	      (setq popped-up-p t)
	      ))
      ;;
      ;; This callback clears out previous text in the text widget...
      ;;
      (send clearbut-w :add_callback :XMN_ACTIVATE_CALLBACK '() 
	    '(
	      (send te-w :clear)
	      ))

      ;;
      ;; this causes the ok button to be pressed when <return> is entered within 
      ;; XmForm-->XmBulletinBoard 
      ;;
      (send top-w :set_values :XMN_DEFAULT_BUTTON closbut-w)
      (send closbut-w :add_callback :XMN_ACTIVATE_CALLBACK '() 
	    '(
	      (send top-w :unmanage)
	      ))

      ;;
      ;; Save the output...
      ;;
      (send savebut-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	    '(
	      (send te-w :save-in-file-dialog
		    "Input filename for saving:")
	      ))

      ;;
      ;; The following two statements cause the ok button to be pressed when
      ;; <return> is entered in the text widget. Return doesn't get set in XmText
      ;; despite XmNdefaultButton setting in XmForm-->XmBulletinBoard parent...
      ;;
      (send te-w :override_translations "<Key>Return: activate()")
      (send te-w :add_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	    '(
	      (send closbut-w :call_action_proc "ArmAndActivate" CALLBACK_XEVENT)
	      ))

      (setq *STDOUT-REDIRECT-INPUT-CB*
	    (xt_add_input		;XtAppAddInput()
	     (REDIRECT_STDOUT) :READ_LINE_TO_STRING
	     '(;; READ_LINE_TO_STRING fires callback once per line, binding line to FDINPUTCB_STRING
	       (send te-w :append-string (concatenate 'string FDINPUTCB_STRING "\n"))
	       (if (null popped-up-p)
		   (send top-w :manage)
		 (send (send top-w :parent) :map_raised) ;force to top of stacking order if already up and obscured.
		 )
	       )
	     ))
      ))
(provide "lib-utils/redir-out")
))
