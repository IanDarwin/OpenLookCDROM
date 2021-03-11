;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         redir-err0.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-utils/RCS/redir-err0.lsp,v 2.1 1994/06/06 14:54:46 npm Exp $
; Description:  Motif 1.1 or Motif 1.0 version of redir-err.lsp. See
;		description in that file for details.
; Author:       Niels Mayer
; Created:      January 1992
; Modified:     Mon Jun  6 00:51:37 1994 (Niels Mayer) npm@indeed
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
(require "lib-widgets/text-view")	;define Text_Display_Widget_Class

(if *MOTIF-1.2-OR-LATER-P*
    (error "redir-err0.lsp is to be used with Motif 1.0 or 1.1 only -- use redir-err.lsp for Motif 1.2 or later.")
  )

;;; Icon stolen from HP Softbench
;;; Mwm*winterp-stderr-warn-shell*iconImage: usr/softbench/icons/action.h

;; global holding the stderr redirection input callback. This is used
;; as a hook for people that want to kill the input callback, and also
;; a predictate preventing multiple redirections of stderr when the file
;; is loaded multiple times.
(defvar *STDERR-REDIRECT-INPUT-CB* NIL)
(if (null *STDERR-REDIRECT-INPUT-CB*)
    (let*
	(;; loc vars
	 (top-w
	  (send TOP_LEVEL_POPUP_SHELL_WIDGET_CLASS :new 
		"winterp-stderr-warn-shell" *TOPLEVEL_WIDGET*
		:XMN_GEOMETRY		"+100+100"
		:XMN_TITLE		"WINTERP: Stderr Output Warning"
		:XMN_ICON_NAME		"W:redir-err0"
		:XMN_DELETE_RESPONSE	:UNMAP
;;;	    :XMN_ICON_PIXMAP		(xm_get_pixmap "/usr/local/include/X11/bitmaps-softbench/action.h" "white" "black")
;;;	    :XMN_ICON_MASK		(xm_get_pixmap "/usr/local/include/X11/bitmaps-softbench/actionMask.h" "white" "black")
		))
	 (form-w
	  (send XM_FORM_WIDGET_CLASS :new :managed
		"form" top-w
		))
	 (fix-form-fuckup-rc-w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
		"fix-form-fuckup-rc" form-w
		:XMN_ORIENTATION	:horizontal
		:XMN_PACKING		:pack_tight
		:XMN_ENTRY_BORDER	0
		:XMN_MARGIN_HEIGHT	0
		:XMN_MARGIN_WIDTH	0
		:XMN_TOP_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_form
		:XMN_RIGHT_ATTACHMENT	:attach_form
		))
	 (closbut-w
	  (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed ;NOTE--THIS MUST BE A WIDGET, else :call_action_proc "ArmAndActivate" below won't work
		"close-but" fix-form-fuckup-rc-w
		:XMN_LABEL_STRING	"Close Window"
		:XMN_ALIGNMENT		:alignment_center
		:XMN_FILL_ON_ARM	t
		:XMN_SHOW_AS_DEFAULT	t
		:XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
		))
	 (msg-frame-w
	  (send XM_FRAME_WIDGET_CLASS :new :managed
		"msg-frame" fix-form-fuckup-rc-w
		:XMN_MARGIN_HEIGHT	0
		:XMN_MARGIN_WIDTH	0
		))
	 (msg-rc-w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
		"msg-rc" msg-frame-w
		:XMN_ORIENTATION	:horizontal
		:XMN_PACKING		:pack_tight
		:XMN_ENTRY_BORDER	0
		:XMN_MARGIN_HEIGHT	0
		:XMN_MARGIN_WIDTH	0
		))
	 (msg-sym-w
	  (send XM_LABEL_GADGET_CLASS :new :managed
		"msg-sym" msg-rc-w
		:XMN_LABEL_TYPE		:pixmap
		:XMN_LABEL_PIXMAP	"default_xm_warning"
		:XMN_ALIGNMENT		:alignment_center
		))
	 (msg-label-w
	  (send XM_LABEL_GADGET_CLASS :new :managed
		"msg-label" msg-rc-w
		:XMN_LABEL_TYPE		:string
		:XMN_LABEL_STRING	"Warning -- Error messages on stderr:"
		:XMN_ALIGNMENT		:alignment_beginning
		))
	 (clearbut-w
	  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		"clear-but" fix-form-fuckup-rc-w
		:XMN_LABEL_STRING	"Clear messages"
		:XMN_ALIGNMENT		:alignment_center
		:XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
		))
	 (sep-w
	  (send XM_SEPARATOR_GADGET_CLASS :new :managed
		"sep" form-w
		:XMN_ORIENTATION	:horizontal
		:XMN_TOP_ATTACHMENT     :attach_widget
		:XMN_TOP_WIDGET		fix-form-fuckup-rc-w
		:XMN_LEFT_ATTACHMENT	:attach_form
		:XMN_RIGHT_ATTACHMENT	:attach_form
		))
	 (te-w
	  (send Text_Display_Widget_Class :new :managed
		"text" form-w
		:XMN_ROWS		6 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
		:XMN_COLUMNS		80 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
		:XMN_TOP_ATTACHMENT	:attach_widget
		:XMN_TOP_WIDGET		sep-w
		:XMN_LEFT_ATTACHMENT	:attach_form
		:XMN_RIGHT_ATTACHMENT	:attach_form
		:XMN_BOTTOM_ATTACHMENT	:attach_form
		))
	 )

      ;;
      ;; This callback clears out previous text in the text widget...
      ;;
      (send clearbut-w :add_callback :xmn_activate_callback '() 
	    '(
	      (send te-w :clear)
	      ))

      ;;
      ;; this causes the ok button to be pressed when <return> is entered within 
      ;; XmForm-->XmBulletinBoard 
      ;;
      (send form-w :set_values :xmn_default_button closbut-w)
      (send closbut-w :add_callback :xmn_activate_callback '() 
	    '(
	      (send top-w :popdown)
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

      (setq *STDERR-REDIRECT-INPUT-CB*
	    (xt_add_input		;XtAppAddInput()
	     (REDIRECT_STDERR) :READ_LINE_TO_STRING
	     '(;; read_line_to_string fires callback once per line, binding line to FDINPUTCB_STRING
	       (send te-w :append-string (concatenate 'string FDINPUTCB_STRING "\n"))
	       (send top-w :popup :grab_none)
	       (send top-w :map_raised)	;force to top of stacking order if already up and obscured.
	       )
	     ))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-utils/redir-err0")
