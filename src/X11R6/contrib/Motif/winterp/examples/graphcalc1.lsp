;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         graphcalc-options.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/graphcalc1.lsp,v 2.6 1994/06/06 14:43:15 npm Exp $
; Description:  Add an "options" button and popup panel for graphcalc.lsp
; Author:       Niels Mayer
; Created:      Tue Jul 10 10:35:58 1990
; Modified:     Sun Jun  5 18:44:53 1994 (Niels Mayer) npm@indeed
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


(setq graph_opts_pbw
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "Node Spacing Options..." graph_controlpanel_w
	    ))

(send graph_opts_pbw :set_callback :xmn_activate_callback '()
      '(
	;; create the dialog if it hasn't been created yet
	(if (null graph_options_dialog_w)
	  (setq graph_options_dialog_w (create-graph-options-dialog top_w)))

	;; make it visible
;	(send graph_options_dialog_w :popup :grab_none)
	(send graph_options_dialog_w :manage)
	))

(progn 
  (setq graph_options_dialog_w nil)
  (defun create-graph-options-dialog (parent_w)
    (let*
	((dialog_w
	  (send XM_FORM_WIDGET_CLASS :new :dialog
		"graphcalc-options" parent_w
		:XMN_DIALOG_TITLE	"GraphCalc: Node Spacing"
		:XMN_DELETE_RESPONSE	:unmap
		:XMN_AUTO_UNMANAGE	nil
		:XMN_FRACTION_BASE	3
		))
;;;	 (msg_box_w
;;;	  (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed "msg_box_w" dialog_w
;;;		))
	 )

      (setq childspacing_pbw
	    (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
		  "Apply" dialog_w
		  :XMN_TOP_ATTACHMENT		:attach_form
		  :XMN_LEFT_ATTACHMENT		:attach_form
		  ))
      (setq childspacing_slider_w
	    (send XM_SCALE_WIDGET_CLASS :new :managed "disp-slider" dialog_w
		  :XMN_TITLE_STRING		"Child Spacing (horizontal)"
		  :XMN_ORIENTATION		:horizontal
		  :XMN_PROCESSING_DIRECTION	:MAX_ON_RIGHT 
		  :XMN_SENSITIVE		t
		  :XMN_SHOW_VALUE		t
		  :XMN_MINIMUM			0
		  :XMN_MAXIMUM			+100
		  :XMN_VALUE			20
		  :XMN_WIDTH			200
		  :XMN_TOP_ATTACHMENT		:attach_form
		  :XMN_LEFT_ATTACHMENT		:attach_widget
		  :XMN_LEFT_WIDGET		childspacing_pbw
		  :XMN_RIGHT_ATTACHMENT		:attach_form
		  ))
      (setq sibspacing_pbw
	    (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
		  "Apply" dialog_w
		  :XMN_TOP_ATTACHMENT		:attach_widget
		  :XMN_TOP_WIDGET		childspacing_slider_w
		  :XMN_LEFT_ATTACHMENT		:attach_form
		  ))
      (setq sibspacing_slider_w
	    (send XM_SCALE_WIDGET_CLASS :new :managed "disp-slider" dialog_w
		  :XMN_TITLE_STRING		"Sibling Spacing (vertical)"
		  :XMN_ORIENTATION		:horizontal
		  :XMN_PROCESSING_DIRECTION	:MAX_ON_RIGHT 
		  :XMN_SENSITIVE		t
		  :XMN_SHOW_VALUE		t
		  :XMN_MINIMUM			0
		  :XMN_MAXIMUM			+100
		  :XMN_VALUE			20
		  :XMN_WIDTH			200
		  :XMN_TOP_ATTACHMENT		:attach_widget
		  :XMN_TOP_WIDGET		childspacing_slider_w
		  :XMN_LEFT_ATTACHMENT		:attach_widget
		  :XMN_LEFT_WIDGET		sibspacing_pbw
		  :XMN_RIGHT_ATTACHMENT		:attach_form
		  ))
      (setq close_pbw
	    (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
		  "Close" dialog_w
		  :XMN_TOP_ATTACHMENT		:attach_widget
		  :XMN_TOP_WIDGET		sibspacing_slider_w
		  :XMN_LEFT_ATTACHMENT		:attach_position
		  :XMN_LEFT_POSITION		1
		  :XMN_RIGHT_ATTACHMENT		:attach_position
		  :XMN_RIGHT_POSITION		2
		  ))
      (send dialog_w :set_values
	    :XMN_CANCEL_BUTTON close_pbw
	    :XMN_DEFAULT_BUTTON close_pbw
	    )
      (send childspacing_pbw :set_callback :xmn_activate_callback '()
	    '(
	      (send graph_w :set_values :xmn_child_spacing  (send childspacing_slider_w :get_value))
	      ))
      (send sibspacing_pbw :set_callback :xmn_activate_callback '()
	    '(
	      (send graph_w :set_values :xmn_sibling_spacing (send sibspacing_slider_w :get_value))
	      ))
      (send close_pbw :set_callback :xmn_activate_callback '()
	    `(
	      (send ,dialog_w :unmanage)
	      ))

      dialog_w				;return
      ))
  )






