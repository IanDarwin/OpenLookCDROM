;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         form-widg2.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/Form2.lsp,v 2.4 1994/06/06 14:43:29 npm Exp $
; Description:  Form widget layout -- similar to formtest.c on p 91-92 of
;		Doug Young's Motif book.	
; Author:       Niels Mayer
; Created:      Fri Jul 27 00:59:56 1990
; Modified:     Sun Jun  5 17:28:48 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "form2shl"
	    :XMN_GEOMETRY	(if *MOTIF-1.1-OR-LATER-P* "+1+1" "300x300+1+1") ;work around Motif 1.0 form-sizing bug.
	    :XMN_TITLE		"WINTERP: XmForm Test 2"
	    :XMN_ICON_NAME	"W:Form2"
	    ))

(setq form_w
      (send XM_FORM_WIDGET_CLASS :new :managed
	    "form" toplevel_w
	    ))

;;
;; place 5 equal-sized buttons attached to the right side of the form widget
;; we specify the :xmn_{top,bottom}_attachment resources as :attach_position,
;; which means that we must specify the :xmn_top_position, :xmn_bottom_position
;; as the vertical percentage taken up by each button. (for 5 buttons, that means
;; each button takes up approx 20% (make that 18 + 2 for spacing)...
;; if more buttons are specified, the percentages must be changed to the
;; appropriate value.
;;

(setq pb0_w
      (send XM_TEXT_WIDGET_CLASS :new :managed
	    "pb0" form_w
	    :XMN_STRING		"Mothra\nIn\nPupa\nStage"
	    :XMN_EDIT_MODE		:multi_line_edit
	    :XMN_TOP_ATTACHMENT		:attach_form
	    :XMN_BOTTOM_ATTACHMENT	:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    ))

(setq pb1_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb1" form_w
	    :XMN_LABEL_STRING		"Jack In"
	    :XMN_TOP_ATTACHMENT		:attach_position
	    :XMN_BOTTOM_ATTACHMENT	:attach_position
	    :XMN_TOP_POSITION		0
	    :XMN_BOTTOM_POSITION	20
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_widget
	    :XMN_LEFT_WIDGET		pb0_w
	    ))

(setq pb2_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb2" form_w
	    :XMN_LABEL_STRING		"Jack Out"
	    :XMN_TOP_ATTACHMENT		:attach_position
	    :XMN_BOTTOM_ATTACHMENT	:attach_position
	    :XMN_TOP_POSITION		20
	    :XMN_BOTTOM_POSITION	40
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_widget
	    :XMN_LEFT_WIDGET		pb0_w
	    ))

(setq pb3_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb3" form_w
	    :XMN_LABEL_STRING		"Do Watoosi"
	    :XMN_TOP_ATTACHMENT		:attach_position
	    :XMN_BOTTOM_ATTACHMENT	:attach_position
	    :XMN_TOP_POSITION		40
	    :XMN_BOTTOM_POSITION	60
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_widget
	    :XMN_LEFT_WIDGET		pb0_w
	    ))

(setq pb4_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb4" form_w
	    :XMN_LABEL_STRING		"Begin Hyperreality"
	    :XMN_TOP_ATTACHMENT	:attach_position
	    :XMN_BOTTOM_ATTACHMENT	:attach_position
	    :XMN_TOP_POSITION		60
	    :XMN_BOTTOM_POSITION	80
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_widget
	    :XMN_LEFT_WIDGET		pb0_w
	    ))

(setq pb5_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb5" form_w
	    :XMN_LABEL_STRING		"Winterpmute"
	    :XMN_TOP_ATTACHMENT		:attach_position
	    :XMN_BOTTOM_ATTACHMENT	:attach_position
	    :XMN_TOP_POSITION		80
	    :XMN_BOTTOM_POSITION	100
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_widget
	    :XMN_LEFT_WIDGET		pb0_w
	    ))

(send toplevel_w :realize)
