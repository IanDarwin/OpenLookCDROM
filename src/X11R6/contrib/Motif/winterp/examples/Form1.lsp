;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         form-widg1.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/Form1.lsp,v 2.4 1994/06/06 14:43:28 npm Exp $
; Description:  Form widget layout -- same as formtest.c on p 88 of Doug Young's
;		Motif book.					
; Author:       Niels Mayer
; Created:      Fri Jul 27 00:22:55 1990
; Modified:     Sun Jun  5 17:28:55 1994 (Niels Mayer) npm@indeed
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

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "form1shl"
	    :XMN_GEOMETRY	"200x200+1+1"
	    :XMN_TITLE		"WINTERP: XmForm Test 1"
	    :XMN_ICON_NAME	"W:Form1"
	    ))

(setq form_w
      (send XM_FORM_WIDGET_CLASS :new :managed
	    "form" toplevel_w
	    ))

(setq pb1_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb1" form_w
	    :XMN_TOP_ATTACHMENT		:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    ))

(setq pb2_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb2" form_w
	    :XMN_TOP_ATTACHMENT		:attach_widget
	    :XMN_TOP_WIDGET		pb1_w
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_form	    
	    ))

(setq pb3_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb3" form_w
	    :XMN_TOP_ATTACHMENT		:attach_widget
	    :XMN_TOP_WIDGET		pb2_w
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_BOTTOM_ATTACHMENT	:attach_form
	    ))

(send toplevel_w :realize)
