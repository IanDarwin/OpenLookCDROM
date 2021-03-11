;; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         2label-pb.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/2label-pb.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Widget hackery -- A composite widget masquerading as a special
;		pushbutton containing both a pixmap and a label. Major
;		difference between this "pseudo-widget" and a real pushbutton
;		is that you don't do :add_callback :XMN_ACTIVATE_CALLBACK, but
;		rather, you set a callback via method :SET-ACTIVATE-CLOSURE.
; Author:       Niels P. Mayer
; Created:      Mon Jun  6 00:59:23 1994
; Modified:     Mon Jun  6 01:03:06 1994 (Niels Mayer) npm@indeed
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
;;; Example useage:
;;; (require "lib-utils/initialize")	;define *X11-BITMAPS-DIRECTORY*
;;; (require "lib-widgets/2label-pb"    ;define 2LABEL_PUSH_BUTTON_WIDGET_CLASS
;;; (require "rc-shell")
;;; 
;;; (send
;;;  (send 2LABEL_PUSH_BUTTON_WIDGET_CLASS :new :managed
;;;        "pb" rc_w
;;;        "pushbutton"
;;;        (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo32")
;;;        :XMN_FOREGROUND "yellow"
;;;        :XMN_BACKGROUND "dimgrey"
;;;        )
;;;  :set-activate-closure
;;;  #'(lambda ()
;;;      (format T "push button pressed\n")
;;;      ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "lib-utils/motif-vers")	;define *MOTIF-1.2-OR-LATER-P*

;; the code below makes use of new Motif 1.2 features for the XmFrame widget
(if (not *MOTIF-1.2-OR-LATER-P*)
    (error "'lib-widgets/2label-pb' must be used with Motif version 1.2 or later."))

(defvar 2LABEL_PUSH_BUTTON_WIDGET_CLASS NIL)
(setq 2LABEL_PUSH_BUTTON_WIDGET_CLASS
      (send Class :new 
	    '(la_w pi_w activate_callback arm_pix top_pix bot_pix back_pix)
	    '()
	    XM_FRAME_WIDGET_CLASS
	    ))

(send 2LABEL_PUSH_BUTTON_WIDGET_CLASS :answer :ISNEW
      '(managed_k name parent_w text_str icon_pmap &rest args)
      '(
	;; initialize SELF, instance of XM_FRAME_WIDGET_CLASS
	(apply #'send-super :isnew managed_k
	       name parent_w
	       :XMN_SHADOW_TYPE				:shadow_out
	       args
	       )
	(send-super :get_values :XMN_BACKGROUND 'back_pix)
	(setq pi_w
	      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		    "pmap" self
		    :XMN_LABEL_TYPE			:pixmap
		    :XMN_LABEL_PIXMAP			icon_pmap
		    :XMN_CHILD_TYPE			:frame_title_child ;XmFrame constraint resource
		    :XMN_CHILD_VERTICAL_ALIGNMENT	:alignment_widget_bottom ;XmFrame constraint resource
		    :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
		    :XMN_CHILD_HORIZONTAL_SPACING	0 ;XmFrame constraint resource
		    :XMN_SHADOW_THICKNESS		0
		    :XMN_HIGHLIGHT_THICKNESS		0
		    :XMN_HIGHLIGHT_ON_ENTER		NIL
		    :XMN_MARGIN_BOTTOM			0
		    :XMN_MARGIN_TOP			0
		    :XMN_MARGIN_LEFT			0
		    :XMN_MARGIN_RIGHT			0
		    :XMN_MARGIN_HEIGHT			0
		    :XMN_MARGIN_WIDTH			0
		    ))
	(setq la_w
	      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		    "str" self
		    :XMN_LABEL_TYPE			:string
		    :XMN_LABEL_STRING			text_str
		    :XMN_CHILD_TYPE			:frame_workarea_child ;XmFrame constraint resource
		    :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
		    :XMN_CHILD_HORIZONTAL_SPACING	0 ;XmFrame constraint resource
		    :XMN_SHADOW_THICKNESS		0
		    :XMN_HIGHLIGHT_THICKNESS		0
		    :XMN_HIGHLIGHT_ON_ENTER		NIL
		    :XMN_MARGIN_BOTTOM			0
		    :XMN_MARGIN_TOP			0
		    :XMN_MARGIN_LEFT			0
		    :XMN_MARGIN_RIGHT			0
		    :XMN_MARGIN_HEIGHT			0
		    :XMN_MARGIN_WIDTH			0
		    ))
	(send la_w :get_values
	      :XMN_ARM_COLOR		'arm_pix
	      :XMN_TOP_SHADOW_COLOR	'top_pix
	      :XMN_BOTTOM_SHADOW_COLOR	'bot_pix
	      )

	(send-super :OVERRIDE_TRANSLATIONS
		    "<Btn1Down>:          Lisp(send ACTION_WIDGET :button-1-press-callproc ACTION_XEVENT) \
                     <Btn1Down>(2+):      Lisp(send ACTION_WIDGET :button-1-press-callproc ACTION_XEVENT) \
                     <Btn1Down>,<Btn1Up>: Lisp(send ACTION_WIDGET :activate-callproc ACTION_XEVENT) \
                     <Btn1Up>:            Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) \
                     <Btn1Up>(2+):        Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) \
                     ")
	))

(send 2LABEL_PUSH_BUTTON_WIDGET_CLASS :answer :SET-ACTIVATE-CLOSURE
      '(callproc)
      '(
	(setq activate_callback callproc)
	))

(send 2LABEL_PUSH_BUTTON_WIDGET_CLASS :answer :BUTTON-1-PRESS-CALLPROC
      '(xevent)
      '(
	(send self :set_values
	      :XMN_TOP_SHADOW_COLOR	bot_pix
	      :XMN_BOTTOM_SHADOW_COLOR	top_pix
	      :XMN_BACKGROUND		arm_pix
	      )
	))

(send 2LABEL_PUSH_BUTTON_WIDGET_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
      '(xevent)
      '(
	(send self :set_values
	      :XMN_TOP_SHADOW_COLOR	top_pix
	      :XMN_BOTTOM_SHADOW_COLOR	bot_pix
	      :XMN_BACKGROUND		back_pix
	      )
	))

(send 2LABEL_PUSH_BUTTON_WIDGET_CLASS :answer :ACTIVATE-CALLPROC
      '(xevent)
      '(
	(send self :BUTTON-ANY-RELEASE-CALLPROC xevent)
	(funcall activate_callback)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/2label-pb")
