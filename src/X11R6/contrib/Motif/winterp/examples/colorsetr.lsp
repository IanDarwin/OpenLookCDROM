;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         colorsetr.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/colorsetr.lsp,v 2.4 1994/06/06 14:43:20 npm Exp $
; Description:	Each time you load this file, it will bring up a window
;		containing a single slider for red, green, and blue colors.
;		You can use the sliders to create colors interactively, then
;		click the button "Set Color On Selected Widget", followed by
;		clicking on the widget whose color you want to set.  Once the
;		color on a widget has been set, you may move the sliders to
;		change that color value without having to reselect the widget.
;		By bringing up multiple instances of the colorsetr.lsp
;		application you can set multiple color planes in other winterp
;		widgets...  Note that this uses XM_GET_COLORS to generate
;		top/bottom/shadow colors based on the background color you've
;		dialed in. Unless you have a lot of planes on your display, this
;		can cause you to run out of colors quickly. Note that this works
;		only on Motif 1.1 or later.
; Author:       Niels Mayer
; Created:      Mon Oct 29 02:44:55 1990
; Modified:     Sun Jun  5 18:34:47 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.2-OR-LATER-P*, *MOTIF-1.0-P*

(if *MOTIF-1.0-P*
    (error "colorsetr.lsp requires Motif 1.1 or later; Motif 1.0 missing XM_GET_COLORS or :CHANGE_COLOR")
  )

(let (
      toplevel_w
      rc_w apply_pb_w color_label_w
      r_la_w r_scale_w
      g_la_w g_scale_w
      b_la_w b_scale_w
      (background_color (aref (X_ALLOC_N_COLOR_CELLS_NO_PLANES 1) 0))
      )

  (setq toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "colorsetr"
	      :XMN_TITLE	"WINTERP: Color Setter"
	      :XMN_ICON_NAME	"W:colorsetr"
	      ))
  (setq rc_w
	(send XM_FORM_WIDGET_CLASS :new :managed
	      "form" toplevel_w
	      ))
  (setq apply_pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "button_apply" rc_w
	      :XMN_LABEL_STRING		"Set Color On Selected Widget"
	      :XMN_TOP_ATTACHMENT	:attach_position
	      :XMN_BOTTOM_ATTACHMENT	:attach_position
	      :XMN_TOP_POSITION		0
	      :XMN_BOTTOM_POSITION	13
	      :XMN_LEFT_ATTACHMENT	:attach_form
	      :XMN_RIGHT_ATTACHMENT	:attach_form
	      ))
;;;(setq take_pb_w
;;;      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
;;;	    "button_take" rc_w
;;;	    :xmn_label_string "Take Color From Selected Widget"
;;;	    ))
  (setq color_label_w
	(send XM_LABEL_WIDGET_CLASS :new :managed "label_color" rc_w
	      :XMN_LABEL_STRING		"Color"
	      :XMN_TOP_ATTACHMENT	:attach_position
	      :XMN_BOTTOM_ATTACHMENT	:attach_position
	      :XMN_TOP_POSITION		13
	      :XMN_BOTTOM_POSITION	25
	      :XMN_LEFT_ATTACHMENT	:attach_form
	      :XMN_RIGHT_ATTACHMENT	:attach_form
	      ))
  (setq r_la_w
	(send XM_LABEL_GADGET_CLASS :new :managed "label_red" rc_w
	      :XMN_LABEL_STRING		"R:"
	      :XMN_TOP_ATTACHMENT	:attach_position
	      :XMN_BOTTOM_ATTACHMENT	:attach_position
	      :XMN_TOP_POSITION		25
	      :XMN_BOTTOM_POSITION	50
	      :XMN_LEFT_ATTACHMENT	:attach_form
	      ))
  (setq r_scale_w
	(send XM_SCALE_WIDGET_CLASS :new :managed "scale_red" rc_w
	      :XMN_SHOW_VALUE		t
	      :XMN_ORIENTATION		:horizontal
	      :XMN_MAXIMUM		255
	      :XMN_MINIMUM		0
	      :XMN_TOP_ATTACHMENT	:attach_position
	      :XMN_BOTTOM_ATTACHMENT	:attach_position
	      :XMN_TOP_POSITION		25
	      :XMN_BOTTOM_POSITION	50
	      :XMN_LEFT_ATTACHMENT	:attach_widget
	      :XMN_LEFT_WIDGET		r_la_w
	      :XMN_RIGHT_ATTACHMENT	:attach_form
	      ))
  (setq g_la_w
	(send XM_LABEL_GADGET_CLASS :new :managed "label_green" rc_w
	      :XMN_LABEL_STRING		"G:"
	      :XMN_TOP_ATTACHMENT	:attach_position
	      :XMN_BOTTOM_ATTACHMENT	:attach_position
	      :XMN_TOP_POSITION		50
	      :XMN_BOTTOM_POSITION	75
	      :XMN_LEFT_ATTACHMENT	:attach_form
	      ))
  (setq g_scale_w
	(send XM_SCALE_WIDGET_CLASS :new :managed "scale_green" rc_w
	      :XMN_SHOW_VALUE		t
	      :XMN_ORIENTATION		:horizontal
	      :XMN_MAXIMUM		255
	      :XMN_MINIMUM		0
	      :XMN_TOP_ATTACHMENT	:attach_position
	      :XMN_BOTTOM_ATTACHMENT	:attach_position
	      :XMN_TOP_POSITION		50
	      :XMN_BOTTOM_POSITION	75
	      :XMN_LEFT_ATTACHMENT	:attach_widget
	      :XMN_LEFT_WIDGET		g_la_w
	      :XMN_RIGHT_ATTACHMENT	:attach_form
	      ))
  (setq b_la_w
	(send XM_LABEL_GADGET_CLASS :new :managed "label_blue" rc_w
	      :XMN_LABEL_STRING		"B:"
	      :XMN_TOP_ATTACHMENT	:attach_position
	      :XMN_BOTTOM_ATTACHMENT	:attach_position
	      :XMN_TOP_POSITION		75
	      :XMN_BOTTOM_POSITION	100
	      :XMN_LEFT_ATTACHMENT	:attach_form
	      ))
  (setq b_scale_w
	(send XM_SCALE_WIDGET_CLASS :new :managed "scale_blue" rc_w
	      :XMN_SHOW_VALUE		t
	      :XMN_ORIENTATION		:horizontal
	      :XMN_MAXIMUM		255
	      :XMN_MINIMUM		0
	      :XMN_TOP_ATTACHMENT	:attach_position
	      :XMN_BOTTOM_ATTACHMENT	:attach_position
	      :XMN_TOP_POSITION		75
	      :XMN_BOTTOM_POSITION	100
	      :XMN_LEFT_ATTACHMENT	:attach_widget
	      :XMN_LEFT_WIDGET		b_la_w
	      :XMN_RIGHT_ATTACHMENT	:attach_form
	      ))

  (send toplevel_w :realize)

  ;; share same callback code between R, G, and B :XMN_DRAG_CALLBACK...
  (setq apply-sliders-to-color-label
	`(
	  (progv			;locally bind *INTEGER-FORMAT*...
	   '(*INTEGER-FORMAT*) '("%02lx") ;hack: print in hex by setting string used by sprintf in format
	   (send ,color_label_w :set_values
		 :XMN_BACKGROUND
		 (x_store_color ,background_color
				(format nil "#~A~A~A" ;RGB in hexadecimal
					(send ,r_scale_w :get_value) ;R
					(send ,g_scale_w :get_value) ;G
					(send ,b_scale_w :get_value))) ;B
		 )
	   )))

  ;; set up drag callbacks so that we can see result of color immediately
  ;; we also need to set up value changed callbacks below...
  (send r_scale_w :set_callback :XMN_DRAG_CALLBACK '()
	apply-sliders-to-color-label
	)
  (send g_scale_w :set_callback :XMN_DRAG_CALLBACK '()
	apply-sliders-to-color-label
	)
  (send b_scale_w :set_callback :XMN_DRAG_CALLBACK '()
	apply-sliders-to-color-label
	)

  ;; value changed callbacks are needed because colors won't change if you only
  ;; have drag callbacks and you move the slider by means other than dragging.
  (send r_scale_w :set_callback :XMN_VALUE_CHANGED_CALLBACK '()
	apply-sliders-to-color-label
	)
  (send g_scale_w :set_callback :XMN_VALUE_CHANGED_CALLBACK '()
	apply-sliders-to-color-label
	)
  (send b_scale_w :set_callback :XMN_VALUE_CHANGED_CALLBACK '()
	apply-sliders-to-color-label
	)

  (if *MOTIF-1.2-OR-LATER-P*
      ;; Motif >= 1.2 means we can use WIDGET_CLASS method :CHANGE_COLOR 
      (send apply_pb_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	    `(
	      (send (get_moused_widget) :change_color ,background_color)
	      ))
      ;; For Motif 1.1, must use XM_GET_COLORS primitive
      (send apply_pb_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	    `(
	      (let ((color_array (xm_get_colors ,background_color)))
		(send (get_moused_widget) :set_values
		      :XMN_BACKGROUND		,background_color
		      :XMN_FOREGROUND		(aref color_array 1)
		      :XMN_TOP_SHADOW_COLOR	(aref color_array 2)
		      :XMN_BOTTOM_SHADOW_COLOR	(aref color_array 3)
		      :XMN_TROUGH_COLOR		(aref color_array 4)
		      )
		)
	      ))
    )

  (apply #'eval apply-sliders-to-color-label)
  )
