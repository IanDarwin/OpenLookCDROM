; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         test-compo.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/test-compo.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Tests on TANGO:COMPOSITE_IMAGE_CLASS. The statements in this
;		file should be evaluated interactively; the results of loading
;		this file are somewhat uninteresting.
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 04:04:33 1994 (Niels Mayer) npm@indeed
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

(progn
  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	      "test-compo"
	      :XMN_TITLE		"WINTERP: Xtango Composite Image Test"
	      :XMN_ICON_NAME		"W:test-compo"
	      ))

  (setq tango_w
	(send TANGO:WIDGET_CLASS :new :managed
	      "tango" top_w
	      :XMN_HEIGHT 300
	      :XMN_WIDTH 300
	      ))

  (send tango_w :set_event_handler BUTTON_PRESS_MASK
	'(EVHANDLER_WIDGET EVHANDLER_XEVENT)
	'(
	  (let ((timage (send EVHANDLER_WIDGET :get_event_image EVHANDLER_XEVENT)))
	    (if timage
		(progn
		  (send timage :tap_flash :perform 1)
		  (format t
			  "\
------------------------------------------------------------------------------\
TANGO-LOC: ~A\
TANGO-IMAGE: ~A\
IMAGE-CLASS: ~A\
"
			  (send timage :image_loc :ctr)
			  (generic timage)
			  (generic (send timage :class)))
		  )
	      ))
	  ))

  (progn
    (send top_w :realize)
    (send tango_w :forced_expose_update) ;wait until exposed to ensure windows created for :begin_drawing call
    (send tango_w :begin_drawing)	;must call this after :realize
    )

  ;; optional...
  (setq expose_callback 
	(send tango_w :add_callback :xmn_expose_callback
	      '(CALLBACK_WIDGET)
	      '(
		(send CALLBACK_WIDGET :refresh)
		)))
  )

(setq comp_image
      (send TANGO:COMPOSITE_IMAGE_CLASS :new :show tango_w #C(0.25 0.25)
	    TANGO:LINE_IMAGE_CLASS #C(0.0 0.0) #C(0.2 0.2) "black" 0.0 1.0 :both_arrow
	    TANGO:CIRCLE_IMAGE_CLASS #C(0.0 0.0) 0.1 "orange" 0.0
	    TANGO:SPLINE_IMAGE_CLASS #C(0.0 0.0) #C(+0.05 +0.10) #C(+0.03 -0.10) #C(-0.02 +0.10) #C(+0.07 -0.10) #C(-0.08 +0.10) #C(+0.09 +0.10) #C(-0.09 -0.10) "magenta" 0.0 1.0
	    TANGO:POLYGON_IMAGE_CLASS #C(0.2 0.2) #C(+0.10 +0.10) #C(-0.10 +0.05) #C(-0.10 +0.10) #C(+0.10 -0.10) #C(+0.10 -0.10) #C(+0.10 +0.10) "green" 1.0
	    ))

;(send comp_image :tx_delete :perform)

(setq comp_image
      (send TANGO:COMPOSITE_IMAGE_CLASS :new :show tango_w #C(0.5 0.5)
	    TANGO:LINE_IMAGE_CLASS #C(0.0 0.0) #C(0.2 0.2) "black" 0.0 1.0 :no_arrow
	    TANGO:CIRCLE_IMAGE_CLASS #C(0.0 0.0) 0.1 "orange" 0.0
	    TANGO:SPLINE_IMAGE_CLASS #C(0.0 0.0) #C(+0.10 +0.10) #C(+0.10 -0.10) #C(-0.10 +0.10) #C(+0.05 -0.10) #C(-0.10 +0.10) #C(+0.10 +0.10) #C(-0.10 -0.10) "red" 0.0 1.0
	    TANGO:POLYGON_IMAGE_CLASS #C(0.2 0.2) #C(+0.10 +0.10) #C(-0.10 +0.05) #C(-0.10 +0.10) #C(+0.10 -0.10) #C(+0.10 -0.10) #C(+0.10 +0.10) "green" 1.0
	    TANGO:TEXT_IMAGE_CLASS #C(0.0 0.0) "This is text this is text!" "black" NIL
	    )
      )
;(send comp_image :tx_delete :perform)
(send comp_image :tx_move :perform #C(0.1 0.1) #C(0.1 -0.1) #C(-0.1 .1) #C(-0.1 -0.1))
(send comp_image :tx_resize :perform #C(0.1 0.1))
(send comp_image :tx_resize1 :perform #C(0.1 0.1))

;; the door example given in xtangodoc.doc
(setq door_image
      (send TANGO:COMPOSITE_IMAGE_CLASS :new :show tango_w #C(0.25 0.25)
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.00 0.00) #C(0.100 0.20) TANGO_COLOR_BLACK 0.0
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.01) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.11) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
	    TANGO:CIRCLE_IMAGE_CLASS    #C(0.09 0.10) 0.005          TANGO_COLOR_BLACK 0.0
	    ))

(send door_image :tx_move :perform #C(0.1 0.1))
(send door_image :tx_resize :perform #C(0.01 0.01))
(send door_image :tx_resize :perform (- #C(0.01 0.01)))
(send door_image :tx_raise :perform)
(send door_image :tx_lower :perform)
(send door_image :tx_raise :perform)
(send door_image :tap_color :perform "red")
(send door_image :tap_fill :perform)
(send door_image :tx_delete :perform)


(setq door_image
      (send TANGO:COMPOSITE_IMAGE_CLASS :new :show tango_w #C(0.25 0.25)
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.00 0.00) #C(0.100 0.20) TANGO_COLOR_BLACK 0.0
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.01) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.11) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
	    TANGO:CIRCLE_IMAGE_CLASS    #C(0.09 0.10) 0.005          TANGO_COLOR_BLACK 0.0
	    ))

(send door_image :tap_fill :perform)
(send door_image :tx_fill :perform #C(-0.1 -0.1))
(send door_image :tx_fill :perform #C(0.005 0.05))
(dotimes (z 3)
	 (send door_image :tx_fill :perform
	       (let ((v (make-array 20)))
		 (dotimes (i (length v) v)
			  (setf (aref v i) #C(0.05 0.0)))))
	 (send door_image :tx_fill :perform
	       (let ((v (make-array 20)))
		 (dotimes (i (length v) v)
			  (setf (aref v i) #C(-0.05 0.0)))))
	 )
	 
(send door_image :tap_vis_toggle :perform)

(send door_image :tx_visible :perform 2)
