; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         test-trans.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/test-trans.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Test/demo of Xtango transitions on various image classes.
;		It is probably best to interactively evaluate statements in
;		this file, but it can also be somewhat entertaining if you
;		just load the file.
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 04:16:30 1994 (Niels Mayer) npm@indeed
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

(require "xtango/cls-image")		;define methods on all tango-image classes allowing movement/resizing, etc.
(require "xtango/cls-widget")		;define XTANGO-WIDGET-CLASS, XTANGO-BUTTON-WIDGET-CLASS

(let ()

  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	      "test-trans"
	      :XMN_TITLE		"WINTERP: Xtango Transition Test"
	      :XMN_ICON_NAME		"W:test-trans"
	      ))

  (setq tango_w
	(send XTANGO-WIDGET-CLASS :new :managed
	      "tango" top_w
	      :XMN_HEIGHT 300
	      :XMN_WIDTH 300
	      ))

  (progn
    (send top_w :realize)
    (send tango_w :forced_expose_update) ;wait until exposed to ensure windows created for :begin_drawing call
    (send tango_w :begin_drawing)	;must call this after :realize
    )

  (setq bitm_image
	(send TANGO:BITMAP_IMAGE_CLASS :new :show tango_w
	      #C(0.0 0.0)		;location_coord
	      #(			;bitmap array
		#(
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  #(1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		  ))
	      ))

  (setq circ_image
	(send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w
	      #C(0.25 0.10)		;location_coord
	      0.1			;radius_float
	      "SteelBlue"		;tango_color
	      1.0			;fill_float
	      ))


  ;; the door example given in xtangodoc.doc
  (setq comp_image
	(send TANGO:COMPOSITE_IMAGE_CLASS :new :show tango_w
	      #C(0.25 0.25)
	      TANGO:RECTANGLE_IMAGE_CLASS #C(0.00 0.00) #C(0.100 0.20) TANGO_COLOR_BLACK 0.0
	      TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.01) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
	      TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.11) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
	      TANGO:CIRCLE_IMAGE_CLASS    #C(0.09 0.10) 0.005 TANGO_COLOR_BLACK 0.0
	      ))

  (setq elli_image
	(send TANGO:ELLIPSE_IMAGE_CLASS :new :show tango_w
	      #C(0.50 0.10)		;location_coord
	      #C(0.15 0.1)		;radius_size
	      "Green"			;tango_color
	      1.0			;fill_float
	      ))

  (setq line_image
	(send TANGO:LINE_IMAGE_CLASS :new :show tango_w
	      #C(0.75 0.1)		;location_coord
	      #C(0.2 0.0)		;size_coord
	      "Blue"			;tango_color
	      0.5			;width_float
	      1.0			;style_float
	      :forw_arrow		;arrow_int
	      ))

  (setq polyg_image
	(send TANGO:POLYGON_IMAGE_CLASS :new :show tango_w
	      #C(0.25 0.50)		;location_coord
	      #C(+0.10 +0.10)
	      #C(+0.10 -0.10)
	      #C(-0.10 +0.10)
	      #C(+0.05 -0.10)
	      #C(-0.10 +0.10)
	      #C(+0.10 +0.10)
	      #C(-0.10 -0.10)
	      "Cyan"			;tango_color
	      1.0			;fill_float
	      ))

  (setq polyl_image
	(send TANGO:POLYLINE_IMAGE_CLASS :new :show tango_w
	      #C(0.50 0.50)
	      #C(+0.10 +0.10)
	      #C(+0.10 -0.10)
	      #C(-0.10 +0.10)
	      #C(+0.05 -0.10)
	      #C(-0.10 +0.10)
	      #C(+0.10 +0.10)
	      #C(-0.10 -0.10)
	      "red"			;tango_color
	      0.0			;width_float
	      1.0			;line_style
	      :no_arrow			;arrow_type
	      ))

  (setq rect_image
	(send TANGO:RECTANGLE_IMAGE_CLASS :new :show tango_w
	      #C(0.75 0.50)		;location_coord
	      #C(0.2 0.1)		;size_coord
	      "IndianRed"		;tango_color
	      1.0			;fill_float
	      ))

  (setq spli_image 
	(send TANGO:SPLINE_IMAGE_CLASS :new :show tango_w
	      #C(0.25 0.75)		;location_coord
	      #C(+0.10 +0.10)
	      #C(+0.10 -0.10)
	      #C(-0.10 +0.10)
	      #C(+0.05 -0.10)
	      #C(-0.10 +0.10)
	      #C(+0.10 +0.10)
	      #C(-0.10 -0.10)
	      "Aquamarine"		;tango_color
	      0.0			;width_float
	      1.0			;line_style
	      ))

  (setq text_image
	(send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
	      #C(0.40 0.75)
	      "IMAGINE VISUALIZATION!"	;text_string
	      "MidnightBlue"		;tango_color
	      NIL			;default font
	      ))

  ;; optional...
  (setq expose_callback 
	(send tango_w :add_callback :xmn_expose_callback
	      '(CALLBACK_WIDGET)
	      '(
		(send CALLBACK_WIDGET :refresh)
		)))

  (send tango_w :add_event_handler BUTTON_PRESS_MASK
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
  )

(send tango_w :zoom :out)
(send tango_w :zoom :out)
(send tango_w :zoom :out)
(send tango_w :zoom :out)
(send tango_w :pan :left)
(send tango_w :pan :left)

(send tango_w :set_delay 100)

(setq path
      (TANGO:PATH_CREATE
       #(-0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01)
       #( 0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01)
       ))


(send bitm_image :TX_VISIBLE :perform 2)
(send bitm_image :TX_VISIBLE :perform path)
(send circ_image :TX_VISIBLE :perform path)
(send comp_image :TX_VISIBLE :perform path)
(send elli_image :TX_VISIBLE :perform path)
(send line_image :TX_VISIBLE :perform path)
(send polyg_image :TX_VISIBLE :perform path)
(send polyl_image :TX_VISIBLE :perform path)
(send rect_image :TX_VISIBLE :perform path)
(send spli_image :TX_VISIBLE :perform path)
(send text_image :TX_VISIBLE :perform path)

;; (send bitm_image :tx_fill :perform path)
(send circ_image :tx_fill :perform path)
(send comp_image :tx_fill :perform path)
(send elli_image :tx_fill :perform path)
(send line_image :tx_fill :perform path)
(send polyg_image :tx_fill :perform path)
(send polyl_image :tx_fill :perform path)
(send rect_image :tx_fill :perform path)
(send spli_image :tx_fill :perform path)
(send spli_image :tx_fill :perform 
      (let ((v (make-array 20)))
	(dotimes (i (length v) v)
		 (setf (aref v i) #C(0.05 0.0)))))
(send spli_image :tx_fill :perform 
      (let ((v (make-array 20)))
	(dotimes (i (length v) v)
		 (setf (aref v i) #C(-0.05 0.0)))))
;; (send text_image :tx_fill :perform path)


(send bitm_image :tx_move :perform
      #(
	#C(0.1 0.1)
	#C(0.1 0.1)
	#C(0.1 0.1)
	#C(0.1 0.1)
	#C(0.1 0.1)
	))


(send bitm_image :tx_move :perform path)
(send circ_image :tx_move :perform path)
(send comp_image :tx_move :perform path)
(send elli_image :tx_move :perform path)
(send line_image :tx_move :perform path)
(send polyg_image :tx_move :perform path)
(send polyl_image :tx_move :perform path)
(send rect_image :tx_move :perform path)
(send spli_image :tx_move :perform path)
(send text_image :tx_move :perform path)

(send circ_image :tx_resize :perform #C(+.01 0))
(send circ_image :tx_resize :perform (- #C(+.01 0)))
(send elli_image :tx_resize :perform #C(+.01 +.01))
(send elli_image :tx_resize :perform (- #C(+.01 +.01)))
(send rect_image :tx_resize :perform 
      (vector #C(0.01 0.1)
	      #C(0.01 0.1)
	      #C(0.01 0.1)
	      #C(0.01 0.1)
	      (- #C(0.01 0.1))
	      (- #C(0.01 0.1))
	      (- #C(0.01 0.1))
	      (- #C(0.01 0.1))
	      ))
(send elli_image :tx_resize :perform
      (vector #C(0.1 0.1)
	      #C(0.1 0.1)
	      #C(0.1 0.1)
	      #C(0.1 0.1)
	      (- #C(0.1 0.1))
	      (- #C(0.1 0.1))
	      (- #C(0.1 0.1))
	      (- #C(0.1 0.1))
	      ))


;;; (send bitm_image :TX_RESIZE :perform path)
(send circ_image :TX_RESIZE :perform path)
(send comp_image :TX_RESIZE :perform path)
(send elli_image :TX_RESIZE :perform path)
(send line_image :TX_RESIZE :perform path)
;;; (send polyg_image :TX_RESIZE :perform path)
;;; (send polyl_image :TX_RESIZE :perform path)
(send rect_image :TX_RESIZE :perform path)
;;; (send spli_image :TX_RESIZE :perform path)
;;; (send text_image :TX_RESIZE :perform path)


;;; (send bitm_image :tx_resize1 :perform path)
;;; (send circ_image :tx_resize1 :perform path)
;;; (send comp_image :tx_resize1 :perform path)
;;; (send elli_image :tx_resize1 :perform path)
;;; (send line_image :tx_resize1 :perform path)
(send polyg_image :tx_resize1 :perform path)
(send polyl_image :tx_resize1 :perform path)
;;; (send rect_image :tx_resize1 :perform path)
(send spli_image :tx_resize1 :perform path)
;;; (send text_image :tx_resize1 :perform path)


;;; (send bitm_image :tx_resize2 :perform path)
;;; (send circ_image :tx_resize2 :perform path)
;;; (send comp_image :tx_resize2 :perform path)
;;; (send elli_image :tx_resize2 :perform path)
;;; (send line_image :tx_resize2 :perform path)
(send polyg_image :tx_resize2 :perform path)
(send polyl_image :tx_resize2 :perform path)
;;; (send rect_image :tx_resize2 :perform path)
(send spli_image :tx_resize2 :perform path)
;;; (send text_image :tx_resize2 :perform path)


;;; (send bitm_image :tx_resize3 :perform path)
;;; (send circ_image :tx_resize3 :perform path)
;;; (send comp_image :tx_resize3 :perform path)
;;; (send elli_image :tx_resize3 :perform path)
;;; (send line_image :tx_resize3 :perform path)
(send polyg_image :tx_resize3 :perform path)
(send polyl_image :tx_resize3 :perform path)
;;; (send rect_image :tx_resize3 :perform path)
(send spli_image :tx_resize3 :perform path)
;;; (send text_image :tx_resize3 :perform path)


;;; (send bitm_image :tx_resize4 :perform path)
;;; (send circ_image :tx_resize4 :perform path)
;;; (send comp_image :tx_resize4 :perform path)
;;; (send elli_image :tx_resize4 :perform path)
;;; (send line_image :tx_resize4 :perform path)
(send polyg_image :tx_resize4 :perform path)
(send polyl_image :tx_resize4 :perform path)
;;; (send rect_image :tx_resize4 :perform path)
(send spli_image :tx_resize4 :perform path)
;;; (send text_image :tx_resize4 :perform path)


;;; (send bitm_image :tx_resize5 :perform path)
;;; (send circ_image :tx_resize5 :perform path)
;;; (send comp_image :tx_resize5 :perform path)
;;; (send elli_image :tx_resize5 :perform path)
;;; (send line_image :tx_resize5 :perform path)
(send polyg_image :tx_resize5 :perform path)
(send polyl_image :tx_resize5 :perform path)
;;; (send rect_image :tx_resize5 :perform path)
(send spli_image :tx_resize5 :perform path)
;;; (send text_image :tx_resize5 :perform path)


;;; (send bitm_image :tx_resize6 :perform path)
;;; (send circ_image :tx_resize6 :perform path)
;;; (send comp_image :tx_resize6 :perform path)
;;; (send elli_image :tx_resize6 :perform path)
;;; (send line_image :tx_resize6 :perform path)
(send polyg_image :tx_resize6 :perform path)
(send polyl_image :tx_resize6 :perform path)
;;; (send rect_image :tx_resize6 :perform path)
(send spli_image :tx_resize6 :perform path)
;;; (send text_image :tx_resize6 :perform path)


;;; (send bitm_image :tx_resize7 :perform path)
;;; (send circ_image :tx_resize7 :perform path)
;;; (send comp_image :tx_resize7 :perform path)
;;; (send elli_image :tx_resize7 :perform path)
;;; (send line_image :tx_resize7 :perform path)
(send polyg_image :tx_resize7 :perform path)
(send polyl_image :tx_resize7 :perform path)
;;; (send rect_image :tx_resize7 :perform path)
(send spli_image :tx_resize7 :perform path)
;;; (send text_image :tx_resize7 :perform path)


(let (
      (pa
       (tango:path_create (+ #C(0.01 0.01)) 
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))))
      (pa1
       (tango:path_create (- #C(0.01 0.01)) 
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01)))))
  (dotimes (i 3)
    (send polyg_image :tx_resize1 :perform pa)
    (send polyg_image :tx_resize2 :perform pa)
    (send polyg_image :tx_resize3 :perform pa)
    (send polyg_image :tx_resize4 :perform pa)
    (send polyg_image :tx_resize5 :perform pa)
    (send polyg_image :tx_resize6 :perform pa)
    (send polyg_image :tx_resize7 :perform pa)

    (send polyg_image :tx_resize1 :perform pa1)
    (send polyg_image :tx_resize2 :perform pa1)
    (send polyg_image :tx_resize3 :perform pa1)
    (send polyg_image :tx_resize4 :perform pa1)
    (send polyg_image :tx_resize5 :perform pa1)
    (send polyg_image :tx_resize6 :perform pa1)
    (send polyg_image :tx_resize7 :perform pa1)
    ))

(let (
      (pa
       (tango:path_create (+ #C(0.01 0.01)) 
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))))
      (pa1
       (tango:path_create (- #C(0.01 0.01)) 
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01)))))
  (dotimes (i 3)
    (send polyl_image :tx_resize1 :perform pa)
    (send polyl_image :tx_resize2 :perform pa)
    (send polyl_image :tx_resize3 :perform pa)
    (send polyl_image :tx_resize4 :perform pa)
    (send polyl_image :tx_resize5 :perform pa)
    (send polyl_image :tx_resize6 :perform pa)
    (send polyl_image :tx_resize7 :perform pa)

    (send polyl_image :tx_resize1 :perform pa1)
    (send polyl_image :tx_resize2 :perform pa1)
    (send polyl_image :tx_resize3 :perform pa1)
    (send polyl_image :tx_resize4 :perform pa1)
    (send polyl_image :tx_resize5 :perform pa1)
    (send polyl_image :tx_resize6 :perform pa1)
    (send polyl_image :tx_resize7 :perform pa1)
    ))

(let (
      (pa
       (tango:path_create (+ #C(0.01 0.01)) 
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))
			  (+ #C(0.01 0.01))))
      (pa1
       (tango:path_create (- #C(0.01 0.01)) 
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01))
			  (- #C(0.01 0.01)))))
  (dotimes (i 3)
    (send spli_image :tx_resize1 :perform pa)
    (send spli_image :tx_resize2 :perform pa)
    (send spli_image :tx_resize3 :perform pa)
    (send spli_image :tx_resize4 :perform pa)
    (send spli_image :tx_resize5 :perform pa)
    (send spli_image :tx_resize6 :perform pa)
    (send spli_image :tx_resize7 :perform pa)

    (send spli_image :tx_resize1 :perform pa1)
    (send spli_image :tx_resize2 :perform pa1)
    (send spli_image :tx_resize3 :perform pa1)
    (send spli_image :tx_resize4 :perform pa1)
    (send spli_image :tx_resize5 :perform pa1)
    (send spli_image :tx_resize6 :perform pa1)
    (send spli_image :tx_resize7 :perform pa1)
    ))



;;; (send bitm_image :tx_grab1 :perform path)
;;; (send circ_image :tx_grab1 :perform path)
;;; (send comp_image :tx_grab1 :perform path)
;;; (send elli_image :tx_grab1 :perform path)
;;; (send line_image :tx_grab1 :perform path)
(send polyg_image :tx_grab1 :perform path)
(send polyl_image :tx_grab1 :perform path)
;;; (send rect_image :tx_grab1 :perform path)
(send spli_image :tx_grab1 :perform path)
;;; (send text_image :tx_grab1 :perform path)


;;; (send bitm_image :tx_grab2 :perform path)
;;; (send circ_image :tx_grab2 :perform path)
;;; (send comp_image :tx_grab2 :perform path)
;;; (send elli_image :tx_grab2 :perform path)
;;; (send line_image :tx_grab2 :perform path)
(send polyg_image :tx_grab2 :perform path)
(send polyl_image :tx_grab2 :perform path)
;;; (send rect_image :tx_grab2 :perform path)
(send spli_image :tx_grab2 :perform path)
;;; (send text_image :tx_grab2 :perform path)


;;; (send bitm_image :tx_grab3 :perform path)
;;; (send circ_image :tx_grab3 :perform path)
;;; (send comp_image :tx_grab3 :perform path)
;;; (send elli_image :tx_grab3 :perform path)
;;; (send line_image :tx_grab3 :perform path)
(send polyg_image :tx_grab3 :perform path)
(send polyl_image :tx_grab3 :perform path)
;;; (send rect_image :tx_grab3 :perform path)
(send spli_image :tx_grab3 :perform path)
;;; (send text_image :tx_grab3 :perform path)


;;; (send bitm_image :tx_grab4 :perform path)
;;; (send circ_image :tx_grab4 :perform path)
;;; (send comp_image :tx_grab4 :perform path)
;;; (send elli_image :tx_grab4 :perform path)
;;; (send line_image :tx_grab4 :perform path)
(send polyg_image :tx_grab4 :perform path)
(send polyl_image :tx_grab4 :perform path)
;;; (send rect_image :tx_grab4 :perform path)
(send spli_image :tx_grab4 :perform path)
;;; (send text_image :tx_grab4 :perform path)


;;; (send bitm_image :tx_grab5 :perform path)
;;; (send circ_image :tx_grab5 :perform path)
;;; (send comp_image :tx_grab5 :perform path)
;;; (send elli_image :tx_grab5 :perform path)
;;; (send line_image :tx_grab5 :perform path)
(send polyg_image :tx_grab5 :perform path)
(send polyl_image :tx_grab5 :perform path)
;;; (send rect_image :tx_grab5 :perform path)
(send spli_image :tx_grab5 :perform path)
;;; (send text_image :tx_grab5 :perform path)


;;; (send bitm_image :tx_grab6 :perform path)
;;; (send circ_image :tx_grab6 :perform path)
;;; (send comp_image :tx_grab6 :perform path)
;;; (send elli_image :tx_grab6 :perform path)
;;; (send line_image :tx_grab6 :perform path)
(send polyg_image :tx_grab6 :perform path)
(send polyl_image :tx_grab6 :perform path)
;;; (send rect_image :tx_grab6 :perform path)
(send spli_image :tx_grab6 :perform path)
;;; (send text_image :tx_grab6 :perform path)


;;; (send bitm_image :tx_grab7 :perform path)
;;; (send circ_image :tx_grab7 :perform path)
;;; (send comp_image :tx_grab7 :perform path)
;;; (send elli_image :tx_grab7 :perform path)
;;; (send line_image :tx_grab7 :perform path)
(send polyg_image :tx_grab7 :perform path)
(send polyl_image :tx_grab7 :perform path)
;;; (send rect_image :tx_grab7 :perform path)
(send spli_image :tx_grab7 :perform path)
;;; (send text_image :tx_grab7 :perform path)


(let (
      (pa
       (tango:path_create (+ #C(0.05 0.05)) 
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))))
      (pa1
       (tango:path_create (- #C(0.05 0.05)) 
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05)))))
  (dotimes (i 3)
    (send polyg_image :tx_grab1 :perform pa)
    (send polyg_image :tx_grab2 :perform pa)
    (send polyg_image :tx_grab3 :perform pa)
    (send polyg_image :tx_grab4 :perform pa)
    (send polyg_image :tx_grab5 :perform pa)
    (send polyg_image :tx_grab6 :perform pa)
    (send polyg_image :tx_grab7 :perform pa)

    (send polyg_image :tx_grab1 :perform pa1)
    (send polyg_image :tx_grab2 :perform pa1)
    (send polyg_image :tx_grab3 :perform pa1)
    (send polyg_image :tx_grab4 :perform pa1)
    (send polyg_image :tx_grab5 :perform pa1)
    (send polyg_image :tx_grab6 :perform pa1)
    (send polyg_image :tx_grab7 :perform pa1)
    ))


(let (
      (pa
       (tango:path_create (+ #C(0.05 0.05)) 
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))))
      (pa1
       (tango:path_create (- #C(0.05 0.05)) 
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05)))))
  (dotimes (i 3)
    (send polyl_image :tx_grab1 :perform pa)
    (send polyl_image :tx_grab2 :perform pa)
    (send polyl_image :tx_grab3 :perform pa)
    (send polyl_image :tx_grab4 :perform pa)
    (send polyl_image :tx_grab5 :perform pa)
    (send polyl_image :tx_grab6 :perform pa)
    (send polyl_image :tx_grab7 :perform pa)

    (send polyl_image :tx_grab1 :perform pa1)
    (send polyl_image :tx_grab2 :perform pa1)
    (send polyl_image :tx_grab3 :perform pa1)
    (send polyl_image :tx_grab4 :perform pa1)
    (send polyl_image :tx_grab5 :perform pa1)
    (send polyl_image :tx_grab6 :perform pa1)
    (send polyl_image :tx_grab7 :perform pa1)
    ))

(let (
      (pa
       (tango:path_create (+ #C(0.05 0.05)) 
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))
			  (+ #C(0.05 0.05))))
      (pa1
       (tango:path_create (- #C(0.05 0.05)) 
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05))
			  (- #C(0.05 0.05)))))
  (dotimes (i 3)
    (send spli_image :tx_grab1 :perform pa)
    (send spli_image :tx_grab2 :perform pa)
    (send spli_image :tx_grab3 :perform pa)
    (send spli_image :tx_grab4 :perform pa)
    (send spli_image :tx_grab5 :perform pa)
    (send spli_image :tx_grab6 :perform pa)
    (send spli_image :tx_grab7 :perform pa)

    (send spli_image :tx_grab1 :perform pa1)
    (send spli_image :tx_grab2 :perform pa1)
    (send spli_image :tx_grab3 :perform pa1)
    (send spli_image :tx_grab4 :perform pa1)
    (send spli_image :tx_grab5 :perform pa1)
    (send spli_image :tx_grab6 :perform pa1)
    (send spli_image :tx_grab7 :perform pa1)
    ))


;;;(send bitm_image :tx_color :perform (tango:path_color TANGO_COLOR_BLACK))
(send circ_image :tx_color :perform (tango:path_color TANGO_COLOR_BLACK))
(send comp_image :tx_color :perform (tango:path_color TANGO_COLOR_BLACK))
(send elli_image :tx_color :perform (tango:path_color TANGO_COLOR_MAROON))
(send line_image :tx_color :perform (tango:path_color TANGO_COLOR_YELLOW))
(send polyg_image :tx_color :perform (tango:path_color TANGO_COLOR_RED))
(send polyl_image :tx_color :perform (tango:path_color TANGO_COLOR_BLUE))
(send rect_image :tx_color :perform (tango:path_color TANGO_COLOR_GREEN))
(send spli_image :tx_color :perform (tango:path_color TANGO_COLOR_RED))
(send text_image :tx_color :perform (tango:path_color TANGO_COLOR_GREEN))


(send bitm_image :tx_raise :perform)
(send bitm_image :tx_raise :perform path)
(send circ_image :tx_raise :perform path)
(send comp_image :tx_raise :perform path)
(send elli_image :tx_raise :perform path)
(send line_image :tx_raise :perform path)
(send polyg_image :tx_raise :perform path)
(send polyl_image :tx_raise :perform path)
(send rect_image :tx_raise :perform path)
(send spli_image :tx_raise :perform path)
(send text_image :tx_raise :perform path)

(send bitm_image :tx_lower :perform path)
(send circ_image :tx_lower :perform path)
(send comp_image :tx_lower :perform path)
(send elli_image :tx_lower :perform path)
(send line_image :tx_lower :perform path)
(send polyg_image :tx_lower :perform path)
(send polyl_image :tx_lower :perform path)
(send rect_image :tx_lower :perform path)
(send spli_image :tx_lower :perform path)
(send text_image :tx_lower :perform path)

(send bitm_image :tx_delay :perform path)
(send circ_image :tx_delay :perform path)
(send comp_image :tx_delay :perform path)
(send elli_image :tx_delay :perform path)
(send line_image :tx_delay :perform path)
(send polyg_image :tx_delay :perform path)
(send polyl_image :tx_delay :perform path)
(send rect_image :tx_delay :perform path)
(send spli_image :tx_delay :perform path)
(send text_image :tx_delay :perform path)

(send bitm_image :tx_refresh :perform path)
(send circ_image :tx_refresh :perform path)
(send comp_image :tx_refresh :perform path)
(send elli_image :tx_refresh :perform path)
(send line_image :tx_refresh :perform path)
(send polyg_image :tx_refresh :perform path)
(send polyl_image :tx_refresh :perform path)
(send rect_image :tx_refresh :perform path)
(send spli_image :tx_refresh :perform path)
(send text_image :tx_refresh :perform path)

(send bitm_image :tx_shuffle :perform path)
;;; (send circ_image :tx_shuffle :perform path)
;;; (send comp_image :tx_shuffle :perform path)
;;; (send elli_image :tx_shuffle :perform path)
;;; (send line_image :tx_shuffle :perform path)
;;; (send polyg_image :tx_shuffle :perform path)
;;; (send polyl_image :tx_shuffle :perform path)
;;; (send rect_image :tx_shuffle :perform path)
;;; (send spli_image :tx_shuffle :perform path)
;;; (send text_image :tx_shuffle :perform path)



(send bitm_image :tx_zoom :perform path)
(send circ_image :tx_zoom :perform path)
(send comp_image :tx_zoom :perform path)
(send elli_image :tx_zoom :perform path)
(send line_image :tx_zoom :perform path)
(send polyg_image :tx_zoom :perform path)
(send polyl_image :tx_zoom :perform path)
(send rect_image :tx_zoom :perform path)
(send spli_image :tx_zoom :perform path)
(send text_image :tx_zoom :perform path)

(send tango_w :SET_COORD 0.0 1.0 1.0 0.0)

(send bitm_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send bitm_image :tx_zoom :perform (- #C(0.1 0.1)))
(send circ_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send circ_image :tx_zoom :perform (- #C(0.1 0.1)))
(send comp_image :tx_zoom :perform #C(0.1 0.1))
(send elli_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send elli_image :tx_zoom :perform (- #C(0.1 0.1)))
(send line_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send line_image :tx_zoom :perform (- #C(0.1 0.1)))
(send polyg_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send polyg_image :tx_zoom :perform (- #C(0.1 0.1)))
(send polyl_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send polyl_image :tx_zoom :perform (- #C(0.1 0.1)))
(send rect_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send rect_image :tx_zoom :perform (- #C(0.1 0.1)))
(send spli_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send spli_image :tx_zoom :perform (- #C(0.1 0.1)))
(send text_image :tx_zoom :perform (+ #C(0.1 0.1)))
(send text_image :tx_zoom :perform (- #C(0.1 0.1)))

(send tango_w :SET_COORD 0.0 1.0 1.0 0.0)

(send polyl_image :tx_zoom :perform #C(0.1 0.1))
(send polyl_image :tx_zoom :perform (- #C(0.2 0.2)))
(send polyl_image :tx_zoom :perform #C(0.2 0.2))

(defun random-complex ()
  (complex (/ (- (random 1001) 500) 5000.0) (/ (- (random 1001) 500) 5000.0))
  )

(defun random-move ()
  (complex (/ (- (random 1001) 500) 20000.0) (/ (- (random 1001) 500) 20000.0))
  )

;; (defun random-complex-seq ()
;;   (let* ((seq (make-array 20))
;; 	 (hlen (/ (length seq) 2)))
;; 
;;     (dotimes (i hlen)
;; 	     (setf (aref seq i) (random-move)))
;;     (dotimes (i hlen)
;; 	     (setf (aref seq (+ i hlen)) (- (aref seq i))))
;;     seq
;;     ))

(defun random-complex-seq ()
  (let* ((seq (make-array 20))
	 (len (length seq)))

    (dotimes (i len)
	     (setf (aref seq i) (random-move)))
    seq
    ))

(random-complex-seq)

(defun random-color-seq ()
  (tango:path_concatenate		;want a path length of 20 to correspond to :tap_move length
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8))
   (tango:path_color (random 8)))
  )


(tango:tx_compose :perform
		  (send bitm_image :tap_move :ctr #C(0.5 0.5))
		  (send circ_image :tap_move :ctr #C(0.5 0.5))
		  (send comp_image :tap_move :ctr #C(0.5 0.5))
		  (send elli_image :tap_move :ctr #C(0.5 0.5))
		  (send line_image :tap_move :ctr #C(0.5 0.5))
		  (send polyg_image :tap_move :ctr #C(0.5 0.5))
		  (send polyl_image :tap_move :ctr #C(0.5 0.5))
		  (send rect_image :tap_move :ctr #C(0.5 0.5))
		  (send spli_image :tap_move :ctr #C(0.5 0.5))
		  (send text_image :tap_move :ctr #C(0.5 0.5))
		  )

(tango:tx_compose :perform
		  (send bitm_image :tx_move (random-complex-seq))
		  (send circ_image :tx_move (random-complex-seq))
		  (send circ_image :tx_resize (random-complex-seq))
		  (send circ_image :tx_fill (random-complex-seq))
		  (send circ_image :tx_color (random-color-seq))
		  (tango:tx_concatenate (send circ_image :tx_delay (1+ (random 20)))
					(send circ_image :tx_lower))
		  (send comp_image :tx_move (random-complex-seq))
		  (send elli_image :tx_move (random-complex-seq))
		  (send elli_image :tx_resize (random-complex-seq))
		  (send elli_image :tx_fill (random-complex-seq))
		  (send elli_image :tx_color (random-color-seq))
		  (tango:tx_concatenate (send elli_image :tx_delay (1+ (random 20)))
					(send elli_image :tx_lower))
		  (send line_image :tx_move (random-complex-seq))
		  (send line_image :tx_resize (random-complex-seq))
		  (send line_image :tx_color (random-color-seq))
		  (tango:tx_concatenate (send line_image :tx_delay (1+ (random 20)))
					(send line_image :tx_lower))
		  (send rect_image :tx_move (random-complex-seq))
		  (send rect_image :tx_resize (random-complex-seq))
		  (send rect_image :tx_color (random-color-seq))
		  (tango:tx_concatenate (send rect_image :tx_delay (1+ (random 20)))
					(send rect_image :tx_lower))

		  (send text_image :tx_move (random-complex-seq))
		  (send text_image :tx_color (random-color-seq))
		  (tango:tx_concatenate (send text_image :tx_delay (1+ (random 20)))
					(send text_image :tx_lower))


		  )

(let ((i 0))
  (xt_add_timeout
   0
   '(
     (let ((trans
	    (tango:tx_compose 
	     (send polyg_image :tx_move (random-complex-seq))
	     (send polyg_image :tx_grab1 (random-complex-seq))
	     (send polyg_image :tx_grab2 (random-complex-seq))
	     (send polyg_image :tx_grab3 (random-complex-seq))
	     (send polyg_image :tx_grab4 (random-complex-seq))
	     (send polyg_image :tx_grab5 (random-complex-seq))
	     (send polyg_image :tx_grab6 (random-complex-seq))
	     (send polyg_image :tx_grab7 (random-complex-seq))
	     (send polyg_image :tx_color (random-color-seq))
	     (tango:tx_concatenate (send polyg_image :tx_delay (1+ (random 20)))
				   (send polyg_image :tx_lower))
	     (send polyl_image :tx_move (random-complex-seq))
	     (send polyl_image :tx_grab1 (random-complex-seq))
	     (send polyl_image :tx_grab2 (random-complex-seq))
	     (send polyl_image :tx_grab3 (random-complex-seq))
	     (send polyl_image :tx_grab4 (random-complex-seq))
	     (send polyl_image :tx_grab5 (random-complex-seq))
	     (send polyl_image :tx_grab6 (random-complex-seq))
	     (send polyl_image :tx_grab7 (random-complex-seq))
	     (send polyl_image :tx_color (random-color-seq))
	     (tango:tx_concatenate (send polyl_image :tx_delay (1+ (random 20)))
				   (send polyl_image :tx_lower))
	     (send spli_image :tx_move (random-complex-seq))
	     (send spli_image :tx_grab1 (random-complex-seq))
	     (send spli_image :tx_grab2 (random-complex-seq))
	     (send spli_image :tx_grab3 (random-complex-seq))
	     (send spli_image :tx_grab4 (random-complex-seq))
	     (send spli_image :tx_grab5 (random-complex-seq))
	     (send spli_image :tx_grab6 (random-complex-seq))
	     (send spli_image :tx_grab7 (random-complex-seq))
	     (send spli_image :tx_color (random-color-seq))
	     (tango:tx_concatenate (send spli_image :tx_delay (1+ (random 20)))
				   (send spli_image :tx_lower))
	     )))
       (tango:tx_compose :perform
			 trans
			 (send bitm_image :tx_move (random-complex-seq))
			 (send circ_image :tx_move (random-complex-seq))
			 (send circ_image :tx_resize (random-complex-seq))
			 (send elli_image :tx_move (random-complex-seq))
			 (send elli_image :tx_resize (random-complex-seq))
			 (send line_image :tx_move (random-complex-seq))
			 (send line_image :tx_resize (random-complex-seq))
			 (send polyg_image :tx_move (random-complex-seq))
			 (send polyg_image :tx_grab1 (random-complex-seq))
			 (send polyg_image :tx_grab2 (random-complex-seq))
			 (send polyg_image :tx_grab3 (random-complex-seq))
			 (send polyg_image :tx_grab4 (random-complex-seq))
			 (send polyg_image :tx_grab5 (random-complex-seq))
			 (send polyg_image :tx_grab6 (random-complex-seq))
			 (send polyg_image :tx_grab7 (random-complex-seq))
			 (send polyl_image :tx_move (random-complex-seq))
			 (send polyl_image :tx_grab1 (random-complex-seq))
			 (send polyl_image :tx_grab2 (random-complex-seq))
			 (send polyl_image :tx_grab3 (random-complex-seq))
			 (send polyl_image :tx_grab4 (random-complex-seq))
			 (send polyl_image :tx_grab5 (random-complex-seq))
			 (send polyl_image :tx_grab6 (random-complex-seq))
			 (send polyl_image :tx_grab7 (random-complex-seq))
			 (send rect_image :tx_move (random-complex-seq))
			 (send rect_image :tx_resize (random-complex-seq))
			 (send spli_image :tx_move (random-complex-seq))
			 (send spli_image :tx_grab1 (random-complex-seq))
			 (send spli_image :tx_grab2 (random-complex-seq))
			 (send spli_image :tx_grab3 (random-complex-seq))
			 (send spli_image :tx_grab4 (random-complex-seq))
			 (send spli_image :tx_grab5 (random-complex-seq))
			 (send spli_image :tx_grab6 (random-complex-seq))
			 (send spli_image :tx_grab7 (random-complex-seq))
			 (send text_image :tx_move  (random-complex))
			 )
       (tango:tx_free trans)

       (if (<= i 10)
	   (progn
	     (xt_add_timeout 100 TIMEOUT_OBJ)
	     (setq i (1+ i)))
	 )
       )
     )
   ))

(tango:tx_concatenate :perform
		      (send bitm_image :tx_move (random-complex-seq))
		      (send circ_image :tx_move (random-complex-seq))
		      (send circ_image :tx_resize (random-complex-seq))
;;;    		      (send circ_image :tx_fill (random-complex-seq))
		      (send circ_image :tx_color (random-color-seq))
;;;                   (send comp_image :tx_move (random-complex-seq))
		      (send elli_image :tx_move (random-complex-seq))
		      (send elli_image :tx_resize (random-complex-seq))
;;;		      (send elli_image :tx_fill (random-complex-seq))
		      (send elli_image :tx_color (random-color-seq))
		      (send line_image :tx_move (random-complex-seq))
		      (send line_image :tx_resize (random-complex-seq))
		      (send line_image :tx_color (random-color-seq))
		      (send polyg_image :tx_move (random-complex-seq))
		      (send polyg_image :tx_grab1 (random-complex-seq))
		      (send polyg_image :tx_grab2 (random-complex-seq))
		      (send polyg_image :tx_grab3 (random-complex-seq))
		      (send polyg_image :tx_grab4 (random-complex-seq))
		      (send polyg_image :tx_grab5 (random-complex-seq))
		      (send polyg_image :tx_grab6 (random-complex-seq))
		      (send polyg_image :tx_grab7 (random-complex-seq))
		      (send polyg_image :tx_color (random-color-seq))
		      (send polyl_image :tx_move (random-complex-seq))
		      (send polyl_image :tx_grab1 (random-complex-seq))
		      (send polyl_image :tx_grab2 (random-complex-seq))
		      (send polyl_image :tx_grab3 (random-complex-seq))
		      (send polyl_image :tx_grab4 (random-complex-seq))
		      (send polyl_image :tx_grab5 (random-complex-seq))
		      (send polyl_image :tx_grab6 (random-complex-seq))
		      (send polyl_image :tx_grab7 (random-complex-seq))
		      (send polyl_image :tx_color (random-color-seq))
		      (send rect_image :tx_move (random-complex-seq))
		      (send rect_image :tx_resize (random-complex-seq))
		      (send rect_image :tx_color (random-color-seq))
		      (send spli_image :tx_move (random-complex-seq))
		      (send spli_image :tx_grab1 (random-complex-seq))
		      (send spli_image :tx_grab2 (random-complex-seq))
		      (send spli_image :tx_grab3 (random-complex-seq))
		      (send spli_image :tx_grab4 (random-complex-seq))
		      (send spli_image :tx_grab5 (random-complex-seq))
		      (send spli_image :tx_grab6 (random-complex-seq))
		      (send spli_image :tx_grab7 (random-complex-seq))
		      (send spli_image :tx_color (random-color-seq))
		      (send text_image :tx_move  (random-complex))
		      (send text_image :tx_color (random-color-seq))
		      )

(tango:tx_iterate :perform (send elli_image :tx_resize (random-complex-seq)) 5)
(tango:tx_iterate :perform 
		  (tango:tx_compose 
		   (send polyg_image :tx_grab1 (random-complex-seq))
		   (send polyg_image :tx_grab2 (random-complex-seq))
		   (send polyg_image :tx_grab3 (random-complex-seq))
		   (send polyg_image :tx_grab4 (random-complex-seq))
		   (send polyg_image :tx_grab5 (random-complex-seq))
		   (send polyg_image :tx_grab6 (random-complex-seq))
		   (send polyg_image :tx_grab7 (random-complex-seq))
		   )
		  5)

(tango:tx_iterate :perform 
		  (tango:tx_compose 
		   (send spli_image :tx_grab1 (random-complex-seq))
		   (send spli_image :tx_grab2 (random-complex-seq))
		   (send spli_image :tx_grab3 (random-complex-seq))
		   (send spli_image :tx_grab4 (random-complex-seq))
		   (send spli_image :tx_grab5 (random-complex-seq))
		   (send spli_image :tx_grab6 (random-complex-seq))
		   (send spli_image :tx_grab7 (random-complex-seq))
		   )
		  5)

;;;;;;;;;;;;;;;;;;;;

; (send bitm_image :tx_delete :perform)
; (send circ_image :tx_delete :perform)
; (send comp_image :tx_delete :perform)
; (send elli_image :tx_delete :perform)
; (send line_image :tx_delete :perform)
; (send polyg_image :tx_delete :perform)
; (send polyl_image :tx_delete :perform)
; (send rect_image :tx_delete :perform)
; (send spli_image :tx_delete :perform)
; (send text_image :tx_delete :perform)



