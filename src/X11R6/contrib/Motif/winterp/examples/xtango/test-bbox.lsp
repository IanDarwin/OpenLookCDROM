; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         test-bbox.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/test-bbox.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Test of tango bounding boxes -- bounding boxes visible due to
;		lack of	expose-callback/:refresh, and displaying images
;		individually, with :tap_show, and using non-white background
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 03:57:47 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define *X11-BITMAPS-DIRECTORY*
;;; (require "xtango/cls-widget")	;define XTANGO-WIDGET-CLASS, XTANGO-BUTTON-WIDGET-CLASS
;;; (require "xtango/cls-image")	;define methods on all tango-image classes allowing movement/resizing, etc.

(let ()
(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	    "test-bbox"
	    :XMN_TITLE			"WINTERP: Xtango Bounding Box Test"
	    :XMN_ICON_NAME		"W:test-bbox"
	    ))

(setq tango_w
      (send
       TANGO:WIDGET_CLASS
;;;    XTANGO-WIDGET-CLASS
       :new :managed
       "tango" top_w
       :XMN_HEIGHT 600
       :XMN_WIDTH 600
       ))

(progn					;incase evaling interactively, must evaluate these "atomically" otherwise :forced_expose_update will take over event loop...
  (send top_w :realize)
  (send tango_w :forced_expose_update)
  (send tango_w :begin_drawing)
  )

;;; (send TANGO:COMPOSITE_IMAGE_CLASS :new :show tango_w #C(0.25 0.25)
;;;       TANGO:CIRCLE_IMAGE_CLASS #C(0.25 0.25) 0.2 "green" 1.0
;;;       TANGO:POLYGON_IMAGE_CLASS #C(-0.05 -0.05) #C(0.05 0.05) #C(-0.10 -0.10) #C(-0.10 0.10) #C(0.10 -0.10) "orange" 1.0
;;;       )


(setq bitm_image
      (send TANGO:BITMAP_IMAGE_CLASS :new :show tango_w
 	    #C(0.03 0.03)
 ;;;	    (vector (bitmap-file-to-array (concatenate 'string *X11-BITMAPS-DIRECTORY* "wingdogs") TANGO_COLOR_BLACK TANGO_COLOR_WHITE))

 	    #(				;bitmap array
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

(setq bitm_image
      (send TANGO:BITMAP_IMAGE_CLASS :new :show tango_w
	    #C(0.03 0.23)		;location_coord
;;;	    (vector (bitmap-file-to-array (concatenate 'string *X11-BITMAPS-DIRECTORY* "wingdogs") TANGO_COLOR_BLACK TANGO_COLOR_WHITE))
	    #(				;bitmap array
	      #(
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		#(7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
		))
	    ))

(setq circ_image
      (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w
	    #C(0.25 0.13)		;location_coord
	    #C(0.1 0.0)			;radius_coord
	    "SteelBlue"			;tango_color
	    1.0				;fill_float
	    ))

(setq circ_image
      (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w
	    #C(-0.10 0.60)		;location_coord
	    0.2222			;radius_float
	    "SteelBlue"			;tango_color
	    0.0				;fill_float
	    ))

; (send circ_image :tx_resize :perform #C(0.01 0))
; (send circ_image :tx_move :perform #C(-0.01 0))
; (send circ_image :tx_move :perform #C(+0.01 0))

(setq circ_image
      (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w
	    #C(-0.0732 0.90)		;location_coord
	    0.1458			;radius_float
	    "SteelBlue"			;tango_color
	    0.0				;fill_float
	    ))

(setq comp_image
      (send TANGO:COMPOSITE_IMAGE_CLASS :new :show tango_w #C(0.80 0.25)
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.00 0.00) #C(0.100 0.20) TANGO_COLOR_BLACK 0.0
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.01) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
	    TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.11) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
	    TANGO:CIRCLE_IMAGE_CLASS    #C(0.09 0.10) 0.005	     TANGO_COLOR_BLACK 0.0
	    ))

(setq elli_image
      (send TANGO:ELLIPSE_IMAGE_CLASS :new :show tango_w
	    #C(0.55 0.13)		;location_coord
	    #C(0.15 0.1)		;radius_coord
	    "Green"			;tango_color
	    1.0				;fill_float
	    ))

(setq line_image
      (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.15)		;location_coord
	    #C(0.2 0.0)			;size_coord
	    "Blue"			;tango_color
	    1.0				;width_float
	    1.0				;style_float
	    :both_arrow
	    ))

(setq line_image
      (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.1)		;location_coord
	    #C(0.2 0.0)			;size_coord
	    "Blue"			;tango_color
	    0.0				;width_float
	    1.0				;style_float
	    :forw_arrow			;arrow_int
	    ))

(setq line_image
      (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.05)		;location_coord
	    #C(0.2 0.0)			;size_coord
	    "Blue"			;tango_color
	    1.0				;width_float
	    1.0				;style_float
	    :no_arrow			;arrow_int
	    ))

(setq line_image
      (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.075)		;location_coord
	    #C(0.2 0.0)			;size_coord
	    "Blue"			;tango_color
	    0.0				;width_float
	    1.0				;style_float
	    :no_arrow			;arrow_int
	    ))

(setq line_image
      (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
	    #C(0.77 0.20)		;location_coord
	    #C(0.0 0.1)			;size_coord
	    "Blue"			;tango_color
	    1.0				;width_float
	    1.0				;style_float
	    :no_arrow			;arrow_int
	    ))

(setq line_image
      (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
	    #C(0.72 0.20)		;location_coord
	    #C(0.0 0.1)			;size_coord
	    "Blue"			;tango_color
	    0.5				;width_float
	    1.0				;style_float
	    :no_arrow			;arrow_int
	    ))

(setq line_image
      (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.20)		;location_coord
	    #C(0.0 0.1)			;size_coord
	    "Blue"			;tango_color
	    0.0				;width_float
	    1.0				;style_float
	    :no_arrow			;arrow_int
	    ))

(setq polyg_image
      (send TANGO:POLYGON_IMAGE_CLASS :new :show tango_w
	    #C(0.25 0.50)		;location_coord
	    #C(-0.10 +0.05)
	    #C(-0.05 +0.10)
	    #C(-0.10 -0.10)
	    #C(+0.08 -0.10)
	    #C(-0.10 +0.05)
	    #C(-0.10 +0.02)
	    #C(-0.03 +0.10)
	    "Cyan"			;tango_color
	    1.0				;fill_float
	    ))

(setq polyg_image
      (send TANGO:POLYGON_IMAGE_CLASS :new :show tango_w
	    #C(0.25 0.71)		;location_coord
	    #C(-0.10 +0.05)
	    #C(-0.05 +0.10)
	    #C(-0.10 -0.10)
	    #C(+0.08 -0.10)
	    #C(-0.10 +0.05)
	    #C(-0.10 +0.02)
	    #C(-0.03 +0.10)
	    "Cyan"			;tango_color
	    1.0				;fill_float
	    ))

(setq polyl_image
      (send TANGO:POLYLINE_IMAGE_CLASS :new :show tango_w
	    #C(0.50 0.50)		;location_coord
	    #C(-0.10 -0.10) #C(+0.05 -0.10) #C(-0.10 +0.05) #C(+0.02 -0.05)
	    #C(-0.10 +0.10) #C(-0.10 -0.03) #C(+0.08 +0.10)
	    "red"			;tango_color
	    1.0				;width_float
	    1.0				;line_style
	    :forw_arrow			;arrow_type
	    ))

(setq polyl_image
      (send TANGO:POLYLINE_IMAGE_CLASS :new :show tango_w
	    #C(0.50 0.80)		;location_coord
	    #C(+0.10 +0.10) #C(+0.10 -0.05) #C(-0.05 +0.10) #C(+0.05 -0.02)
	    #C(-0.10 +0.10) #C(+0.03 +0.10) #C(-0.10 -0.08)
	    "red"			;tango_color
	    1.0				;width_float
	    1.0				;line_style
	    :forw_arrow			;arrow_type
	    ))

(setq rect_image
      (send TANGO:RECTANGLE_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.50)		;location_coord
	    #C(0.2 0.13)		;size_coord
	    "IndianRed"			;tango_color
	    0.0				;fill_float
	    ))

(setq rect_image
      (send TANGO:RECTANGLE_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.65)		;location_coord
	    #C(0.2 0.13)		;size_coord
	    "IndianRed"			;tango_color
	    1.0				;fill_float
	    ))

(setq spli_image 
      (send TANGO:SPLINE_IMAGE_CLASS :new :show tango_w
	    #C(0.25 0.93)		;location_coord
	    #C(+0.10 +0.10) #C(+0.10 -0.10) #C(-0.10 +0.10) #C(+0.05 -0.10)
	    #C(-0.10 +0.10) #C(+0.10 +0.10) #C(-0.10 -0.10)
	    "Aquamarine"		;tango_color
	    1.0				;width_float
	    1.0				;line_style
	    ))

(setq text_image
      (send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
	    #C(0.15 0.35)		;location_coord
	    "IMAGINE VISUALIZATION!"	;text_string
	    "MidnightBlue"		;tango_color
	    "-*-*-bold-r-*--18-*-*-*-p-*-iso8859-1"
	    ))

(setq text_image
      (send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
	    #C(0.90 0.95) :ctr		;location_coord
	    "Ugly yeti"			;text_string
	    "MidnightBlue"		;tango_color
	    "5x7"			;font
	    ))

(setq text_image
      (send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.87)		;location_coord
	    "Xyzg"	;text_string	   
	    "red"			;tango_color
	    "-*-*-bold-r-*--60-*-*-*-p-*-iso8859-1"
;;;	    "fg-22"			;font
;;;	    "-sun-open look glyph-*-*-*-*-20-*-*-*-*-*-*-*"
	    ))
)

(setq text_image
      (send TANGO:TEXT_IMAGE_CLASS :new :show :invisible tango_w
	    #C(0.0 0.0)			;location_coord
	    "Xyzg"	;text_string	   
	    "MidnightBlue"		;tango_color
	    "-*-*-bold-r-*--60-*-*-*-p-*-iso8859-1"
	    ))

(send tango_w :get_images :visible)
(send tango_w :get_images :invisible)
(send tango_w :get_images)

;; (dolist (elt (send tango_w :get_images))
;; 	(send elt :tx_visible :perform)
;; 	)
;; 
;; (apply #'tango:tx_compose :perform
;;        (mapcar
;; 	#'(lambda (x) (send x :tx_visible))
;; 	(send tango_w :get_images)
;; 	))
