; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         test-tango.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/test-tango.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Various interactive tests/demos of WINTERP's xtango functionality.
;		this file can either be loaded, or you may evaluate stements in the
;		file interactively.
; Author:       Niels P. Mayer
; Created:      Thu Mar 18 19:18:25 1993
; Modified:     Mon Jun  6 04:10:41 1994 (Niels Mayer) npm@indeed
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

(require "xtango/cls-image")		;define methods on all tango-image classes allowing movement/resizing, etc.
(require "xtango/cls-widget")		;define XTANGO-WIDGET-CLASS, XTANGO-BUTTON-WIDGET-CLASS

;; (send tango_w :refresh)
(let ()
(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	    "test-tango"
	    :XMN_TITLE			"WINTERP: Xtango Test"
	    :XMN_ICON_NAME		"W:test-tango"
	    ))

(setq tango_w
      (send XTANGO-WIDGET-CLASS :new :managed
	    "tango" top_w
	    :XMN_HEIGHT 300
	    :XMN_WIDTH 300
	    ))

(progn
  (send top_w :realize)
  (send tango_w :forced_expose_update)	;wait until exposed to ensure windows created for :begin_drawing call
  (send tango_w :begin_drawing)		;must call this after :realize
  )

(send TANGO:LINE_IMAGE_CLASS :new :show tango_w
      #C(0.50 1.0)			;location_coord
      (complex 0.0  (/ 20 -30.0))	;size_coord
      TANGO_COLOR_BLACK			;tango_color
      1.0				;width_float
      0.333				;style_float
      :no_arrow				;arrow_int
      )

(setq im1
      (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w
	    #C(0.25 0.25)		;location_coord
	    0.2				;radius_float
	    TANGO_COLOR_MAROON		;tango_color
	    1.0				;fill_float
	    ))

(setq im2
      (send TANGO:RECTANGLE_IMAGE_CLASS :new :show tango_w
	    #C(0.75 0.75)		;location_coord
	    #C(0.2 0.1)			;size_coord
	    TANGO_COLOR_RED		;tango_color
	    1.0				;fill_float   
	    ))

(setq im3
      (send TANGO:RECTANGLE_IMAGE_CLASS :new :show tango_w
	    #C(0.55 0.19)
	    #C(0.2 0.2)
	    TANGO_COLOR_BLUE		;tango_color
	    1.0				;fill_float 
	    ))

(setq eyejump-bitmovie-arr
      #(
	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 7 7 7 7 7 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 7 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 7 0 0 0 7 0 7 7 7 0 0 0 7 0 0 0 7 0 0 0 0)
	  #(0 0 0 7 0 0 0 0 7 7 7 7 0 0 7 0 0 0 0 7 0 0 0 0)
	  #(0 0 7 0 0 0 0 7 0 7 7 7 7 7 0 7 0 0 0 0 7 0 0 0)
	  #(0 0 7 0 7 0 0 0 7 7 7 7 7 7 7 0 0 0 7 0 7 0 0 0)
	  #(0 7 0 0 7 0 0 7 0 7 7 7 7 7 0 7 0 0 7 0 0 7 0 0)
	  #(0 7 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 7 0 7 0 0)
	  #(7 0 0 0 7 0 7 0 0 0 0 0 0 0 0 0 7 0 7 0 0 0 7 0)
	  #(7 0 0 0 7 0 0 7 7 0 0 7 0 0 7 7 0 0 7 0 0 0 7 0)
	  #(0 7 7 7 0 0 0 0 7 0 7 0 7 0 7 0 0 0 0 7 7 7 0 0)
	  #(0 0 0 0 0 0 7 7 0 0 7 0 7 0 0 7 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 7 7 0 0 0 0 0 7 0 0 0 0 0 7 7 0 0 0 0 0)
	  #(0 0 0 7 7 7 7 7 7 7 7 0 7 7 7 7 7 7 7 7 0 0 0 0)
	  )
	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 7 7 7 7 7 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 7 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 7 0 0 0 7 0 7 7 7 0 0 0 7 0 0 0 7 0 0 0 0)
	  #(0 0 7 0 0 0 0 0 7 7 7 7 0 0 7 0 0 0 0 0 7 0 0 0)
	  #(0 7 0 0 0 0 0 7 0 7 7 7 7 7 0 7 0 0 0 0 0 7 0 0)
	  #(0 7 0 0 7 0 0 0 7 7 7 7 7 7 7 0 0 0 7 0 0 7 0 0)
	  #(0 7 0 7 7 0 0 7 0 7 7 7 7 7 0 7 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 7 0 7 0 0)
	  #(7 0 0 0 7 7 0 0 0 0 0 0 0 0 0 0 0 7 7 0 0 0 7 0)
	  #(7 0 0 0 7 0 7 0 0 0 0 0 0 0 0 0 7 0 7 0 0 0 7 0)
	  #(0 7 7 7 0 0 0 7 7 0 0 7 0 0 7 7 0 0 0 7 7 7 0 0)
	  #(0 0 0 0 0 0 0 0 7 0 7 0 7 0 7 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 7 0 7 0 7 0 7 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 7 0 0 7 0 7 0 0 7 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 7 7 0 0 0 0 0 7 0 0 0 0 0 7 7 0 0 0 0 0)
	  #(0 0 0 7 7 7 7 7 7 7 7 0 7 7 7 7 7 7 7 7 0 0 0 0)
	  )
	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 7 7 7 7 7 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 7 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 0 0 0)
	  #(0 0 7 7 0 0 0 7 0 7 7 7 0 0 0 7 0 0 0 7 7 0 0 0)
	  #(0 7 0 0 0 0 0 0 7 7 7 7 0 0 7 0 0 0 0 0 0 7 0 0)
	  #(0 7 0 0 0 0 0 7 0 7 7 7 7 7 0 7 0 0 0 0 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 7 7 7 7 7 7 7 0 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 7 0 0 7 0 7 7 7 7 7 0 7 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 7 0 7 0 0)
	  #(7 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 7 0)
	  #(7 0 0 0 7 7 0 0 0 0 0 0 0 0 0 0 0 7 7 0 0 0 7 0)
	  #(0 7 7 7 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 7 7 7 0 0)
	  #(0 0 0 0 0 0 0 7 0 0 7 7 7 0 0 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 7 0 0 0 7 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 0 7 0 0 0 7 0 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 7 0 0 7 0 7 0 0 7 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 7 7 0 0 0 0 0 7 0 0 0 0 0 7 7 0 0 0 0 0)
	  #(0 0 0 7 7 7 7 7 7 7 7 0 7 7 7 7 7 7 7 7 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  )
	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 7 7 7 7 7 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 7 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 0 0)
	  #(0 0 7 7 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 7 7 0 0 0)
	  #(0 7 0 0 0 0 0 7 0 7 7 7 0 0 0 7 0 0 0 0 0 7 0 0)
	  #(0 7 0 7 7 7 0 0 7 7 7 7 0 0 7 0 0 7 7 0 0 7 0 0)
	  #(0 7 0 0 7 0 0 7 0 7 7 7 7 7 0 7 0 0 7 0 0 7 0 0)
	  #(0 7 0 0 7 0 0 0 7 7 7 7 7 7 7 0 0 0 7 0 0 7 0 0)
	  #(0 7 7 7 0 0 0 7 0 7 7 7 7 7 0 7 0 0 0 7 7 7 0 0)
	  #(7 0 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 0 7 0)
	  #(7 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 7 0)
	  #(0 7 7 7 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 7 7 7 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 0 0 7 7 7 0 0 7 0 0 0 0 0 0 0 0)
	  #(0 7 7 7 0 0 7 0 0 7 0 0 0 7 0 0 7 0 0 7 7 7 0 0)
	  #(0 0 7 0 7 7 0 0 7 0 0 0 0 0 7 0 0 7 7 0 7 0 0 0)
	  #(0 0 0 7 0 0 0 7 0 0 0 0 0 0 0 7 0 0 0 7 0 0 0 0)
	  #(0 0 0 0 7 0 7 0 0 0 0 0 0 0 0 0 7 0 7 0 0 0 0 0)
	  #(0 0 0 0 0 7 7 0 0 0 0 0 0 0 0 0 7 7 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  )
	))

(setq im4
      (send TANGO:BITMAP_IMAGE_CLASS :new :show tango_w
	    #C(0.1 0.7)			;location_coord
	    eyejump-bitmovie-arr	;bitmap array
	    ))

(setq im5
      (send TANGO:BITMAP_IMAGE_CLASS :new :show tango_w
	    #C(0.2 0.7)			;location_coord
	    eyejump-bitmovie-arr	;bitmap array
	    ))

;;;(setq eyejump-bitmovie-arr nil)

;;;(setq tango_w (get_moused_widget))

(setq im6
      (send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
	    #C(0.10 0.93) 		;location_coord
;;;	    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=+|><,./?~!@#$%^&*()"
;;;	    "The quick Brown Fox Jumped Over The Lazy Dog" ;text_string
	    "IMAGINE VISUALIZATION"
;;;	     "text"
;;;	    "IMAGINE VISUALS"
	    "BlueViolet"		;tango_color
;;;	    "-sun-open look glyph-*-*-*-*-5-*-*-*-*-*-*-*"
;;;	     "-*-*-bold-r-*--18-*-*-*-p-*-iso8859-1" ; -- top clipped, leaves droppings w/ "IMAGINE VISUALIZATION" and "text"
	    "-*-*-*-i-*--24-*-*-*-p-*-iso8859-1"
;;;	     "6x13"			             ; -- ok for "IMAGINE VISUALIZATION", leaves droppings for "text"
;;;	    "hp8.10x20b"			     ; -- top clipped, leaves droppings w/ "IMAGINE VISUALIZATION"
;;;	    "fg-22"                                  ; -- top clipped, leaves droppings w/ "IMAGINE VISUALIZATION"
;;;	     NIL				     ; -- ok for "IMAGINE VISUALIZATION", leaves droppings for "text"
	    ))
;; (send im6 :tx_delete :perform)

;;; this is kinda bogus, since :xmn_input_callback gets called
;;; for both buttonpress, buttonrelease, keypress, keyrelease
;;; events, and we'd have to intercept/filter event stream
;;; in order to get, say, one callback fire per press/release
;;; pair.... we're better off using event handlers, where we
;;; can specify e.g. BUTTON_PRESS_MASK, BUTTON_RELEASE_MASK,
;;; KEY_PRESS_MASK, KEY_RELEASE_MASK, etc.
;;	(send tango_w :set_callback :xmn_input_callback
;;	      '(CALLBACK_XEVENT CALLBACK_WIDGET)
;;	      '(
;;		(let ((timage (send CALLBACK_WIDGET :get_event_image CALLBACK_XEVENT)))
;;		  (if timage
;;		      (send timage :tap_flash :perform 1)
;;		    ))
;;		))

;;(require "xlisp-2.1d/classes")	;from xlisp-2.1*/ subdir...
;;(require "xlisp-2.1d/pp")		;from xlisp-2.1*/ subdir...

(send tango_w :add_event_handler BUTTON_PRESS_MASK
      '(EVHANDLER_WIDGET EVHANDLER_XEVENT)
      '(
	(let ((timage (send EVHANDLER_WIDGET :get_event_image EVHANDLER_XEVENT)))
	  (if timage
	      (progn
		(send tango_w :SET_DELAY 200) ;flash is too fast to see
		(send timage :tap_flash :perform 2)
		(send tango_w :SET_DELAY 0)
		(format t
			"\
------------------------------------------------------------------------------\
TANGO-IMAGE: ~A\
IMAGE-CLASS: ~A\
"
			(generic timage)
			(generic (send timage :class)))
		)
	    ))
	))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run animation displaying jumping eyeballs playing leapfrog...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ()
  (send im4 :TAP_JUMP :perform :sw #C(0.0 0.99))
  (send im5 :TAP_JUMP :perform :sw #C(0.0 0.99))

  (send tango_w :SET_DELAY 1000)	;SLOW IT DOWN!

  (do ((loc4 (send im4 :IMAGE_LOC :nw)
	     (send im4 :IMAGE_LOC :nw))
       (loc5 (send im5 :IMAGE_LOC :nw)
	     (send im5 :IMAGE_LOC :nw))
       (current_eyeball im5))

      ((or (>= (realpart loc4) 0.9)
	   (>= (realpart loc5) 0.9)))

      ;; alternate between the two different images created at the beginning
      ;; of this file.
      (if (eq current_eyeball im4)
	  (setq current_eyeball im5)
	(setq current_eyeball im4))

      ;; show eyeball getting ready to jump by running one cycle of 4
      ;; frames, then 3 more to show eyeball "in-the-air" during
      ;; execution of :tap_traverse -- 7 total frames.
      (send current_eyeball :TX_SHUFFLE :perform 7)

      (if (>= (realpart loc4) (realpart loc5))
	  (send current_eyeball :TAP_TRAVERSE :perform :nw
		(+ loc4 #C(0.1 0.0))
		:clockwise)
	(send current_eyeball :TAP_TRAVERSE :perform :nw
	      (+ loc5 #C(0.1 0.0))
	      :clockwise))

      ;; since we did 7 frames above, move back to 0th frame to
      ;; show the eyeball back in the "landed" position.
      (send current_eyeball :TX_SHUFFLE :perform 1)

      )

  (send tango_w :SET_DELAY 0)		;RESET SLOWDOWN!
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run animation displaying a polygon moving like an amoeba (?)...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq polyg_image
      (send TANGO:POLYGON_IMAGE_CLASS :new :show tango_w
	    #C(0.00 0.50)		;location_coord
	    #C(+0.10 +0.20)
	    #C(+0.20 -0.10)
	    #C(-0.10 +0.20)
	    #C(+0.15 -0.10)
	    #C(-0.10 +0.20)
	    #C(+0.20 +0.10)
	    #C(-0.10 -0.20)
	    "Black"			;tango_color
	    1.0				;fill_float
	    ))
(setq polyg_image
      (send TANGO:SPLINE_IMAGE_CLASS :new :show tango_w
	    #C(0.00 0.50)
	    #C(+0.10 +0.10)
	    #C(+0.10 -0.10)
	    #C(-0.10 +0.10)
	    #C(+0.05 -0.10)
	    #C(-0.10 +0.10)
	    #C(+0.10 +0.10)
	    #C(-0.10 -0.10)
	    "Black"			;tango_color
	    1.0				;fill_float
	    1.0
	    ))

;; (setq tango_w (get_moused_widget))
;; (setq polyg_image (send tango_w :input_image))

(let* ((pa0 (TANGO:PATH_TYPE :CLOCKWISE))
       (pa1 (TANGO:PATH_SCALE pa0 #C(-1.0 -1.0)))
       )

  (TANGO:TX_ITERATE
   :perform
   (tango:tx_concatenate
    (tango:tx_compose 
     (send polyg_image :tx_move  pa0)
     (send polyg_image :tx_grab1 pa1)
     (send polyg_image :tx_grab2 pa1)
     (send polyg_image :tx_grab3 pa1)
     (send polyg_image :tx_grab4 pa1)
     (send polyg_image :tx_grab5 pa1)
     (send polyg_image :tx_grab6 pa1)
     (send polyg_image :tx_grab7 pa1)
     )
    (send polyg_image :tx_grab1 pa0)
    (send polyg_image :tx_grab2 pa0)
    (send polyg_image :tx_grab3 pa0)
    (send polyg_image :tx_grab4 pa0)
    (send polyg_image :tx_grab5 pa0)
    (send polyg_image :tx_grab6 pa0)
    (send polyg_image :tx_grab7 pa0)
    )
   5)
  )

(let* ((pa0 (TANGO:PATH_TYPE :COUNTERCLOCKWISE))
       (pa1 (TANGO:PATH_SCALE pa0 #C(-1.0 -1.0)))
       )
  (TANGO:TX_ITERATE
   :perform
   (tango:tx_concatenate
    (tango:tx_compose 
     (send polyg_image :tx_move  pa0)
     (send polyg_image :tx_grab1 pa1)
     (send polyg_image :tx_grab2 pa1)
     (send polyg_image :tx_grab3 pa1)
     (send polyg_image :tx_grab4 pa1)
     (send polyg_image :tx_grab5 pa1)
     (send polyg_image :tx_grab6 pa1)
     (send polyg_image :tx_grab7 pa1)
     )
    (send polyg_image :tx_grab1 pa0)
    (send polyg_image :tx_grab2 pa0)
    (send polyg_image :tx_grab3 pa0)
    (send polyg_image :tx_grab4 pa0)
    (send polyg_image :tx_grab5 pa0)
    (send polyg_image :tx_grab6 pa0)
    (send polyg_image :tx_grab7 pa0)
    )
   5))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test methods from t_classes.c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send im1 :EXISTS_P)      
(send im2 :EXISTS_P)      
(send im1 :PRIN1)      
(send im2 :PRIN1)      
(send im1 :TAP_SHOW)
(send im2 :TAP_SHOW)
(send im1 :TAP_FILL :perform)
(send im2 :TAP_FILL :perform)
(send im1 :TAP_COLOR :perform (random 8))
(send im1 :TAP_COLOR :perform (random 8))
(send im1 :TAP_COLOR :perform (random 8))
(send im1 :TAP_COLOR :perform (random 8))
(send im2 :TAP_COLOR :perform (random 8))
(send im2 :TAP_COLOR :perform (random 8))
(send im2 :TAP_COLOR :perform (random 8))
(send im2 :TAP_COLOR :perform (random 8))
(send im1 :TAP_VIS_TOGGLE :perform)
(send im1 :TAP_VIS_TOGGLE :perform)
(send im2 :TAP_VIS_TOGGLE :perform)
(send im2 :TAP_VIS_TOGGLE :perform)
(send im1 :TAP_FLASH :perform 1)
(send im2 :TAP_FLASH :perform 1)
(TANGO:TAP_SWITCH :perform im1 im2)
(TANGO:TAP_SWITCH :perform im2 im1)
(TANGO:TAP_EXCHANGE :perform im1 im2)
(TANGO:TAP_EXCHANGE :perform im1 im2)
(send im4 :TX_SHUFFLE :perform 1)

(setq path_color_r (TANGO:PATH_COLOR tango_color_red))
(setq path_color_g (TANGO:PATH_COLOR tango_color_green))
(setq path_color_b (TANGO:PATH_COLOR tango_color_blue))
(send im1 :TX_COLOR :perform path_color_r)
(send im1 :TX_COLOR :perform path_color_g)
(send im1 :TX_COLOR :perform path_color_b)

(setq tx_exchange (TANGO:TAP_EXCHANGE im1 im2))
(TANGO:TX_PERFORM tx_exchange)
(setq tx_exchange2 (TANGO:TAP_EXCHANGE im1 im2))
(TANGO:TX_PERFORM tx_exchange2)

(progn
  (TANGO:TX_PERFORM tx_exchange)
  (TANGO:TX_PERFORM tx_exchange2)
  )

(send im1 :image_loc :ctr)
(send im2 :image_loc :ctr)
(send im3 :image_loc :ctr)
(send im4 :image_loc :ctr)
(send im5 :image_loc :ctr)

(send im1 :image_loc :nw)
(send im2 :image_loc :nw)
(send im3 :image_loc :nw)
(send im4 :image_loc :nw)
(send im5 :image_loc :nw)

(send im1 :tap_jump :perform :ctr #C(0.8 0.8))
(send im2 :tap_jump :perform :nw #C(0.1 0.1))
(send im3 :tap_jump :perform :nw #C(0.5 0.5))
(send im4 :tap_jump :perform :nw #C(0.1 0.9))
(send im5 :tap_jump :perform :nw #C(0.2 0.9))

(send im1 :tap_move :perform :ctr #C(0.2 0.3))
(send im2 :tap_move :perform :nw #C(0.7 0.8))
(send im3 :tap_move :perform :nw #C(0.6 0.6))
(send im4 :tap_move :perform :nw #C(0.8 0.1))
(send im5 :tap_move :perform :nw #C(0.9 0.1))

(send im1 :tap_traverse :perform :ctr #C(0.8 0.8) :clockwise)
(send im2 :tap_traverse :perform :nw #C(0.1 0.1) :clockwise)
(send im3 :tap_traverse :perform :nw #C(0.5 0.5) :clockwise)
(send im4 :tap_traverse :perform :nw #C(0.2 0.9) :clockwise)
(send im5 :tap_traverse :perform :nw #C(0.1 0.9) :clockwise)


(setq im-copy (send im4 :image_copy))
(send im-copy :tx_shuffle :perform 4)
(send im4 :TAP_JUMP :perform :sw #C(0.5 0.5))
(send im-copy :tx_delete :perform)

(setq im-copy (send im5 :image_copy))
(send im-copy :tx_shuffle :perform 4)
(TANGO:TX_PERFORM (send im5 :TAP_JUMP :sw #C(0.4 0.7)))
(send im-copy :tx_delete :perform)


(setq pa (TANGO:PATH_CREATE
	  #(-0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01)
	  #(-0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01)
	  ))
(TANGO:PATH_LENGTH pa)
(TANGO:PATH_DX pa)
(TANGO:PATH_DY pa)
(setq tx (send im5 :TX_MOVE pa))
(TANGO:TX_PERFORM tx)


(setq pa1 (TANGO:PATH_TYPE :STRAIGHT))
(setq pa2 (TANGO:PATH_TYPE :CLOCKWISE))
(setq pa3 (TANGO:PATH_TYPE :COUNTERCLOCKWISE))
(TANGO:PATH_LENGTH pa1)
(TANGO:PATH_LENGTH pa2)
(TANGO:PATH_LENGTH pa3)
(TANGO:PATH_DX pa1)
(TANGO:PATH_DX pa2)
(TANGO:PATH_DX pa3)
(TANGO:PATH_DY pa1)
(TANGO:PATH_DY pa2)
(TANGO:PATH_DY pa3)
(setq tx1 (send im3 :TX_MOVE pa1))
(setq tx2 (send im3 :TX_MOVE pa2))
(setq tx3 (send im3 :TX_MOVE pa3))
(TANGO:TX_PERFORM tx1)
(TANGO:TX_PERFORM tx2)
(TANGO:TX_PERFORM tx3)


(send im1 :TX_MOVE :perform
      (TANGO:PATH_CREATE
       #(-0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01)
       #(-0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01)
       ))
(send im1 :TX_MOVE :perform
      (TANGO:PATH_CREATE
       #(-0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01)
       #( 0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01)
       ))
(send im1 :TX_MOVE :perform
      (TANGO:PATH_CREATE
       #( 0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01)
       #(-0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01 -0.01)
       ))
(send im1 :TX_MOVE :perform
      (TANGO:PATH_CREATE
       #( 0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01)
       #( 0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01)
       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test methods from wc_Xtango.c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send tango_w :set_animation_event_processing NO_EVENT_MASK)
(send tango_w :set_animation_event_processing BUTTON_PRESS_MASK)
(send tango_w :set_animation_event_processing KEY_PRESS_MASK)
(send tango_w :set_animation_event_processing EXPOSURE_MASK)
(send tango_w :set_animation_event_processing (logior BUTTON_PRESS_MASK KEY_PRESS_MASK EXPOSURE_MASK))

(send tango_w :SET_DELAY 1000000)
(send tango_w :SET_DELAY 0)

(send tango_w :SET_DEBUG t)
(send tango_w :SET_DEBUG nil)

(send tango_w :MONO_PATTERN_REPRESENTATION :colors)
(send tango_w :MONO_PATTERN_REPRESENTATION :fills)
; (send tango_w :MONO_PATTERN_REPRESENTATION nil)


(send tango_w :REFRESH)
; (send tango_w :INPUT_COORD)

(send tango_w :PAN :up 0.1)
(send tango_w :PAN :down 0.1)
(send tango_w :PAN :left 0.05)
(send tango_w :PAN :right 0.05)
(send tango_w :ZOOM :in)
(send tango_w :ZOOM :out)
(send tango_w :SET_COORD 0.0 1.0 1.0 0.0)
(setq coords (send tango_w :INQ_COORD))
(apply 'send `(,tango_w :set_coord ,@coords))

;; (setq tango_w (get_moused_widget))

;; (let ((timage (send tango_w :input_image)))
;;   (if timage
;;       (send timage :tap_flash :perform 1)
;;     (format T ":INPUT_IMAGE returned NIL\n")
;;     ))
(send tango_w :SET_BGCOLOR "lightgrey")
(send tango_w :SET_BGCOLOR "yellow")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (setq tango_w (get_moused_widget))
;;; (setq im1 (send tango_w :input_image)
;;;       im2 (send tango_w :input_image))


(let (tx1 tx2)
  (setq tx1 (TANGO:TAP_EXCHANGE im1 im2))
  (TANGO:TX_PERFORM tx1)
  (setq tx2 (TANGO:TAP_EXCHANGE im1 im2))
  (TANGO:TX_PERFORM tx2)
  (xt_add_timeout
   0
   '(
     (TANGO:TX_PERFORM tx1)
     (TANGO:TX_PERFORM tx2)
     (setq t1 (xt_add_timeout 500 TIMEOUT_OBJ))
     ))
  )

(xt_add_timeout 10000 
		'((xt_remove_timeout t1)))


(xt_add_timeout
 10000
 '(
   (TANGO:TAP_EXCHANGE :perform im1 im2)
   (setq t2 (xt_add_timeout 500 TIMEOUT_OBJ))
   ))

(xt_add_timeout 20000
		'((xt_remove_timeout t2)))

(xt_add_timeout
 20000
 '(
   (dotimes (i 20)
     (send tango_w :ZOOM :out 0.90)
     )
   (dotimes (i 20)
     (send tango_w :ZOOM :in 0.90)
     )
   (setq t3 (xt_add_timeout 200 TIMEOUT_OBJ))
   ))

(xt_add_timeout 30000
		'((xt_remove_timeout t3)))

(xt_add_timeout
 30000
 '(
   (send im4 :TX_SHUFFLE :perform 1)
   (setq t4 (xt_add_timeout 100 TIMEOUT_OBJ))
   ))

(xt_add_timeout 40000
		'((xt_remove_timeout t4)))

(xt_add_timeout
 40000
 '(
   (send im5 :TX_SHUFFLE :perform 1)
   (setq t5 (xt_add_timeout 100 TIMEOUT_OBJ))
   ))

(xt_add_timeout 50000
		'((xt_remove_timeout t5)))

(xt_add_timeout
 50000
 '(
   (TANGO:TAP_SWITCH :perform im1 im2)
   (setq t6 (xt_add_timeout 500 TIMEOUT_OBJ))
   ))

(xt_add_timeout 60000
		'((xt_remove_timeout t6)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq input_callback 
      (send tango_w :set_callback :xmn_input_callback
	    '(CALLBACK_REASON CALLBACK_XEVENT CALLBACK_WIDGET)
	    '(
	      (format T "reason=~A; event=~A; widget=~A\n"
		      CALLBACK_REASON
		      CALLBACK_XEVENT
		      CALLBACK_WIDGET)
	      )))
