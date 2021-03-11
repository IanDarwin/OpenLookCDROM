; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wcls-knob.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/wcls-knob.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  KNOB-WIDGET-CLASS -- use xtango to define the graphics of a
;		new motif widget class without having to use low-level xt
;		widget subclassing uglyness. See also test-knob.lsp
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 04:33:34 1994 (Niels Mayer) npm@indeed
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

(require "xlisp-2.1d/classes")		;define DEFCLASS DEFMETHOD
(require "xtango/icls-plrli")		;define POLAR-LINE-IMAGE-CLASS and methods
					; :SET-PHASE, :SET-MAGNITUDE, :GET-PHASE, :GET-MAGNITUDE 

;;;;; Uncomment the following form and also uncomment the creation of
;;;;; POLAR-ARROW-IMAGE-CLASS instance below if you want this widget
;;;;; to use a large arrow as indicator, rather than line.
;;;
;;; (require "xtango/icls-plrar")	;define POLAR-ARROW-IMAGE-CLASS and method :SET-PHASE
;;; 

;;;
;;; KNOB-WIDGET-CLASS -- a subclass of TANGO:WIDGET_CLASS/:button
;;; containing a TANGO:WIDGET_CLASS with meter semantics
;;;
(defclass KNOB-WIDGET-CLASS
  (					;new ivars for this subclass
   ivar_needle_ti			;POLAR-LINE-IMAGE-CLASS -- subclass of TANGO:LINE_IMAGE_CLASS displaying line at a location of given size and magnitude
   )				
  ()					;no class variables for subclass
  TANGO:WIDGET_CLASS			;name of the superclass
  )

;;;
;;; override TANGO:WIDGET_CLASS instance initializer (method :isnew)...
;;;
;;; (send KNOB-WIDGET-CLASS [:managed] <widget_name> <widget_parent>
;;;       ...)
;;;
(let* (					;def initialization constants...
       (needle_origin #C(0.5 0.5))
       (needle_size   0.45)
       (bezel_size    needle_size)
       (scale_size    (+ bezel_size 0.03))
       )

  (defmethod KNOB-WIDGET-CLASS :ISNEW
    (managed_k widget_name widget_parent &rest args)
    ;; create 'self', an instance of TANGO:WIDGET_CLASS/:button
    (apply #'send-super	:isnew		;call superclass's init to create widget
	   managed_k :button widget_name widget_parent
	   :XMN_PUSH_BUTTON_ENABLED	t
	   args
	   )

    ;; Set up expose callback to draw the button-image once
    ;; the window is created... Subsequent exposes will refresh
    ;; the drawing.
    (let ((init_p NIL))
      (send-super :add_callback :XMN_EXPOSE_CALLBACK
		  '()
		  '(
		    (if init_p
			(send-super :refresh)
		      (progn
			(send-super :begin_drawing)

			(send TANGO:CIRCLE_IMAGE_CLASS :new self ;would use DEFINST here, but I don't need the instance set to a variable/symbol
			      needle_origin ;location_coord
			      bezel_size ;radius_float
			      TANGO_COLOR_BLACK	;tango_color
			      0.0	;fill_float
			      )

			(do ((i -9 (1+ i)))
			    ((> i 10))
			    (let* ((x (* (/ i 10.0) pi))
				   (cis (exp (* #C(0.0 1.0) (- x)))) ;cis == cos(x) + i*sin(x) = e^(i*x)
				   (origin (+ (* bezel_size cis) needle_origin))
				   (endpoint (+ (* scale_size cis) needle_origin))
				   (size (- endpoint origin))
				   )
			      (send TANGO:LINE_IMAGE_CLASS :new self ;would use DEFINST here, but I don't need the instance set to a variable/symbol
				    origin ;location_coord
				    size ;size_coord
				    TANGO_COLOR_BLACK ;tango_color
				    0.0	;width_float
				    1.0	;style_float
				    :no_arrow ;arrow_int
				    )
			      ))

			(definst POLAR-LINE-IMAGE-CLASS ivar_needle_ti
			  self		;<tango_widget>
			  #C(0.5 0.5)	;<location_coord>
			  needle_size	;<magnitude_float>
			  0.0		;<phase_float>
			  TANGO_COLOR_RED ;<tango_color>
			  0.5		;<line_width_float>
			  1.0		;<line_style_float>
			  )

;;;;; Comment out above and uncomment the following form in order to display a large
;;;;; arrow as the indicator for this widget, instead of a line.
;;;;; Note: you also need to uncomment '(require "xtango/icls-plrar")' above...
;;; 
;;; 		   (definst POLAR-ARROW-IMAGE-CLASS ivar_needle_ti
;;; 		     self		;<tango_widget>
;;; 		     #C(0.5 0.5)	;<location_coord>
;;; 		     needle_size	;<magnitude_float>
;;; 		     0.0		;<phase_float>
;;; 		     TANGO_COLOR_RED	;<tango_color>
;;; 		     1.0		;<fill_float>			 
;;; 		     )

			(send-super :refresh)
			(setq init_p t)
			)
		      )
		    ))
      )

    (send-super :add_event_handler BUTTON_PRESS_MASK
		'(EVHANDLER_XEVENT EVHANDLER_BUTTON)
		'(
		  (case EVHANDLER_BUTTON
			(1
			 (send ivar_needle_ti :set-phase
			       (- (phase (- (send self :get_event_coord EVHANDLER_XEVENT)
					    needle_origin)))
			       )
			 ))
		  ))
    (send-super :add_event_handler BUTTON1_MOTION_MASK
		'(EVHANDLER_XEVENT)
		'(
		  (send ivar_needle_ti :set-phase
			(- (phase (- (send self :get_event_coord EVHANDLER_XEVENT)
				     needle_origin)))
			)
		  ))
    )
  )

;;;
;;; value ranges from -1.0 to +1.0, will "wrap" if larger or smaller value given
;;;
(defmethod KNOB-WIDGET-CLASS :SET_VALUE (value)
  (send ivar_needle_ti :set-phase (* pi value))
  )

;;;
;;; returns value between -1.0 and +1.0
;;;
(defmethod KNOB-WIDGET-CLASS :GET_VALUE ()
  (/ (send ivar_needle_ti :get-phase) pi)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/wcls-knob")
