; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         icls-plrar.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/icls-plrar.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Define POLAR-ARROW-IMAGE-CLASS -- a subclass of
;		TANGO:POLYGON_IMAGE_CLASS
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 03:26:29 1994 (Niels Mayer) npm@indeed
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

;;;
;;; POLAR-ARROW-IMAGE-CLASS -- a subclass of TANGO:POLYGON_IMAGE_CLASS
;;;
(setq POLAR-ARROW-IMAGE-CLASS		;name of the new subclass
      (send Class :new
	    '(				;new ivars for this subclass
	      ivar_origin_coord		;complex
	      ivar_end_coord		;complex
	      ivar_begin_coord		;complex
	      ivar_right_coord		;complex
	      ivar_left_coord		;complex
	      ivar_phase_float		;float
	      ivar_magnitude_float	;float
	      )				
	    '()                         ;no class variables for subclass
	    TANGO:POLYGON_IMAGE_CLASS	;name of the superclass
 	    )) 

;;;
;;; override TANGO:WIDGET_CLASS instance initializer (method :isnew)...
;;;
;;; (send POLAR-ARROW-IMAGE-CLASS :new [:show :[in]visible] <tango_w>
;;;				       <origin_coord> <magnitude_float> <phase_float>
;;;				       <tango_color> <fill_float>
;;;                                    )
;;;
;;; where <phase-float> ranges from -PI to PI; 0.0 --> 0 degrees; pi/2 --> 90 degrees; pi --> 180deg; -pi/2 --> 270 deg
;;;
(send POLAR-ARROW-IMAGE-CLASS :answer :ISNEW
      '(tango_w origin_coord magnitude_float phase_float tango_color fill_float)
      '(
	;; initialize instance vars
	(setq ivar_origin_coord origin_coord
	      ivar_magnitude_float magnitude_float)
	(send self :_calculate_coords_from_phase phase_float)

	;; create 'self', an instance of TANGO:POLYGON_IMAGE_CLASS
	(send-super :ISNEW		;create TANGO:POLYGON_IMAGE_CLASS instance
		    tango_w	
		    ivar_begin_coord
		    (- ivar_right_coord ivar_begin_coord)
		    (- ivar_end_coord ivar_begin_coord)
		    (- ivar_left_coord ivar_begin_coord)
		    tango_color		;tango_color
		    fill_float		;fill_float
		    )

;;; 	(cond				;conditionalize for optional argument frobbing...
;;; 	 ((and k_show_p (or (eq show :visible) (eq show :invisible)))
;;; 	  (send-super :ISNEW :show show ;create TANGO:POLYGON_IMAGE_CLASS instance
;;; 		      tango_w	
;;; 		      ivar_begin_coord
;;; 		      (- ivar_right_coord ivar_begin_coord)
;;; 		      (- ivar_end_coord ivar_begin_coord)
;;; 		      (- ivar_left_coord ivar_begin_coord)
;;; 		      tango_color	;tango_color
;;; 		      fill_float	;fill_float
;;; 		      )
;;; 	  )
;;; 	 (k_show_p
;;; 	  (error ":VISIBLE or :INVISIBLE keyword parameter missing" show)
;;; 	  )
;;; 	 (T
;;; 	  (send-super :ISNEW		;create TANGO:POLYGON_IMAGE_CLASS instance
;;; 		      tango_w	
;;; 		      ivar_begin_coord
;;; 		      (- ivar_right_coord ivar_begin_coord)
;;; 		      (- ivar_end_coord ivar_begin_coord)
;;; 		      (- ivar_left_coord ivar_begin_coord)
;;; 		      tango_color	;tango_color
;;; 		      fill_float	;fill_float
;;; 		      )
;;; 	  ))
	))

;;;
;;; <phase-float> ranges from -PI to PI; 0.0 --> 0 degrees; pi/2 --> 90 degrees; pi --> 180deg; -pi/2 --> 270 deg
;;;
(send POLAR-ARROW-IMAGE-CLASS :answer :SET-PHASE
      '(phase_float)
      '(
	;; save old instance variables prior to recalculating so that we
	;; can use distance deltas in computing params for :tx_move/:tx_grab
	;; below
	(let (
	      (old_begin_coord	ivar_begin_coord)
	      (old_end_coord	ivar_end_coord)
	      (old_right_coord	ivar_right_coord)
	      (old_left_coord	ivar_left_coord)
	      )

	  ;; recaclulate instance vars based on new <phase_float>
	  (send self :_calculate_coords_from_phase phase_float)

	  (let ((origin_move_delta (- ivar_begin_coord old_begin_coord)))
	    (tango:tx_compose :perform
			      (send-super :tx_move  origin_move_delta)
			      (send-super :tx_grab1 (- ivar_right_coord old_right_coord origin_move_delta))
			      (send-super :tx_grab2 (- ivar_end_coord old_end_coord origin_move_delta))
			      (send-super :tx_grab3 (- ivar_left_coord old_left_coord origin_move_delta))
			      )
	    )
	  )
	))

;;;
;;; <phase-float> ranges from -PI to PI; 0.0 --> 0 degrees; pi/2 --> 90 degrees; pi --> 180deg; -pi/2 --> 270 deg
;;;
(send POLAR-ARROW-IMAGE-CLASS :answer :GET-PHASE
      '()
      '(
	ivar_phase_float
	))

;;;
;;; Internal method for calculating coordinates of the POLAR-ARROW-IMAGE 
;;; based on COMPLEX <origin_coord> and FLONUM <phase_float> <magnitude_float>
;;; This routine only sets instance variables -- doesn't update graphics...
;;;
(send POLAR-ARROW-IMAGE-CLASS :answer :_CALCULATE_COORDS_FROM_PHASE
      '(phase_float)
      '(
 	(let ((cis		(exp (* #C(0.0 1.0) (- phase_float)))) ;cis == cos(x) + i*sin(x) = e^(i*x)
	      (phase_delta	(/ pi 3))
	      (magnitude_delta	(/ ivar_magnitude_float 3))
	      )
 	  (setq 
	   ivar_end_coord	(+ ivar_origin_coord (* ivar_magnitude_float
							cis))
	   ivar_begin_coord	(+ ivar_origin_coord (* (/ ivar_magnitude_float 2)
							cis))
	   ivar_right_coord	(+ ivar_origin_coord (* magnitude_delta
							(exp (* #C(0.0 1.0) (- (+ phase_float phase_delta))))))
	   ivar_left_coord	(+ ivar_origin_coord (* magnitude_delta
							(exp (* #C(0.0 1.0) (- (- phase_float phase_delta))))))
	   ivar_phase_float	phase_float
	   )
 	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/icls-plrar")
