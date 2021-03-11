; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         icls-plrli.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/icls-plrli.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Define POLAR-LINE-IMAGE-CLASS -- a subclass of
;		TANGO:LINE_IMAGE_CLASS.
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 03:27:33 1994 (Niels Mayer) npm@indeed
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

;;;
;;; POLAR-LINE-IMAGE-CLASS -- a subclass of TANGO:LINE_IMAGE_CLASS
;;;
(defclass POLAR-LINE-IMAGE-CLASS	;name of the new subclass
  (					;new ivars for this subclass
   ivar_end_coord			;complex
   ivar_phase_float			;float
   ivar_magnitude_float			;float
   )				
  ()					;no class variables for subclass
  TANGO:LINE_IMAGE_CLASS		;name of the superclass
  )

;;;
;;; override TANGO:WIDGET_CLASS instance initializer (method :isnew)...
;;;
;;; (send POLAR-LINE-IMAGE-CLASS  [:show :[in]visible] <tango_w>
;;;				       <origin_coord> <magnitude_float> <phase_float>
;;;				       <tango_color> <line_width_float> <line_style_float>
;;;                                    )
;;;
;;; where <phase-float> ranges from -PI to PI; 0.0 --> 0 degrees; pi/2 --> 90 degrees; pi --> 180deg; -pi/2 --> 270 deg
;;;
(defmethod POLAR-LINE-IMAGE-CLASS :ISNEW
  (tango_w origin_coord magnitude_float phase_float tango_color line_width_float line_style_float)
  ;; initialize instance vars
  (setq ivar_end_coord		(* magnitude_float (exp (* #C(0.0 1.0) (- phase_float)))) ;cis == cos(x) + i*sin(x) = e^(i*x)
	ivar_phase_float		phase_float
	ivar_magnitude_float	magnitude_float
	)

  ;; create 'self', an instance of TANGO:LINE_IMAGE_CLASS
  (send-super :ISNEW			;create TANGO:LINE_IMAGE_CLASS instance
	      tango_w
	      origin_coord		;location_coord
	      ivar_end_coord		;size_coord
	      tango_color		;tango_color
	      line_width_float		;width_float
	      line_style_float		;style_float
	      :forw_arrow		;arrow_int
	      )
;;; 	(cond				;conditionalize for optional argument frobbing...
;;; 	 ((and k_show_p (or (eq show :visible) (eq show :invisible)))
;;; 	  (send-super :ISNEW :show show	;create TANGO:LINE_IMAGE_CLASS instance
;;; 		      tango_w
;;; 		      origin_coord	;location_coord
;;; 		      ivar_end_coord	;size_coord
;;; 		      tango_color	;tango_color
;;; 		      line_width_float	;width_float
;;; 		      line_style_float	;style_float
;;; 		      :forw_arrow	;arrow_int
;;; 		      )
;;; 	  )
;;; 	 (k_show_p
;;; 	  (error ":VISIBLE or :INVISIBLE keyword parameter missing" show)
;;; 	  )
;;; 	 (T
;;; 	  (send-super :ISNEW		;create TANGO:LINE_IMAGE_CLASS instance
;;; 		      tango_w
;;; 		      origin_coord	;location_coord
;;; 		      ivar_end_coord	;size_coord
;;; 		      tango_color	;tango_color
;;; 		      line_width_float	;width_float
;;; 		      line_style_float	;style_float
;;; 		      :forw_arrow	;arrow_int
;;; 		      )
;;; 	  ))
  )

;;;
;;; <phase-float> ranges from -PI to PI; 0.0 --> 0 degrees; pi/2 --> 90 degrees; pi --> 180deg; -pi/2 --> 270 deg
;;;
(defmethod POLAR-LINE-IMAGE-CLASS :SET-PHASE
  (phase_float
   &aux (new_end_coord			;'&aux...' == 'let...new_end_coord'
	 (* ivar_magnitude_float
	    (exp (* #C(0.0 1.0) (- phase_float))) ;cis == cos(x) + i*sin(x) = e^(i*x)
	    )))
  (send-super :tx_resize :perform (- new_end_coord ivar_end_coord))

  (setq ivar_phase_float phase_float
	ivar_end_coord   new_end_coord
	)
  )

;;;
;;; <phase-float> ranges from -PI to PI; 0.0 --> 0 degrees; pi/2 --> 90 degrees; pi --> 180deg; -pi/2 --> 270 deg
;;;
(defmethod POLAR-LINE-IMAGE-CLASS :GET-PHASE ()
  ivar_phase_float
  )

(defmethod POLAR-LINE-IMAGE-CLASS :SET-MAGNITUDE
  (magnitude_float
   &aux (new_end_coord			;'&aux...' == 'let...new_end_coord'
	 (* magnitude_float
	    (exp (* #C(0.0 1.0) (- ivar_phase_float))) ;cis == cos(x) + i*sin(x) == e^(i*x)
	    )))

  (send-super :tx_resize :perform (- new_end_coord ivar_end_coord))

  (setq ivar_magnitude_float magnitude_float
	ivar_end_coord   new_end_coord
	)
  )

(defmethod POLAR-LINE-IMAGE-CLASS :GET-MAGNITUDE ()
  ivar_magnitude_float
  )


;;;(send polar_ti :set-phase (* 1.1 pi))
;;;(send polar_ti :set-magnitude .4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/icls-plrli")
