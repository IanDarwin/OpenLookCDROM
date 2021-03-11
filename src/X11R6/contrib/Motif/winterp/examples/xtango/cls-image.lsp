; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         cls-image.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/cls-image.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Installs special methods on all the Xtango image classes. These
;		methods allow the interactive manipulation (move, resize, etc)
;		of images drawn within an instance of XTANGO-WIDGET-CLASS.
;		(see also cls-widget.lsp).
; Author:       Niels P. Mayer
; Created:      Fri Jul  2 10:36:47 1993
; Modified:     Mon Jun  6 04:34:22 1994 (Niels Mayer) npm@indeed
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

(let (
      ;; variable 'initial-selection-offset' is set to a COMPLEX FLONUM
      ;; representing the distance between the location of the
      ;; button-down-event and the center of the selected image.
      (init-button-1-press-offset #C(0.0 0.0))

      ;; variable 'SELECTED-VERTEX' is set by :BUTTON-2-PRESS-CALLPROC
      ;; to set up :BUTTON-2-MOTION-CALLPROC for resizing
      ;; polyline/polygon/spline images. 
      (selected-vertex NIL)
      )

  (let ((button-1-press-common-code
	 '(
	   (setq init-button-1-press-offset
		 (- (send parent-w :get_event_coord xevent)
		    (send self :image_loc :ctr)
		    ))
	   )
	 ))
    (send TANGO:BITMAP_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:CIRCLE_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:COMPOSITE_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:ELLIPSE_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:GIF_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:LINE_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:POLYGON_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:POLYLINE_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:SPLINE_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:RECTANGLE_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    (send TANGO:TEXT_IMAGE_CLASS :answer :BUTTON-1-PRESS-CALLPROC
	  '(parent-w xevent) button-1-press-common-code
	  )
    )

  (send TANGO:BITMAP_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  ))
  (send TANGO:CIRCLE_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  ))
  (send TANGO:COMPOSITE_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  ))
  (send TANGO:ELLIPSE_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  ))
  (send TANGO:GIF_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  ))
  (send TANGO:LINE_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  ))
  (send TANGO:POLYGON_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  (setq selected-vertex
		(find-closest-vertex
		 self
		 (send parent-w :get_event_coord xevent)))
	  ))
  (send TANGO:POLYLINE_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  (setq selected-vertex
		(find-closest-vertex
		 self
		 (send parent-w :get_event_coord xevent)))
	  ))
  (send TANGO:SPLINE_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  (setq selected-vertex
		(find-closest-vertex
		 self
		 (send parent-w :get_event_coord xevent)))
	  ))
  (send TANGO:RECTANGLE_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  ))
  (send TANGO:TEXT_IMAGE_CLASS :answer :BUTTON-2-PRESS-CALLPROC
	'(parent-w xevent)
	'(
	  ))

  (let ((button-3-press-common-code
	 '(
	   (require "xtango/im_methpop")
	   (popup-menu-of-methods-of-object self parent-w xevent)
	   )
	 ))
    (send TANGO:BITMAP_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:CIRCLE_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:COMPOSITE_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:ELLIPSE_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:GIF_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:LINE_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:POLYGON_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:POLYLINE_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:RECTANGLE_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:SPLINE_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    (send TANGO:TEXT_IMAGE_CLASS :answer :BUTTON-3-PRESS-CALLPROC
	  '(parent-w xevent) button-3-press-common-code
	  )
    )

  (send TANGO:BITMAP_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
 	  ))
  (send TANGO:CIRCLE_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
 	  ))
  (send TANGO:COMPOSITE_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
 	  ))
  (send TANGO:ELLIPSE_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
 	  ))
  (send TANGO:GIF_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
 	  ))
  (send TANGO:LINE_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
 	  ))
  (send TANGO:POLYGON_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
	  (setq selected-vertex NIL)
 	  ))
  (send TANGO:POLYLINE_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
	  (setq selected-vertex NIL)
 	  ))
  (send TANGO:SPLINE_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
	  (setq selected-vertex NIL)
 	  ))
  (send TANGO:RECTANGLE_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
 	  ))
  (send TANGO:TEXT_IMAGE_CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC
 	'(parent-w xevent)
 	'(
	  (setq init-button-1-press-offset #C(0.0 0.0))
 	  ))

  (let ((button-1-motion-common-code
	 '(
	   (send self :tx_move :perform 
		 (- (send parent-w :get_event_coord xevent)
		    init-button-1-press-offset
		    (send self :image_loc :ctr)
		    ))
	   )
	 ))
    (send TANGO:BITMAP_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:CIRCLE_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:COMPOSITE_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:ELLIPSE_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:GIF_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:LINE_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:RECTANGLE_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:TEXT_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:POLYGON_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:POLYLINE_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    (send TANGO:SPLINE_IMAGE_CLASS :answer :BUTTON-1-MOTION-CALLPROC
	  '(parent-w xevent) button-1-motion-common-code
	  )
    )

  (send TANGO:BITMAP_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	'(parent-w xevent)
	'(
	  (X_BELL)			;SIGNAL ERROR -- BEEP -- bitmaps not resizeable
	  ))
  (send TANGO:CIRCLE_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	'(parent-w xevent)
	'(
	  (let*
	      ((ev-coord (send parent-w :get_event_coord xevent))
	       (im-coord (send self :image_loc :ctr))
	       (angle    (+ (/ (phase (- ev-coord im-coord)) pi) 0.25)) 
	       )
	    (if (>= angle 1.0)		;nw,w -- special case for 'phase' discontinuity over 'w'
		(send self :tx_resize :perform (- (send self :image_loc :w) ev-coord))
	      (if (>= angle 0.0)	; 0<=angle<1 --> SE, E, NE, N, <NW
		  (if (<= angle 0.5)	; SE, E, NE
		      (send self :tx_resize :perform (- ev-coord (send self :image_loc :e)))
		    ;; angle>0.5 --> NE, N, NW
		    (send self :tx_resize :perform (complex (imagpart (- ev-coord (send self :image_loc :s)))))
		    )
		;; angle<0.0 --> <W, SW, S, SE, <E
		(if (> angle -0.5)	; -- SW, S, SE
		    (send self :tx_resize :perform (complex (imagpart (- (send self :image_loc :n) ev-coord))))
		  ;; angle<-0.5  continue discontinuity for 'phase' over 'w' -- SW, W, NW
		  (send self :tx_resize :perform (- (send self :image_loc :w) ev-coord))
		  )
		)
	      )
	    )
	  ))
  (send TANGO:COMPOSITE_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	'(parent-w xevent)
	'(
	  (send self :tx_resize :perform 
		(- (send parent-w :get_event_coord xevent)
		   (send self :image_loc :se)
		   ))
	  ))
  (send TANGO:ELLIPSE_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	'(parent-w xevent)
	'(
	  (let* ((ev-coord (send parent-w :get_event_coord xevent))
		 (angle    (/ (phase (- ev-coord (send self :image_loc :ctr))) pi)))
	    (if (> angle 0)
		(if (> angle 0.5)
		    (send self :tx_resize :perform (conjugate (- (send self :image_loc :sw) ev-coord)))
		  (send self :tx_resize :perform (- ev-coord (send self :image_loc :se)))
		  )
	      (if (< angle -0.5)
		  (send self :tx_resize :perform (- (send self :image_loc :nw) ev-coord))
		(send self :tx_resize :perform (conjugate (- ev-coord (send self :image_loc :ne))))
		)
	      ))

	  ))
  (send TANGO:GIF_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	'(parent-w xevent)
	'(
	  (X_BELL)			;SIGNAL ERROR -- BEEP -- bitmaps not resizeable
	  ))
  (send TANGO:LINE_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	'(parent-w xevent)
	'(
	  (let* ((ev-coord (send parent-w :get_event_coord xevent))
		 (angle    (/ (phase (- ev-coord (send self :image_loc :ctr))) pi)))
	    (if (>= angle 0)
		(if (>= angle 0.5)
		    (send self :tx_resize :perform (- ev-coord (send self :image_loc :sw)))
		  (send self :tx_resize :perform (- ev-coord (send self :image_loc :se)))
		  )
	      (if (<= angle -0.5)
		  (send self :tx_resize :perform (- ev-coord (send self :image_loc :nw)))
		(send self :tx_resize :perform (- ev-coord (send self :image_loc :ne)))
		)
	      ))
	  ))
  (send TANGO:RECTANGLE_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	'(parent-w xevent)
	'(
	  (send self :tx_resize :perform 
		(- (send parent-w :get_event_coord xevent)
		   (send self :image_loc :se)
		   ))
	  ))
  (send TANGO:TEXT_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	'(parent-w xevent)
	'(
	  (X_BELL)			;SIGNAL ERROR -- BEEP -- text not resizable
	  ))

  ;; this indicates need for a poly superclass for polygon, polyline, spline, but ...
  (let ((button-2-motion-common-code
	 '(
	   (case selected-vertex	;'selected-vertex' is set by button-2-press on polyline/polygon/spline
	     (0 
	      (let
		  ((loc
		    (send parent-w :get_event_coord xevent))
		   (loc0
		    (send self :image_loc 0))
		   (loc1
		    (send self :image_loc 1))
		   (loc2
		    (unwind-protect (send self :image_loc 2) NIL))
		   (loc3
		    (unwind-protect (send self :image_loc 3) NIL))
		   (loc4
		    (unwind-protect (send self :image_loc 4) NIL))
		   (loc5
		    (unwind-protect (send self :image_loc 5) NIL))
		   (loc6
		    (unwind-protect (send self :image_loc 6) NIL))
		   (loc7
		    (unwind-protect (send self :image_loc 7) NIL))
		   )
		(cond
		 (loc7
		  (tango:tx_compose :perform
				    (send self :tx_move  (- loc loc0))
				    (send self :tx_grab1 (- (- loc loc0)))
				    (send self :tx_grab2 (- (- loc loc0)))
				    (send self :tx_grab3 (- (- loc loc0)))
				    (send self :tx_grab4 (- (- loc loc0)))
				    (send self :tx_grab5 (- (- loc loc0)))
				    (send self :tx_grab6 (- (- loc loc0)))
				    (send self :tx_grab7 (- (- loc loc0)))
				    )
		  )
		 (loc6
		  (tango:tx_compose :perform
				    (send self :tx_move  (- loc loc0))
				    (send self :tx_grab1 (- (- loc loc0)))
				    (send self :tx_grab2 (- (- loc loc0)))
				    (send self :tx_grab3 (- (- loc loc0)))
				    (send self :tx_grab4 (- (- loc loc0)))
				    (send self :tx_grab5 (- (- loc loc0)))
				    (send self :tx_grab6 (- (- loc loc0)))
				    )
		  )
		 (loc5
		  (tango:tx_compose :perform
				    (send self :tx_move  (- loc loc0))
				    (send self :tx_grab1 (- (- loc loc0)))
				    (send self :tx_grab2 (- (- loc loc0)))
				    (send self :tx_grab3 (- (- loc loc0)))
				    (send self :tx_grab4 (- (- loc loc0)))
				    (send self :tx_grab5 (- (- loc loc0)))
				    )
		  )
		 (loc4
		  (tango:tx_compose :perform
				    (send self :tx_move  (- loc loc0))
				    (send self :tx_grab1 (- (- loc loc0)))
				    (send self :tx_grab2 (- (- loc loc0)))
				    (send self :tx_grab3 (- (- loc loc0)))
				    (send self :tx_grab4 (- (- loc loc0)))
				    )
		  )
		 (loc3
		  (tango:tx_compose :perform
				    (send self :tx_move  (- loc loc0))
				    (send self :tx_grab1 (- (- loc loc0)))
				    (send self :tx_grab2 (- (- loc loc0)))
				    (send self :tx_grab3 (- (- loc loc0)))
				    )
		  )
		 (loc2
		  (tango:tx_compose :perform
				    (send self :tx_move  (- loc loc0))
				    (send self :tx_grab1 (- (- loc loc0)))
				    (send self :tx_grab2 (- (- loc loc0)))
				    )
		  )
		 (T
		  (tango:tx_compose :perform
				    (send self :tx_move  (- loc loc0))
				    (send self :tx_grab1 (- (- loc loc0)))
				    )
		  )
		 )
		)
	      )
	     (1 (send self :tx_grab1 :perform
		      (- (send parent-w :get_event_coord xevent)
			 (send self :image_loc 1))
		      ))
	     (2 (send self :tx_grab2 :perform
		      (- (send parent-w :get_event_coord xevent)
			 (send self :image_loc 2))
		      ))
	     (3 (send self :tx_grab3 :perform
		      (- (send parent-w :get_event_coord xevent)
			 (send self :image_loc 3))
		      ))
	     (4 (send self :tx_grab4 :perform
		      (- (send parent-w :get_event_coord xevent)
			 (send self :image_loc 4))
		      ))
	     (5 (send self :tx_grab5 :perform
		      (- (send parent-w :get_event_coord xevent)
			 (send self :image_loc 5))
		      ))
	     (6 (send self :tx_grab6 :perform
		      (- (send parent-w :get_event_coord xevent)
			 (send self :image_loc 6))
		      ))
	     (7 (send self :tx_grab7 :perform
		      (- (send parent-w :get_event_coord xevent)
			 (send self :image_loc 7))
		      ))
	     (T (format T "error: no vertex selected\n"))
	     )
	   )
	 ))

    (send TANGO:POLYGON_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	  '(parent-w xevent) button-2-motion-common-code
	  )

    (send TANGO:POLYLINE_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	  '(parent-w xevent) button-2-motion-common-code
	  )

    (send TANGO:SPLINE_IMAGE_CLASS :answer :BUTTON-2-MOTION-CALLPROC
	  '(parent-w xevent) button-2-motion-common-code
	  )
    )
  )


;; This routine returns a FIXNUM [0-7] representing the vertex
;; of <imobj> closest to the coordinate of the COMPLEX FLONUM
;; <event-coord>. <imobj> must be an instance of class
;; TANGO:POLYGON_IMAGE_CLASS, TANGO:POLYLINE_IMAGE_CLASS,
;; TANGO:SPLINE_IMAGE_CLASS. This routine is used by the methods
;; for message :BUTTON-2-PRESS-CALLPROC.
(defun find-closest-vertex (imobj event-coord)
  (let
      ((loc0
	(send imobj :image_loc 0))
       (loc1
	(send imobj :image_loc 1))
       (loc2
	(unwind-protect (send imobj :image_loc 2) NIL))
       (loc3
	(unwind-protect (send imobj :image_loc 3) NIL))
       (loc4
	(unwind-protect (send imobj :image_loc 4) NIL))
       (loc5
	(unwind-protect (send imobj :image_loc 5) NIL))
       (loc6
	(unwind-protect (send imobj :image_loc 6) NIL))
       (loc7
	(unwind-protect (send imobj :image_loc 7) NIL))
       )
    (cond
     (loc7
      (let* (
	     (v7 (abs (- loc7 event-coord)))
	     (v6 (abs (- loc6 event-coord)))
	     (v5 (abs (- loc5 event-coord)))
	     (v4 (abs (- loc4 event-coord)))
	     (v3 (abs (- loc3 event-coord)))
	     (v2 (abs (- loc2 event-coord)))
	     (v1 (abs (- loc1 event-coord)))
	     (v0 (abs (- loc0 event-coord)))
	     (val (min v7 v6 v5 v4 v3 v2 v1 v0))
	     )
	(cond
	 ((= val v7) 7)
	 ((= val v6) 6)
	 ((= val v5) 5)
	 ((= val v4) 4)
	 ((= val v3) 3)
	 ((= val v2) 2)
	 ((= val v1) 1)
	 ((= val v0) 0)
	 )
	)
      )
     (loc6
      (let* (
	     (v6 (abs (- loc6 event-coord)))
	     (v5 (abs (- loc5 event-coord)))
	     (v4 (abs (- loc4 event-coord)))
	     (v3 (abs (- loc3 event-coord)))
	     (v2 (abs (- loc2 event-coord)))
	     (v1 (abs (- loc1 event-coord)))
	     (v0 (abs (- loc0 event-coord)))
	     (val (min v6 v5 v4 v3 v2 v1 v0))
	     )
	(cond
	 ((= val v6) 6)
	 ((= val v5) 5)
	 ((= val v4) 4)
	 ((= val v3) 3)
	 ((= val v2) 2)
	 ((= val v1) 1)
	 ((= val v0) 0)
	 )
	)
      )
     (loc5
      (let* (
	     (v5 (abs (- loc5 event-coord)))
	     (v4 (abs (- loc4 event-coord)))
	     (v3 (abs (- loc3 event-coord)))
	     (v2 (abs (- loc2 event-coord)))
	     (v1 (abs (- loc1 event-coord)))
	     (v0 (abs (- loc0 event-coord)))
	     (val (min v5 v4 v3 v2 v1 v0))
	     )
	(cond
	 ((= val v5) 5)
	 ((= val v4) 4)
	 ((= val v3) 3)
	 ((= val v2) 2)
	 ((= val v1) 1)
	 ((= val v0) 0)
	 )
	)
      )
     (loc4
      (let* (
	     (v4 (abs (- loc4 event-coord)))
	     (v3 (abs (- loc3 event-coord)))
	     (v2 (abs (- loc2 event-coord)))
	     (v1 (abs (- loc1 event-coord)))
	     (v0 (abs (- loc0 event-coord)))
	     (val (min v4 v3 v2 v1 v0))
	     )
	(cond
	 ((= val v4) 4)
	 ((= val v3) 3)
	 ((= val v2) 2)
	 ((= val v1) 1)
	 ((= val v0) 0)
	 )
	)
      )
     (loc3
      (let* (
	     (v3 (abs (- loc3 event-coord)))
	     (v2 (abs (- loc2 event-coord)))
	     (v1 (abs (- loc1 event-coord)))
	     (v0 (abs (- loc0 event-coord)))
	     (val (min v3 v2 v1 v0))
	     )
	(cond
	 ((= val v3) 3)
	 ((= val v2) 2)
	 ((= val v1) 1)
	 ((= val v0) 0)
	 )
	)
      )
     (loc2
      (let* (
	     (v2 (abs (- loc2 event-coord)))
	     (v1 (abs (- loc1 event-coord)))
	     (v0 (abs (- loc0 event-coord)))
	     (val (min v2 v1 v0))
	     )
	(cond
	 ((= val v2) 2)
	 ((= val v1) 1)
	 ((= val v0) 0)
	 )
	)
      )
     (T
      (let* (
	     (v1 (abs (- loc1 event-coord)))
	     (v0 (abs (- loc0 event-coord)))
	     (val (min v1 v0))
	     )
	(cond
	 ((= val v1) 1)
	 ((= val v0) 0)
	 )
	)
      )
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/cls-image")
