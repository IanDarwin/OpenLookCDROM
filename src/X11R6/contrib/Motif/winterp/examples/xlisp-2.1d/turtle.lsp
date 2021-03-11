; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         turtle.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/turtle.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Turtle graphics primitives -- for MS-DOS
; Author:       Peter Ashwood-Smith, Tom Almy
; Created:      April 2nd, 1986 
; Modified:     Mon Jun  6 03:00:45 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       X11r6 contrib release
; 
; Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
; WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
; WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
; 
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Enterprise Integration Technologies,
; Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Enterprise Integration
; Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
; Betz make no representations about the suitability of this software for any
; purpose. It is provided "as is" without express or implied warranty.
; 
; ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
; LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
; SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
; IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
; COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
; INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
; OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TURTLE.L for PC-LISP.EXE V2.10
;; Modified for XLISP 2.1d by Tom Almy
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;      A set of turtle graphics primitives to demonstrate PC-LISP's BIOS 
;; graphics routines. These routines are pretty self explanitory. The first
;; 5 defun's define the primitives, next are a set of routines to draw things
;; like squares, triangles etc. Try the function (GraphicsDemo). It will
;; draw Squirals, Trianglerals, etc. Note that the BIOS line drawing is really
;; slow. This is because the BIOS 'set dot/pixel' routine is used for every
;; point in a line. Using the BIOS has the advantage however of portability,
;; these routines work on virtually every MS-DOS machine. The global variable
;; *GMODE* controls the graphics resolution that will be used. It is set by 
;; default to 6 I set it to 8 or 9 for my 2000 but these routines will not
;; support the lower resolution modes. 
;;
;;                      Peter Ashwood-Smith
;;                      April 2nd, 1986 
;;


;; Several bugs  fixed by Tom Almy
;; The playing field is 200x200, after scaling.
;; Lfactor = ypixels/200
;; Scale = xpixels/ypixels
;; CenterX=CenterY= ypixels/2



(defvar *GMODE* 18)                                     ; default setting


(if (fboundp 'get-internal-run-time)
    (defun pause (time) 
	   (let ((fintime (+ (* time internal-time-units-per-second)
			     (get-internal-run-time))))
		(loop (when (> (get-internal-run-time) fintime)
			    (return-from pause)))))
    (defun pause () (dotimes (x (* time 1000)))))


(defun TurtleGraphicsUp()           
       (case *GMODE*
	     (6				; 640x200 B&W mode
	      (mode 6)
	      (setq CenterX 100 CenterY 100 Scale 3.2 Lfactor 1) 
	      (TurtleCenter))  
	     (16			; 640x350 Graphics
	      (mode 16)
	      (setq CenterX 175 CenterY 175 Scale 1.83 Lfactor 1.75) 
	      (TurtleCenter))  
	     (18			; 640x480 VGA Graphics
	      (mode 18)
	      (setq CenterX 240 CenterY 240 Scale 1.33 Lfactor 2.4) 
	      (TurtleCenter))  
 	    (t (error "unsupported *GMODE*" *GMODE*))
       )
       (color 15)
)   

(defun TurtleGraphicsDown() 
	(mode 3))
(defun TurtleCenter()       
	(setq Lastx CenterX Lasty CenterY Heading 1.570796372))
(defun TurtleRight(n)       (setq Heading (- Heading (* n 0.01745329))))
(defun TurtleLeft(n)        (setq Heading (+ Heading (* n 0.01745329))))
(defun TurtleGoto(x y)      (setq Lastx (* x Lfactor) Lasty (* y Lfactor) )) 

(defun TurtleForward(n) 
      (setq n (* n Lfactor) 
      	    Newx (+ Lastx (* (cos Heading) n))
	    Newy (+ Lasty (* (sin Heading) n)))
      (move (truncate (* Lastx Scale))
            (truncate Lasty)
	    (truncate (* Newx Scale))
	    (truncate Newy))
      (setq Lastx Newx Lasty Newy)
)

;
; end of Turtle Graphics primitives, start of Graphics demonstration code
; you can cut this out if you like and leave the Turtle primitives intact.
;

(defun Line_T(n)        
	(TurtleForward n) (TurtleRight 180)
	(TurtleForward (/ n 4)) 
)
	
(defun Square(n)
	(TurtleForward n)  (TurtleRight 90)     
	(TurtleForward n)  (TurtleRight 90)     
	(TurtleForward n)  (TurtleRight 90)     
	(TurtleForward n)                       
)

(defun Triangle(n)
	(TurtleForward n)  (TurtleRight 120)
	(TurtleForward n)  (TurtleRight 120)
	(TurtleForward n)
)

(defun Make(ObjectFunc Size star skew) 
      (dotimes (dummy star)
	   (apply ObjectFunc (list Size)) 
	   (TurtleRight skew)
       )
)

(defun GraphicsDemo()
	   (TurtleGraphicsUp) 
	   (Make #'Square 40 18 5) (Make #'Square 60 18 5)
	   (pause 1.0)
	   (TurtleGraphicsUp) 
	   (Make #'Triangle 40 18 5) (Make #'Triangle 60 18 5)
	   (pause 1.0)
	   (TurtleGraphicsUp) 
	   (Make #'Line_T 80 50 10)
	   (pause 1.0)
	   (TurtleGraphicsDown)
)

(print "Try (GraphicsDemo)")

