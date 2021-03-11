; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         gblocks.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/gblocks.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Blocks World from Winston&Horn Lisp book
; Author:       Winston&Horn, Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 02:41:58 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       Experimental (Do Not Distribute)
;
; (C) Copyright 1994, Enterprise Integration Technologies Corporation.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Enterprise Integration Technologies not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Enterprise Integration Technologies
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;
; ENTERPRISE INTEGRATION TECHNOLOGIES DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
; SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
; IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES BE LIABLE FOR ANY SPECIAL,
; INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
; OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Blocks World from Winston&Horn
; modified for XLISP and graphics by Tom Almy

(unless (fboundp 'defclass) (load 'classes))

;
; Functions for graphic assistance

(defvar *bx* 0)		; text communication region
(defvar *by* 21)
(defvar *gx* 50)	; Graphic region origin
(defvar *gy* 100)
(defvar *ymax* 349)	; height of display
(defconstant *char-width* 8)	; width of characters
(defvar *char-height* 14)	; height of characters
(defvar *step-size* 10)	; lcd of block widths 
(defvar *delay-time* 0.3)	; delay time in seconds


; Move the cursor to nearest position to graphic coordiates
(if (fboundp 'round)
(defun setgpos (x y)
     (goto-xy (round (+ x *gx*) 8)
	      (round (- *ymax* y *gy*) *char-height*)))
(defun setgpos (x y)
     (goto-xy (truncate (/ (+ x *gx*) 8))
	     (truncate (/ (+ (/ *char-height* 2) (- *ymax* y *gy*)) 
			  *char-height*))))
)

; Move the cursor to the currently set bottom position and clear the line
;  under it
(defun bottom ()
    (goto-xy *bx* (+ *by* 1))
    (cleol)
    (goto-xy *bx* *by*)
    (cleol)
    (goto-xy *bx* (- *by* 1))
    (cleol)
    nil)

; Clear the screen and go to the bottom
(defun cb ()
    (cls)
    (bottom))


; Go to graphics mode
(defun gmode () 
       (mode 16)
       (setq *by* 21)
       (setq *ymax* 349) ; reset defaults
       (setq *char-height* 14))

(defun gmode480 () ; this is for GENOA SuperEGA HiRes+
       (mode 115 115 640 480)
       (setq *ymax* 480)
       (setq *by* 21)
       (setq *char-height* 8))

(defun gmode600 () ; this is for GENOA SuperEGA HiRes+
       (mode 121 121 800 600)
       (setq *by* 21)
       (setq *ymax* 600)
       (setq *char-height* 8))

(defun gmodev () ; EVEREX 640x480 mode
       (setq *by* 21)
       (mode 112 0 640 480)
       (setq *ymax* 480)
       (setq *char-height* 14)
       (display-blocks))

(defun gmodeVGA () ; standard 640x480 VGA
       (mode 18)
       (setq *ymax* 480)
       (setq *by* 9)
       (setq *char-height* 16)
       (display-blocks))

(defun gmodeVGA800 () ; this is for Video 7 FastWrite/VRAM VGA
       (mode 28421 98 800 600)
       (setq *by* 21)
       (setq *ymax* 600)
       (setq *char-height* 8)
       (display-blocks))

; abstract classes for ball types

; basic blocks support nothing
(defclass basic-block (name color width height position supported-by))

(defmethod basic-block :support-for () nil)

(defmethod basic-block :top-location  () 
	(list (+ (first position) (/ width 2))
	      (+ (second position) height)))

(defmethod basic-block :drawname ()
	(setgpos (+ (first position) 
		    (/ (- width (* *char-width* (flatc name))) 2))
	         (+ (second position) (/ height 2)))
	(princ name))

(defmethod basic-block :undrawname ()
	(setgpos (+ (first position) 
		    (/ (- width (* *char-width* (flatc name))) 2)) 
	         (+ (second position) (/ height 2)))
	(dotimes (i (flatc name)) (princ " ")))

(defmethod basic-block :draw ()
	(color (+ color 128))
	(move (+ *gx* (first position)) (+ *gy* (second position)))
	(drawrel (1- width) 0 
		 0 (1- height)
		 (- 1 width) 0 
		 0 (- 1 height)))

; movable-blocks can be moved
(defclass movable-block () () basic-block)

(defmethod movable-block :new-position (newpos)
	(send self :draw)
	(send self :undrawname)
	(setf position newpos)
	(send self :drawname)
	(send self :draw))

; load-bearing blocks can support other blocks, and can be moved
(defclass load-bearing-block (support-for) () movable-block)

; we can't have multiple inheritance, so we need a separate class for table
; table blocks can support other blocks but cannot be moved.

(defclass table-block (support-for) () basic-block)

; Specific classes for table brick wedge and ball

(defclass brick () () load-bearing-block)

(defclass wedge () () movable-block)

(defmethod wedge :draw ()
	(color (+ color 128))
	(move (+ *gx* (first position)) (+ *gy* (second position)))
	(drawrel (1- width) 0 
		 (- 1 (/ width 2)) (1- height )
		 (- (/ width 2) width 1) (- 1 height)))

(defclass ball  () () movable-block)

(defmethod ball :draw ()
	(color (+ color 128))
	(let ((cx (+ (first position) (/ width 2) -1 *gx*))
	      (cy (+ (second position) (/ height 2) -1 *gy*))
	      (fstep (/ 3.14159 18))
	      (radius (1- (/ (min width height) 2))))
	     (move (+ cx radius) cy)
	     (dotimes (i 36)
	              (draw (truncate (+ cx (* radius (cos (* (1+ i) fstep)))))
		      	    (truncate (+ cy (* radius (sin (* (1+ i) fstep)))))))))

(defclass hand  (name position grasping))

(defmethod hand :top-location  () position)

(defmethod hand :draw ()
	(color (if grasping 143 136))
	(move (+ *gx* -7 (first position)) (+ *gy* (second position)))
	(drawrel 5 0 0 10 5 0 0 -10 5 0 0 20 -15 0 0 -20))

(defmethod hand :new-position (newpos)
	(send self :draw)
	(setf position newpos)
	(send self :draw))

; define all the individual blocks

(setf *blocks*
      (list
        (send table-block :new :name 'table :width 430 :height 10 
			       :position '(0 0) :color 7)
	(send brick :new :name 'b1 :width 40 :height 40 
			       :position '(0 10) :color 1)
	(send brick :new :name 'b2 :width 40 :height 40 
			       :position '(40 10) :color 2)
	(send brick :new :name 'b3 :width 80 :height 80 
			       :position '(80 10) :color 3)
	(send brick :new :name 'b4 :width 40 :height 40 
			       :position '(160 10) :color 4)
	(send wedge :new :name 'w5 :width 40 :height 80 
			       :position '(200 10) :color 5)
	(send brick :new :name 'b6 :width 80 :height 40 
			       :position '(240 10) :color 6)
	(send wedge :new :name 'w7 :width 40 :height 40 
			       :position '(320 10) :color 14)
	(send ball  :new :name 'l8 :width 40 :height 40 
			       :position '(360 10) :color 13)
	(send brick :new :name 'b9 :width 30 :height 30 
			       :position '(400 10) :color 12)
       ))

(dolist (l *blocks*) (set (send l :name) l))

(dolist (l (rest *blocks*)) ; all blocks but the table
	(setf (send table :support-for) 
	      (cons l (send table :support-for))
	      (send l :supported-by)
	      table))

(definst hand *hand* :name '*hand* :position '(0 120))

(defun display-blocks ()
	(cls)
	(dolist (l *blocks*) (send l :drawname)(send l :draw))
	(send *hand* :draw)
	(bottom)
	t)

(defmethod basic-block :put-on (support) ; default case is bad
	(format t
		"Sorry, the ~a cannot be moved.~%"
		name))

(defmethod movable-block :put-on (support)
	(if (send self :get-space support)
	    (and (send *hand* :grasp self)
	    	 (send *hand* :move  self support)
		 (send *hand* :ungrasp self))
	    (format t
	    	    "Sorry, there is no room for ~a on ~a.~%"
		    name
		    (send support :name))))

(defmethod movable-block :get-space (support)
	(or (send self :find-space support)
	    (send self :make-space support)))

(defmethod hand :grasp (obj)
	(unless (eq grasping obj)
		(when (send obj :support-for)
		      (send obj :clear-top))
		(when grasping
		      (send grasping :rid-of))
		(let ((lift (max-height self obj)))
		     (send self :new-position lift)
		     (pause *delay-time*)
		     (send self :new-position 
		     	(list (first (send obj :top-location)) (second lift)))
		     (pause *delay-time*)
		     (send self :new-position (send obj :top-location))
		     (pause *delay-time*))
		(send self :draw)
		(setf grasping obj)
		(send self :draw))
	t)

(defmethod hand :ungrasp (obj)
	(when (send obj :supported-by)
	      (send self :draw)
	      (setf grasping nil)
	      (send self :draw)
	      t))


(defmethod movable-block :rid-of ()
	(send self :put-on table))

(defmethod movable-block :make-space (support)
	(dolist (obstruction (send support :support-for))
		(send obstruction :rid-of)
		(let ((space (send self :find-space support)))
		     (when space (return space)))))

(defmethod  load-bearing-block :clear-top ()
	(dolist (obstacle support-for) (send obstacle :rid-of))
	t)


(defmethod hand :move (obj support)
	(send obj :remove-support)
	(let ((newplace (send obj :get-space support)))
          (let ((lift (max-height obj support)))
	     (send obj :new-position lift)
	     (send self :new-position (send obj :top-location))
	     (pause *delay-time*)
	     (send obj :new-position (list (first newplace) (second lift)))
     	     (send self :new-position (send obj :top-location))
	     (pause *delay-time*)
	     (send obj :new-position newplace)
	     (send self :new-position (send obj :top-location))
	     (pause *delay-time*)))
	(send support :add-support obj)
	t)


; helper function to find height necessary to move object

(defun max-height (obj1 obj2)
	(let    ((source (first (send obj1 :top-location)))
	         (dest   (first (send obj2 :top-location))))
	(let	((roof 0) (min (min source dest)) (max (max source dest)) )
		(dolist (obstacle *blocks*)
			(let ((x (send obstacle :top-location)))
			     (when (and (>= (first x) min)
			     		(<= (first x) max)
					(> (second x) roof))
				   (setf roof (second x)))))
		(list (first (send obj1 :position)) (+ 20 roof)))))
				   
(if (fboundp 'get-internal-run-time)
    (defun pause (time) 
	   (let ((fintime (+ (* time internal-time-units-per-second)
			     (get-internal-run-time))))
		(loop (when (> (get-internal-run-time) fintime)
			    (return-from pause)))))
    (defun pause () (dotimes (x (* time 1000)))))


; remove-support-for is defined twice, for each load bearing class

(defmethod load-bearing-block :remove-support-for (obj)
	(setf support-for (remove obj support-for))
	t)

(defmethod table-block :remove-support-for (obj)
	(setf support-for (remove obj support-for))
	t)

(defmethod movable-block :remove-support ()
	(when supported-by
	      (send supported-by :remove-support-for self)
	      (setf supported-by nil))
	t)



(defmethod load-bearing-block :add-support (obj)
	(setf support-for 
	      (cons obj support-for)
	      (send obj :supported-by) 
	      self)
	t)

(defmethod table-block :add-support (obj)
	(setf support-for 
	      (cons obj support-for)
	      (send obj :supported-by) 
	      self)
	t)

(defmethod basic-block :add-support (obj)
	t)

(defmethod movable-block :find-space (support)
	(do     ((offset (- (send support :width) width)
	                 (- offset *step-size*)))
		((< offset 0))
		 (unless (intersections-p self offset
		 			  (first (send support :position))
					  (send support :support-for))
			 (return (list (+ offset (first (send support 
			 				      :position)))
				       (+ (second (send support :position))
				          (send support :height)))))))

(defun intersections-p (obj offset base obstacles)
	(dolist (obstacle obstacles)
		(let* ((ls-proposed (+ offset base))
			(rs-proposed (+ ls-proposed (send obj :width)))
			(ls-obstacle (first (send obstacle :position)))
			(rs-obstacle (+ ls-obstacle (send obstacle :width))))
		      (unless (or (>= ls-proposed rs-obstacle)
		      		  (<= rs-proposed ls-obstacle))
			      (return t)))))


(gmode)
(defun m (a b) (send a :put-on b) (bottom))
(defun d () (display-blocks))
(d)
