; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         akalah.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/akalah.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Akalah Game
; Author:       Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 03:03:10 1994 (Niels Mayer) npm@indeed
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

; To play against the computer:
; (meplay #pits-per-side #pebbles-per-pit #search-depth #computer-plays-first)
;
; To have the computer play against itself:
; (play #pits-per-side #pebbles-per-pit #search-depth)


; The playing arena:
; 13    12 11 10 9  8  7
;	0  1  2  3  4  5     6 

; Pick up the pile of stones in a pit and seed them counterclockwise,
; except for the oponents kalah hole.
; If the last pebble lands in your own empty hole, pick up that pebble
;  and the one oposite of the opponents hole, and put them in ones kalah hole
; If the last pebble lands in your own kalah hole, play again
; If as a result of your move, you have no more pebbles, then your oponent
; takes all of his pebbles.
; The first player to get at greater than 50% of the pebbles wins.

(defun makelist (length contents)
	(if	(zerop length)
		'nil
		(cons contents (makelist (1- length) contents))))

(defconstant *maxvalue* 1000)
(defconstant *minvalue* (- *maxvalue*))
(defconstant *firsta* 0)
(defvar *enda*)
(defvar *endb*)
(defvar *moves*)
(defvar *firstb*)
(defvar *halfall*)
(defvar *board*)
(defvar *lasta*)
(defvar *lastb*)

(defmacro copy (list) `(append ,list 'nil))

(defmacro empty (position hole)	; empty out the given hole
	`(setf (nth ,hole ,position) 0))

(defmacro kalaholefn (whoseturn)   ; the scoring hole for the given player
	`(if ,whoseturn *endb* *enda*))

(defmacro opposholefn (hole)	; calculate the opposing hole
	 `(- *lastb* ,hole))

(defmacro ownsidep (hole whoseturn)
	`(if ,whoseturn (> ,hole *enda*) (< ,hole *firstb*)))

(defmacro prinb (x) `(dotimes (i ,x) (princ #\space)))

(defun search (startpos depth whoseturn)
   (if (zerop depth)
       (list startpos (evaluate startpos whoseturn))
       (let (bestval nextval bestpos succlist)
	    (setq succlist (successorsfn startpos whoseturn))
	    (when (> depth 1) (setq succlist (reorder succlist whoseturn)))
	    (setq 
	        beta    *maxvalue*
	        bestval *minvalue*
		bestpos (car succlist))
	    (dolist (this succlist)
	    	(when (wincheck this whoseturn)
		    (return-from search (list this *maxvalue*)))
		(setq nextval (- (alphabeta this
					    (- beta)
					    (- bestval)
					    (1- depth)
					    (not whoseturn))))
		(when (> nextval bestval)
		      (setq bestval nextval)
		      (setq bestpos this)))
	    (list bestpos bestval))))	; return value

(defun alphabeta (position alpha beta depth whoseturn)
    (if (zerop depth)
    	(evaluate position whoseturn)
	(let (bestval nextval succlist)
	    (setq succlist (successorsfn position whoseturn))
	    (when (> depth 1) (setq succlist (reorder succlist whoseturn)))
	    (setq bestval alpha)
	    (dolist (this succlist)
	    	(when (wincheck this whoseturn)
		      (return-from alphabeta *maxvalue*))
		(setq nextval (- (alphabeta this
					    (- beta)
					    (- bestval)
					    (1- depth)
					    (not whoseturn))))
		(when (> nextval bestval) (setq bestval nextval))
		(when (<= beta bestval) (return-from alphabeta bestval)))
	    bestval)))

(defun successorsfn (position whoseturn 
			&aux 
			(picuphole (1- (if whoseturn *firstb* *firsta*)))
			succlist 
			succ 
			stones 
			disthole 
			lasthole)
   (dotimes (dummy *enda*)
	    (when (not (zerop (nth (setq picuphole (1+ picuphole)) position)))
		  (setq	succ (copy position))
		  (setf (nth *moves* succ) 
			(cons (1+ dummy) (nth *moves* succ)))
		  (setq stones (nth picuphole succ)) ; stones in this pit
		  (empty succ picuphole)
		  (setq disthole picuphole)
		  (dotimes (dummy2 stones) ; drop in successive holes except
		  			   ; opponents kalah hole
		  	   (setq disthole (nextdistholefn disthole whoseturn))
			   (dropin succ disthole 1))
		  (setq lasthole disthole)
		  (cond ((allownzerok succ whoseturn) ; all played out
		  	 (opptakesallfn succ whoseturn))
			((eq lasthole (kalaholefn whoseturn)) ; last in kalah
			 (setq succ (successorsfn succ whoseturn)))
			((and (eq (nth lasthole succ) 1) ; last into own empty
			      (> (nth (opposholefn lasthole) succ) 0)
			      (ownsidep lasthole whoseturn))
			 (dropin succ 
			 	 (kalaholefn whoseturn)
				 (1+ (nth (opposholefn lasthole) succ)))
			 (empty succ lasthole)
			 (empty succ (opposholefn lasthole))
			 (when (allownzerok succ whoseturn)
			       (opptakesallfn succ whoseturn))))
		  (setq succlist (nconc (preparelisfn succ) succlist))))
    (if (null succlist)
    	(progn	(setq succ (copy position))
		(opptakesallfn succ whoseturn)
		(list succ))
	succlist))

(defun dropin (position hole number) 
	(setf (nth hole position)
	      (+ number
		 (nth hole position))))

(defun nextdistholefn (disthole whoseturn)
	(cond	((and whoseturn (eql disthole *lasta*)) *firstb*) ; skip own pile
		((and (not whoseturn) (eql disthole *lastb*)) *firsta*)
		((< disthole *endb*) (1+ disthole))
		(t *firsta*)))

(defun preparelisfn (x)
	(if (atom (car x))
	    (list x)
	    (unimbedfn x)))

(defun reorder (poslist whoseturn)
       (mapcar #'car (sort (mapcar #'(lambda (x) 
					     (cons x
						   (evaluate x whoseturn)))
				   poslist)
			   #'(lambda (x y) (>= (cdr x) (cdr y))))))
				

(defun wincheck (position whoseturn)
	(> (nth (kalaholefn whoseturn) position) *halfall*))

(defun evaluate (position whoseturn) ; assign the value to the position
				     ; could obviously use work
	(let ((ownkala (nth (kalaholefn whoseturn) position))
	      (oppkala (nth (kalaholefn (not whoseturn)) position)))
	     (cond ((> ownkala *halfall*) *maxvalue*)
	     	   ((> oppkala *halfall*) *minvalue*)
		   (t (- ownkala oppkala)))))

(defun printins ()
       (terpri)
       (format t "Select Hole:~%    ")
       (dotimes (i *enda*)
		(let ((n (- *enda* i)))
		     (prin1 n)
		     (prinb (if (> 10 n) 2 1))
		     )))

(defun listmoves (position &aux (l (nth *moves* position)))
       (setf (nth *moves* position) nil)
       (terpri)
       (format t "Moves: ")
       (mapc #'(lambda (x) (format t "~s " x)) (reverse l)))

(defun printpos (position)
	(let ((minprint (reverse (subseq position 0 *firstb*)))
	      (maxprint (subseq position *firstb* *moves*)))
	     (terpri)
	     (prin1 (car minprint))
	     (prinb (if (> 10 (car minprint)) 3 2))
	     (setq minprint (cdr minprint))
	     (do ((list minprint (cdr list)))
	         ((null list))
		 (prin1 (car list))
		 (prinb (if (> 10 (car list)) 2 1)))
	     (terpri)
	     (prinb 4)
	     (do ((list maxprint (cdr list)))
	         ((null (cdr list)))
		 (prin1 (car list))
		 (prinb (if (> 10 (car list)) 2 1)))
	     (prinb 2)
	     (prin1 (car (last maxprint)))
	     (terpri)
	     (terpri)))

(defun allownzerok (position whoseturn)
	(zerop (apply #'+ (if whoseturn 
			      (subseq position *firstb* *endb*)
			      (subseq position *firsta* *enda*)))))


(defun opptakesallfn (position whoseturn)
	(dropin position
		(kalaholefn (not whoseturn))
		(apply #'+ (if whoseturn 
			       (subseq position *firsta* *enda*)
			       (subseq position *firstb* *endb*))))
	(do	((count *enda* (1- count))
		 (hole (if whoseturn *firsta* *firstb*) (1+ hole)))
		((zerop count))
		(empty position hole)))

(defun nextmove (depth whoseturn)
       (terpri)
       (setq *board* (search (car *board*) depth whoseturn))
       (listmoves (car *board*))
       (printpos (car *board*))
       (print (cdr *board*))
       (terpri))

(defun unimbedfn (poslist)
	(do	((list poslist (cdr list))
		 (result nil (if (atom (caar list))
		 		 (cons (car list) result)
				 (append (unimbedfn (car list)) result))))
	        ((null list) result)))

(defun initialize (holes pebbles 
			 &aux (temp (makelist holes pebbles)))
        ; initialize the playing area
	(setq *enda* holes
	      *endb* (+ holes holes 1)
	      *lasta* (1- *enda* )
	      *firstb* (1+ *enda*)
	      *lastb* (1- *endb*)
	      *moves* (1+ *endb*)
	      *halfall* (* holes pebbles)
	      *board* (list (append temp
				    (list 0)
				    temp
				    (list 0)
				    '(nil))
			    0)))

(defun play (holes pebbles depth) ; play the game
	(initialize holes pebbles)
	(do ((whoseturn nil (not whoseturn))
	     (scorea 0 (nth *enda* (car *board*)))
	     (scoreb 0 (nth *endb* (car *board*))))
	    ((or (> scorea *halfall*)
		 (> scoreb *halfall*)
		 (= scorea scoreb *halfall*)))
	    (nextmove depth whoseturn)))

(defun meplay (holes pebbles depth computer-first) 
  (prog (picuphole)
	(initialize holes pebbles)
	(when computer-first (setq succ (copy (car *board*)))
	 	 (go y))
n	(setq succ (copy (car *board*)))
	(printins)
	(printpos succ)
	(when (> (nth *enda* succ) *halfall*) 
	      (format t "You win!!~%")
	      (return t))	; win for side a
	(when (> (nth *endb* succ) *halfall*) 
	      (format t "I win!!!~%")
	      (return nil)) ; win for side b
	(when (= (nth *enda* succ) (nth *endb* succ) *halfall*)
	      (format t "We tie???~%")
	      (return nil))
x	(princ "Hole? ") (setq picuphole (read))
	(when (or (not (numberp picuphole))
		  (>= picuphole *firstb*)
		  (> 1 picuphole)
		  (zerop (setq stones 
			       (nth (setq picuphole (1- picuphole)) succ))))
	    (go x))
	(empty succ picuphole)
	(setq disthole picuphole)
	(dotimes (dummy stones)
	         (dropin succ 
			 (setq disthole (nextdistholefn disthole nil)) 1))
	(setq lasthole disthole)
	(cond	((allownzerok succ nil)
		 (opptakesallfn succ nil)
		 (setq *board* (list succ 0))
		 (go n))
		((eql lasthole *enda*)
		 (setq *board* (list succ 0))
		 (go n))
		((and (eql (nth lasthole succ) 1)
		      (> (nth (opposholefn lasthole) succ) 0)
		      (> *enda* lasthole))
		 (dropin succ *enda* (1+ (nth (opposholefn lasthole) succ)))
		 (empty succ lasthole)
		 (empty succ (opposholefn lasthole))
		 (when (allownzerok succ nil)
		       (opptakesallfn succ nil)
		       (setq *board* (list succ 0))
		       (go n))))
	(printpos succ)
y	(format t "(Computer is thinking...)~%")
	(setq *board* (search succ depth t))
	(listmoves (car *board*))
	(go n)))

