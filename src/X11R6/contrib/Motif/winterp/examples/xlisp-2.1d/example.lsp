; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         example.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/example.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Object oriented programming example
; Author:       Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 03:02:33 1994 (Niels Mayer) npm@indeed
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

;; (unless (fboundp 'defclass) (load 'classes))
(require "xlisp-2.1d/classes")		;NPM: for WINTERP...

; Make the class ship and its instance variables be known

(defclass ship ((x 0) (y 0) (xv 0) (yv 0) (mass 0) (name 'unknown) 
	(captain 'unknown) (registry 'unknown)))

(defmethod ship :sail (time) 
	; the METHOD for sailing
	(princ (list "sailing for " time " hours\n"))
	   ; note that this form is expressed in terms of objects:  "self"
	   ; is bound to the object being talked to during the execution
	   ; of its message.  It can ask itself to do things.
	   (setf (send self :x)
	   	 (+  (send self :x) (* (send self :xv) time)))
	   ; This form performs a parallel action to the above, but more
	   ; efficiently, and in this instance, more clearly
	   (setq y (+ y (* yv time)))
	   ; Cute message for return value.  Tee Hee.
	   "Sailing, sailing, over the bountiful chow mein...")

; <a SHIP: #12345667> is not terribly instructive.  How about a more
; informative print routine?

(defmethod ship :print () (princ (list
				"SHIP NAME: " (send self :name) "\n"
				"REGISTRY: " (send self :registry) "\n"
				"CAPTAIN IS: " (send self :captain) "\n"
				"MASS IS: " (send self :mass) " TONNES\n"
				"CURRENT POSITION IS: " 
					(send self :x)	" X BY "
					(send self :y)	" Y\n"
				"SPEED IS: "
					(send self :xv)	" XV BY "
					(send self :yv)	" YV\n") ) )


; and an example object.

(definst ship Bounty :mass 50 
		     :name 'Bounty 
		     :registry 'England 
		     :captain 'Bligh)

(send Bounty :print)

(definst ship lollipop :mass (+ 10 20) :captain 'Temple :x 1000 :y 2000)

(send lollipop :print)

(definst ship hard :mass 1000 :captain 'Bozo :registry 'North-pole )

(send hard :print)
