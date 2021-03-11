; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         ifthen.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/ifthen.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  If then rules - mini expert system from Ch. 18 of Winston&Horn
;		Lisp book
; Author:       Winston&Horn, Tom Almy and/or David Betz
; Created:      Mon Jun  6 01:54:02 1994
; Modified:     Mon Jun  6 03:02:10 1994 (Niels Mayer) npm@indeed
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

; -*-Lisp-*-
;
; If then rules - mini expert from Ch. 18 of Winston and Horn
; Written using recursion without progs
; Added function 'how' to explain deductions
;
; Use:
;	After loading type (deduce). It will make all the deductions
;	given the list fact. If you want to know how it deduced something
;	type (how '(a deduction)) for example (how '(animal is tiger))
;	and so on.



; rules data base

(setq rules
      '((rule identify1
	      (if (animal has hair))
	      (then (animal is mammal)))
	(rule identify2
	      (if (animal gives milk))
	      (then (animal is mammal)))
	(rule identify3
	      (if (animal has feathers))
	      (then (animal is bird)))
	(rule identify4
	      (if (animal flies)
		  (animal lays eggs))
	      (then (animal is bird)))
	(rule identify5
	      (if (animal eats meat))
	      (then (animal is carnivore)))
	(rule identify6
	      (if (animal has pointed teeth)
		  (animal has claws)
		  (animal has forward eyes))
	      (then (animal is carnivore)))
	(rule identify7
	      (if (animal is mammal)
		  (animal has hoofs))
	      (then (animal is ungulate)))
	(rule identify8
	      (if (animal is mammal)
		  (animal chews cud))
	      (then (animal is ungulate)
		    (even toed)))
	(rule identify9
	      (if (animal is mammal)
		  (animal is carnivore)
		  (animal has tawny color)
		  (animal has dark spots))
	      (then (animal is cheetah)))
	(rule identify10
	      (if (animal is mammal)
		  (animal is carnivore)
		  (animal has tawny color)
		  (animal has black stripes))
	      (then (animal is tiger)))
	(rule identify11
	      (if (animal is ungulate)
		  (animal has long neck)
		  (animal has long legs)
		  (animal has dark spots))
	      (then (animal is giraffe)))
	(rule identify12
	      (if (animal is ungulate)
		  (animal has black stripes))
	      (then (animal is zebra)))
	(rule identify13
	      (if (animal is bird)
		  (animal does not fly)
		  (animal has long neck)
		  (animal has long legs)
		  (animal is black and white))
	      (then (animal is ostrich)))
	(rule identify14
	      (if (animal is bird)
		  (animal does not fly)
		  (animal swims)
		  (animal is black and white))
	      (then (animal is penguin)))
	(rule identify15
	      (if (animal is bird)
		  (animal flys well))
	      (then (animal is albatross)))))
; utility functions
(defun squash(s)
       (cond ((null s) ())
	     ((atom s) (list s))
	     (t (append (squash (car s))
			(squash (cdr s))))))

(defun p(s)
       (princ (squash s)))

; functions

; function to see if an item is a member of a list

(defun member(item list)
       (cond((null list) ())	; return nil on end of list
	    ((equal item (car list)) list) ; found
	    (t (member item (cdr list))))) ; otherwise try rest of list

; put a new fact into the facts data base if it is not already there

(defun remember(newfact)
       (cond((member newfact facts) ())	; if present do nothing
	    (t ( setq facts (cons newfact facts)) newfact)))

; is a fact there in the facts data base

(defun recall(afact)
       (cond ((member afact facts) afact)	; it is here
	     (t ())))				; no it is'nt

; given a rule check if all the if parts are confirmed by the facts data base

(defun testif(iflist)
       (cond((null iflist) t)	; all satisfied
	    ((recall (car iflist)) (testif (cdr iflist))) ; keep searching
	    					          ; if one is ok
	    (t ())))					; not in facts DB

; add the then parts of the rules which can be added to the facts DB
; return the ones that are added

(defun usethen(thenlist addlist)
       (cond ((null thenlist) addlist) ; all exhausted
	     ((remember (car thenlist))
	     (usethen (cdr thenlist) (cons (car thenlist) addlist)))
	     (t (usethen (cdr thenlist) addlist))))

; try a rule
; return t only if all the if parts are satisfied by the facts data base
; and at lest one then ( conclusion ) is added to the facts data base

(defun tryrule(rule &aux ifrules thenlist addlist)
       (setq ifrules (cdr(car(cdr(cdr rule)))))
       (setq thenlist (cdr(car(cdr(cdr(cdr rule))))))
       (setq addlist '())
       (cond (( testif ifrules)
	      (cond ((setq addlist (usethen thenlist addlist))
		     (p (list "Rule " (car(cdr rule)) "\n\tDeduced " addlist "\n\n"))
		     (setq ruleused (cons rule ruleused))
		     t)
		    (t ())))
	     (t ())))

; step through one iteration if the forward search
; looking for rules that can be deduced from the present fact data base

(defun stepforward( rulelist)
       (cond((null rulelist) ())	; all done
	    ((tryrule (car rulelist)) t)
	    ( t (stepforward(cdr rulelist)))))

; stepforward until you cannot go any further

(defun deduce()
      (cond((stepforward rules) (deduce))
	   (t t)))

; function to answer if a fact was used to come to a certain conclusion
; uses the ruleused list cons'ed by tryrule to answer

(defun usedp(rule)
       (cond ((member rule ruleused) t)	; it has been used
	     (t () )))			; no it hasnt

; function to answer how a fact was deduced

(defun how(fact)
       (how2 fact ruleused nil))

(defun how2(fact rulist found)
       (cond ((null rulist)	; if the rule list exhausted
	      (cond (found t)   ; already answered the question return t
		    ((recall fact) (p (list fact " was a given fact\n")) t) ;known fact
		    (t (p (list fact " -- not a fact!\n")) ())))
	      
	      ((member fact (thenpart (car rulist))) 	; if rulist not empty
	       (setq found t)	; and fact belongs to the then part of a rule
	       (p (list fact " was deduced because the following were true\n"))
	       (printifs (car rulist))
	       (how2 fact (cdr rulist) found))
	      (t (how2 fact (cdr rulist) found))))

; function to return the then part of a rule

(defun thenpart(rule)
       (cdr(car(cdr(cdr(cdr rule))))))

; function to print the if part of a given rule

(defun printifs(rule)
       (pifs (cdr(car(cdr(cdr rule))))))

(defun pifs(l)
	(cond ((null l) ())
	      (t (p (list "\t" (car l) "\n"))
		 (pifs (cdr l)))))


; initial facts data base
; Uncomment one or make up your own
; Then run 'deduce' to find deductions
; Run 'how' to find out how it came to a certain deduction

;(setq facts
;      '((animal has dark spots)
;	(animal has tawny color)
;	(animal eats meat)
;	(animal has hair)))

(setq facts
      '((animal has hair)
	(animal has pointed teeth)
	(animal has black stripes)
	(animal has claws)
	(animal has forward eyes)
	(animal has tawny color)))


(setq rl1
      	'(rule identify14
	      (if (animal is bird)
		  (animal does not fly)
		  (animal swims)
		  (animal is black and white))
	      (then (animal is penguin))))

(setq rl2
        '(rule identify10
	      (if (animal is mammal)
		  (animal is carnivore)
		  (animal has tawny color)
		  (animal has black stripes))
	      (then (animal is tiger))))

; Initialization
(expand 10)
(setq ruleused nil)
