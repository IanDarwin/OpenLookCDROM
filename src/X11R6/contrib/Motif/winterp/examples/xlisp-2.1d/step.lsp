; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         step.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/step.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  single-step debugger
; Author:       Jonathan Engdahl
; Created:      Jan-25-1987
; Modified:     Mon Jun  6 03:01:05 1994 (Niels Mayer) npm@indeed
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

;Title:  step.lsp
;Author: Jonathan Engdahl (jengdahl on BIX)
;Date:   Jan-25-1987

;This file contains a simple Lisp single-step debugger. It
;started as an implementation of the "hook" example in chapter 20
;of Steele's "Common Lisp". This version was brought up on Xlisp 1.7
;for the Amiga, and then on VAXLISP.

;To invoke: (step (whatever-form with args))
;For each list (interpreted function call), the stepper prints the
;environment and the list, then enters a read-eval-print loop
;At this point the available commands are:

;    (a list)<CR> - evaluate the list in the current environment,
;                   print the result, and repeat.                 
;    <CR> - step into the called function
;    anything_else<CR> - step over the called function.

;If the stepper comes to a form that is not a list it prints the form 
;and the value, and continues on without stopping.

;Note that stepper commands are executed in the current environment.
;Since this is the case, the stepper commands can change the current
;environment. For example, a SETF will change an environment variable
;and thus can alter the course of execution.


;set the representation for an input #/newline
;the value, notation, and data type of newline may be implementation dependent
(setf newline #\newline)   ;for XLISP
;(setf newline 10)         ;for VAXLISP

;define a C-like iterator.
(defmacro while (test &rest forms) `(do () ((not ,test)) ,@forms))

;create the nesting level counter.
(defparameter *hooklevel* 0)

;this macro invokes the stepper.
;for VAXLISP you better rename this to xstep or something, lest
;defun say nasty things to you about step already being defined

(defmacro step (form &aux val)
     `(progn
       (step-flush)                  ;get rid of garbage on the line
       (setf *hooklevel* 0)          ;init nesting counter
       (princ *hooklevel*)           ;print the form
       (princ "  form: ")
       (prin1 ',form)
       (terpri)
       (setf val (evalhook ',form    ;eval, and kick off stepper
                           #'eval-hook-function
                           nil
                           nil))
       (princ *hooklevel*)           ;print returned value
       (princ " value: ")
       (prin1 val)
       (terpri)
       val))                         ;and return it


;this is the substitute "eval" routine that gets control when
;a user form is evaluated during stepping.

(defun eval-hook-function (form env &aux val f1)
     (setf *hooklevel* (+ *hooklevel* 1))    ;inc the nesting level
     (cond ((consp form)                     ;if interpreted function 
            (step-spaces *hooklevel*)        ;print the environment
            (princ *hooklevel*)
            (princ "    env: ")
            (prin1 env)
            (terpri)
            (step-spaces *hooklevel*)        ;then the form
            (princ *hooklevel*)
            (princ "   form: ")
            (prin1 form)
            (princ " ")
            (while (eql (peek-char) #\( )    ;while a form is typed           
                   (setf f1 (read))          ;read a form
                   (step-flush)              ;get rid of junk
                   (step-spaces *hooklevel*) ;print out result
                   (princ *hooklevel*)
                   (princ " result: ")       ;which is evaled in env
                   (prin1 (evalhook f1 nil nil env))
                   (princ " "))   
            (cond ((eql (read-char) newline) ;if <cr> then step into
                   (setf val (evalhook form
                                       #'eval-hook-function
                                       nil
                                       env)))
                  (t (step-flush)            ;else step over
                     (setf val (evalhook form nil nil env)))))
           (t (step-spaces *hooklevel*)      ;if not interpreted func
              (princ *hooklevel*)            ;print the form
              (princ "   form: ")
              (prin1 form)
              (terpri)
              (setf val (evalhook form nil nil env)))) ;eval it
     (step-spaces *hooklevel*)               ;in either case
     (princ *hooklevel*)                     ;print the result
     (princ "  value: ")
     (prin1 val)
     (terpri)
     (setf *hooklevel* (- *hooklevel* 1))    ;decrement level
     val)                                    ;and return the value


;a non-recursive fn to print spaces (not as elegant, easier on the gc)
(defun step-spaces (n) (while (> n 0) (princ " ") (setf n (- n 1))))
     
;and one to clear the input buffer
(defun step-flush () (while (not (eql (read-char) newline))))
