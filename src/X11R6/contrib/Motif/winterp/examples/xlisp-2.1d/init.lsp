; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         init.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/init.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  initialization file for XLISP-PLUS 2.1c
; Author:       David Betz, Tom Almy, and others
; Created:      
; Modified:     Mon Jun  6 03:02:03 1994 (Niels Mayer) npm@indeed
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

; initialization file for XLISP-PLUS 2.1c

(princ "XLISP-PLUS contains contributed code by:
Tom Almy, Mikael Pettersson, Neal Holtz, Johnny Greenblatt, Ken Whedbee,
Blake McBride, and Pete Yadlowsky.
Portions Copyright (c) 1988, Luke Tierney.\n")

(defun strcat (&rest str)	;; Backwards compatibility
       (apply #'concatenate 'string str))


; (fmakunbound sym) - make a symbol function be unbound
(defun fmakunbound (sym) (setf (symbol-function sym) '*unbound*) sym)

; (mapcan fun list [ list ]...)
; (defmacro mapcan (&rest args) `(apply #'nconc (mapcar ,@args)))

; (mapcon fun list [ list ]...)
; (defmacro mapcon (&rest args) `(apply #'nconc (maplist ,@args)))

; (set-macro-character ch fun [ tflag ])
(defun set-macro-character (ch fun &optional tflag)
    (setf (aref *readtable* (char-int ch))
          (cons (if tflag :tmacro :nmacro) fun))
    t)

; (get-macro-character ch)
(defun get-macro-character (ch)
  (if (consp (aref *readtable* (char-int ch)))
    (cdr (aref *readtable* (char-int ch)))
    nil))

; (savefun fun) - save a function definition to a file
(defmacro savefun (fun)
  `(let* ((fname (strcat (symbol-name ',fun) ".lsp"))
          (fval (get-lambda-expression (symbol-function ',fun)))
          (fp (open fname :direction :output)))
     (cond (fp (print (cons (if (eq (car fval) 'lambda)
                                'defun
                                'defmacro)
                            (cons ',fun (cdr fval))) fp)
               (close fp)
               fname)
           (t nil))))

; (debug) - enable debug breaks
(defun debug ()
       (setq *breakenable* t))

; (nodebug) - disable debug breaks
(defun nodebug ()
       (setq *breakenable* nil))

; initialize to enable breaks but no trace back
(setq *breakenable* t *tracenable* nil)


; macros get displaced with expansion
; Good feature -- but be warned that it creates self modifying code!
(setq *displace-macros* t)

; Enable the following to have DOS do the line editing
; (setq  *dos-input* t)

;; Select one of these three choices
;; Other modes will not read in other standard lsp files


; print in upper case, case insensitive input
  (setq *print-case* :upcase *readtable-case* :upcase)

; print in lower case
; (setq *print-case* :downcase *readtable-case* :upcase)

; case sensitive, lowercase and uppercase swapped (favors lower case)
; (setq *print-case* :downcase *readtable-case* :invert)


;; Define Class and Object to be class and object when in case sensitive
;; mode

(when (eq *readtable-case* :invert)
      (defconstant Class class)
      (defconstant Object object))
