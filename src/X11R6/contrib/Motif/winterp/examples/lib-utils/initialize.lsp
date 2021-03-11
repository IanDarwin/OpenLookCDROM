; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         initialize.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-utils/RCS/initialize.lsp,v 2.1 1994/06/06 14:54:46 npm Exp $
; Description:  INITIALIZE WINTERP AND XLISP DEFAULT ENVIRONMENT
; Author:       David Betz, Tom Almy, Luke Tierney, Niels Mayer, and others
; Created:      Mon Nov 20 18:13:23 1989
; Modified:     Mon Jun  6 03:03:20 1994 (Niels Mayer) npm@indeed
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  INITIALIZE XLISP-PLUS DEFAULT ENVIRONMENT ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;            from xlisp-2.1d/init.lsp        ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun strcat (&rest str)	;; Backwards compatibility
       (apply #'concatenate 'string str))


; (fmakunbound sym) - make a symbol function be unbound
(defun fmakunbound (sym) (setf (symbol-function sym) '*unbound*) sym)

; (mapcan fun list [ list ]...)
(defmacro mapcan (&rest args) `(apply #'nconc (mapcar ,@args)))

; (mapcon fun list [ list ]...)
(defmacro mapcon (&rest args) `(apply #'nconc (maplist ,@args)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;  WINTERP INITIALIZATION  ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *breakenable* nil)		; T allows entry into breakloop
(setq *tracenable* t)			; set this to T if you want to see a
					; backtrace on error.
(setq *gc-flag* nil)			; IN WINTERP, DON'T DISPLAY GC MESSAGES DUE TO XLISP BUG
					; WHICH CAUSES GC-MSG OUTPUT TO CORRUPT READER INPUT
					; (this will get fixed eventually, perhaps when I upgrade
					; from xlisp 2.1c to xlisp 2.1e).

;;;
;;; Add a method on the widget metaclass WIDGET_CLASS. The method allow use of
;;; simpler notation for doing XtGetValues() for a single resource.
;;; (send <widget-class> :get :<resource-name>) ==> returns the resource value.
;;;
(send WIDGET_CLASS :answer :get '(resource-name)
      '(
	(car (send self :GET_VALUES resource-name NIL))
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ***********   P E R S O N A L  C U S T O M I Z A T I O N S  ************ ;;
;;                                                                          ;;
;; The following are defaults for variables used by various WINTERP example ;;
;; programs. Rather than editing/customizing these here, you should copy    ;;
;; .../examples/00.winterp.lsp to ~/.winterp and customize your personal    ;;
;; preferences in your ~/.winterp file.                                     ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "lib-utils/unixstuf")		;define unix:get-uname, and other unixisms.

;;;
;;; set up editor used by "$EDITOR" button in w_ctrlpnl.lsp, dircmp.lsp,
;;; grep-br.lsp, etc. If you don't set this, then environment variable
;;; $EDITOR will be used.
;;;

; ;; For those using Andy Norman's emacs gnuserv/gnuclient/gnudoit package...
; (defvar *SYSTEM-EDITOR* "gnuclient -q")
;
; ;; For those using emacs' standard emacsserver/emacsclient package...
; (defvar *SYSTEM-EDITOR* "emacsclient") 
;
;; The lowest common denominator external viewer option for Unix -- VI (puke!)
(defvar *SYSTEM-EDITOR* "xterm -e vi") 

;;;
;;; This variable holds the directory pathname (including trailing '/') of the
;;; X11 bitmaps directory, which is typically "/usr/include/X11/bitmaps/"
;;;

(defvar *X11-BITMAPS-DIRECTORY*
  (cond
   ((equal (unix:get-uname) "SunOS")
    "/usr/openwin/include/X11/bitmaps/")
   (t
    "/usr/include/X11/bitmaps/")	;default location
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-utils/initialize")

