; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xmu-init.lsp
; RCS:          $Header: $
; Description:  WINTERP-based Menu Server INITIALIZATION FILE
; Author:       Richard Hess, Consilium
; Created:      Sun Oct  6 00:04:34 1991
; Modified:     Sun Oct  6 00:05:17 1991 (Niels Mayer) mayer@hplnpm
; Language:     Lisp
; Package:      N/A
; Status:       X11r5 contrib tape release
;
; WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and David Betz not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and David Betz
; make no representations about the suitability of this software for any
; purpose. It is provided "as is" without express or implied warranty.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; +---------------------------------------------------------------------------
;  WHO:    Richard Hess                    CORP:   Consilium
;  TITLE:  Staff Engineer                  VOICE:  [415] 691-6342
;      [ X-SWAT Team:  Special Projects ]  USNAIL: 640 Clyde Court
;  UUCP:   ...!uunet!cimshop!rhess                 Mountain View, CA 94043
; +---------------------------------------------------------------------------

; initialization file for XLISP 2.1 & WINTERP

; define some macros
(defmacro defvar (sym &optional val)
  `(if (boundp ',sym) ,sym (setq ,sym ,val)))
(defmacro defparameter (sym val)
  `(setq ,sym ,val))
(defmacro defconstant (sym val)
  `(setq ,sym ,val))

; (makunbound sym) - make a symbol value be unbound
(defun makunbound (sym) (setf (symbol-value sym) '*unbound*) sym)

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

(setq *breakenable* nil)		; T allows entry into breakloop
(setq *tracenable* t)			; set this to T if you want to see a
					; backtrace on error.
(setq *gc-flag* t)			; we want to see garbage collection messages


;; --------------------------------------------------------------------[ new ]

(send WIDGET_CLASS :answer :get '(resource-name)
 '(
   (car (send self :GET_VALUES resource-name NIL))
   ))


;; ------------------------------------------------------------------[ rhess ]

(load "xmu-menu.lsp")		;; [ NEW ]:  menu server library... [ core ]
(load "xmu-cache.lsp")		;; [ NEW ]:  initialize the menu cache...
(load "gnu-hooks.lsp")		;; [ NEW ]:  GNU hooks for menu server...

;;; (setq *NUKE_WIDGET* (send TOP_LEVEL_SHELL_WIDGET_CLASS :new
;;;                           :XMN_TITLE            "XmuNuke"
;;;                           :XMN_ICON_NAME        "XmuNuke"
;;;                           :XMN_GEOMETRY         "+996+540"
;;;                           :XMN_MWM_FUNCTIONS     MWM_FUNC_MOVE
;;; ;;			  :XMN_MWM_FUNCTIONS    (logior MWM_FUNC_MINIMIZE
;;; ;;							MWM_FUNC_MOVE)
;;;                           ))
;;; 
;;; (setq *NEURO_W* (send XM_PUSH_BUTTON_WIDGET_CLASS
;;;                       :new :managed         *NUKE_WIDGET*
;;;                       :XMN_LABEL_STRING     "Neuromancer"
;;;                       :XMN_FONT_LIST        "-*-helvetica-bold-r-normal--12-*"
;;;                       :XMN_HIGHLIGHT_THICKNESS  0
;;;                       ))
;;; 
;;; (send *NEURO_W* :add_callback :XMN_ACTIVATE_CALLBACK '() '((exit)))
;;; 
;;; (send *NUKE_WIDGET* :REALIZE)

;; ----------------------------------------------------------------------<eof>
