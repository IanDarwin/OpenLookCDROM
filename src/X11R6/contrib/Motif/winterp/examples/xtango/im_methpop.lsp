; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         im_methpop.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/im_methpop.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  popup-menu-of-methods-of-object -- pops up a menu of the
;		methods available on the class of the given instance. This is
;		a pretty random way of inspecting the methods of an image
;		given that the popup menu entries don't actually do anything.
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 03:30:13 1994 (Niels Mayer) npm@indeed
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

(require "xlisp-2.1d/common")		;define FILL

(defun methods-from-class (x)
  (aref (generic x) 1)
  )

(defun superclass-from-class (x)
  (aref (generic x) 5)
  )

(defun popup-menu-of-methods-of-object (inst parent-w xevent)
  (let ((methods NIL) (buttons NIL) (popup_menu_w NIL))
    (setq methods 
	  (do* ((c
		 (send inst :class)
		 (superclass-from-class c))
		(res 
		 (methods-from-class c))
		)
	      ((null c)
	       res)
	    (setq res (append (methods-from-class c) res))
	    ))
    (setq buttons
	  (map 'list #'(lambda (x) (format NIL "~A" (car x))) methods))
    (setq popup_menu_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_popup_menu
		"image-popup" parent-w
		:XMN_PACKING		:pack_column
		:XMN_NUM_COLUMNS	5
		:XMN_BUTTON_COUNT	(length buttons)
		:XMN_BUTTON_TYPE	(fill (make-array (length buttons)) :RADIOBUTTON)
		:XMN_BUTTONS		buttons
		))
    (send (send popup_menu_w :parent) :add_callback :XMN_POPDOWN_CALLBACK '(CALLBACK_WIDGET)
	  '(
;;; (format T "popped down ~A\n" (send CALLBACK_WIDGET :name))
	    ;; I do the destroy in a 0-length-timeout to ensure that
	    ;; the popdown callback has "returned" prior to destroying 
	    ;; the widget. Otherwise I have SUPERSTITION that Motif
	    ;; might destroy the menu before the menu grab is finished.
	    ;; (This could basically render your X-server inoperative
	    ;; until WINTERP gets killed).
	    (xt_add_timeout 0 '( (send CALLBACK_WIDGET :destroy) ))
	    ))
    (send popup_menu_w :menu_position xevent)
    (send popup_menu_w :manage)
;;; (send popup_menu_w :add_callback :XMN_DESTROY_CALLBACK '(CALLBACK_WIDGET)
;;;      '(
;;;        (format T "destroyed ~A\n" CALLBACK_WIDGET)
;;;        ))
;;; (send (send popup_menu_w :parent) :add_callback :XMN_POPUP_CALLBACK '(CALLBACK_WIDGET)
;;;      '(
;;;        (format T "popped up ~A\n" (send CALLBACK_WIDGET :name))
;;;        ))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/im_methpop")
