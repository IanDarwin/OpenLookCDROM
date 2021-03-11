; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_object-browser.lsp
; RCS:          $Header: $
; Description:  Object_Browser_Widget_Class and Methods
; Author:       Niels Mayer, HPLabs
; Created:      Fri Aug 28 12:55:16 1992
; Modified:     Fri Aug 28 12:55:37 1992 (Niels Mayer) mayer@hplmmws
; Language:     Lisp
; Package:      N/A
; Status:       X11r5 contrib tape release
;
; WINTERP Copyright 1989-1992 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and Niels Mayer not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and Niels Mayer
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object_Browser_Widget_Class --> String_Browser_Widget_Class --> XM_LIST_WIDGET_CLASS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; This class is mostly the same as String_Browser_Widget_Class, except that you give
;; :set_browser_items method a list of OBJECTs, rather than STRINGs.
;;
;; The OBJECTs may be of different classes, but the must have a :DISPLAY_STRING
;; method defined.
;;
(defvar Object_Browser_Widget_Class
#|
  nil)
  (setq Object_Browser_Widget_Class
|#
  (send Class :new
	'()				;new instance vars
	'()				;no class vars
	String_Browser_Widget_Class))	;Object_Browser_Widget_Class-->String_Browser_Widget_Class-->XM_LIST_WIDGET_CLASS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We add a method to set the items browsed by the list browser
;; and set the 'items' instance variable.
;;
;; (send <Object_Browser_Widget_Class> :set_browser_items <items_list>)
;; <items_list> is a list of arbitrary OBJECTS responding to :display_string method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Object_Browser_Widget_Class :answer :SET_BROWSER_ITEMS
      '(items_list)
      '(
	(let* (
	       (items_end_idx (length items_list))
	       (display_items (make-array items_end_idx)))

	  (if (= 0 items_end_idx)	;if items_list empty
	      (setq items nil)		;reset 'items' ivar to init value
	    (progn			;else setup 'items' and 'display_items'
	      ;; initialize the 'items' instance variable so that it
	      ;; holds all the BROWSER_OBJECTs passed in <items_list>
	      (setq items (make-array items_end_idx)) ;create the array
	      (do (			;copy elts from list to array
		   (i    0          (1+ i))
		   (elts items_list (cdr elts)))
		  ;; loop till no more elts
		  ((null elts))
		  ;; loop body
		  (setf (aref items i) (car elts))
		  (setf (aref display_items i) (send (car elts) :display_string))
		  )
	      )
	    )

	  ;; tell the widget about the new browser items to display
	  (send-super :set_values
		      :XMN_ITEMS display_items
		      :XMN_ITEM_COUNT items_end_idx
		      )
	  )

	;; clear out any previous selection...
	(send-super :clear_selection)
	))
