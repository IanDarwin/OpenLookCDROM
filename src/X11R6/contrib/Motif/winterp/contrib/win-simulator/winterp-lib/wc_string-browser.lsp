; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_string-browser.lsp
; RCS:          $Header: $
; Description:  String_Browser_Widget_Class and Methods
; Author:       Niels Mayer, HPLabs
; Created:      Fri Aug 28 12:51:40 1992
; Modified:     Sat Sep 18 03:56:31 1993 (Niels Mayer) mayer@hplnpm
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
;;;;;;;;;;;;  String_Browser_Widget_Class --> XM_LIST_WIDGET_CLASS  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Make a subclass of XM_LIST_WIDGET_CLASS which holds an additional
;; instance variable 'items'. 'items' is an array of arbitrary objects
;; (BROWSER_OBJECT) to be displayed in a browser made from the list widget.
;;
;;
(defvar String_Browser_Widget_Class
  (send Class :new
	'(items selected_pos)		;new instance vars
	'()				;no class vars
	XM_LIST_WIDGET_CLASS))		;superclass

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :ISNEW
      '(&rest args)
      '(
	;; initialize the instance variables
	(setq items nil)
	(setq selected_pos nil)

	;; initialize the widget...
	(apply 'send-super `(:isnew
			     ,@args
			     :XMN_SELECTION_POLICY :browse_select
			     ))

	;; add a callback to set selected_pos instance var.
	(send-super :add_callback :XMN_BROWSE_SELECTION_CALLBACK '(CALLBACK_ITEM_POSITION)
		    '(
		      (setq selected_pos CALLBACK_ITEM_POSITION)
		      )
		    )

	;;
	;; ???Setting these at create-time generates the following warning:
	;; "Warning: Actions not found: ListBeginSelect, ListEndSelect, ListKbdActivate"
	;;
;;;	(send-super :set_values
;;;	    :XMN_TRANSLATIONS	"#override \
	(send-super :OVERRIDE_TRANSLATIONS
	       "<Btn2Down>:   	ListBeginSelect() \
		<Btn2Up>:	ListEndSelect() ListKbdActivate() \
		Ctrl<Key>N:	Lisp(send ACTION_WIDGET :goto_next) \
		<Key>N:		Lisp(send ACTION_WIDGET :goto_next) \
		<Key>osfDown:	Lisp(send ACTION_WIDGET :goto_next) \
		Ctrl<Key>P:	Lisp(send ACTION_WIDGET :goto_prev) \
		<Key>P:		Lisp(send ACTION_WIDGET :goto_prev) \
		<Key>osfUp:	Lisp(send ACTION_WIDGET :goto_prev)"
	    )

	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We add a method to set the items browsed by the list browser
;; and set the 'items' instance variable.
;;
;; (send <String_Browser_Widget_Class_inst> :set_browser_items <items_list>)
;; <items_list> is a list of STRINGs...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :SET_BROWSER_ITEMS
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
		  (setf (aref display_items i) (car elts))
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
	(send self :clear_selection)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieves object at selected position, else NIL. 'selected_pos' is set by
;; :XMN_BROWSE_SELECTION_CALLBACK in :ISNEW method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :GET_SELECTED_ITEM
      '()
      '(
	(if selected_pos
	    (aref items (1- selected_pos))
	  NIL)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clears the selection from the browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :CLEAR_SELECTION
      '()
      '(
	;; clear out any previous selection...
	(setq selected_pos nil)
	(send-super :deselect_all_items)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removes object from selected position.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :REMOVE_SELECTED_ITEM
      '()
      '(
	(if selected_pos
	    (progn
	      (setq items (array-delete-pos items (1- selected_pos)))
	      (send-super :delete_pos selected_pos)
	      (send self :clear_selection)
	      ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select next item in list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :GOTO_NEXT
      '()
      '(
	(if items			;trap empty list...
	    (progn
	      ;; ensure that the position remains valid
	      (if (not (integerp selected_pos)) ;if no current selection,
		  (setq selected_pos 1)	;then start at beginning of list
		(if (>= selected_pos (length items))
		    (X_BELL)		;SIGNAL ERROR -- BEEP
		  (setq selected_pos (1+ selected_pos))
		  )
		)

	      ;; select the next position
	      (send-super :select_pos selected_pos t)

	      ;; make sure the item is visible
	      (let (num-br-visible num-br-items)
		(send self :get_values
		      :XMN_ITEM_COUNT 'num-br-items
		      :XMN_VISIBLE_ITEM_COUNT 'num-br-visible)
		(send self :set_bottom_pos
		      (min num-br-items (+ selected_pos (/ num-br-visible 2))))
		)
	      ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select previous item in list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :GOTO_PREV
      '()
      '(
	(if items			;trap empty list...
	    (progn
	      ;; ensure that the position remains valid
	      (if (not (integerp selected_pos))
		  (setq selected_pos 1)	;if no current selection, start at beginning.
		(if (= selected_pos 1)
		    (X_BELL)		;SIGNAL ERROR -- BEEP
		  (setq selected_pos (1- selected_pos))
		  )
		)

	      ;; select the prev position
	      (send-super :select_pos selected_pos t)

	      ;; make sure the item is visible
	      (let (visible-items)
		(send self :get_values :XMN_VISIBLE_ITEM_COUNT 'visible-items)
		(send self :set_pos (max 1 (- selected_pos (/ visible-items 2))))
		)
	      ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select next item in list and browse it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :BROWSE_NEXT
      '(XEVENT)
      '(
	(send self :goto_next)
	(send-super :call_action_proc "ListKbdActivate" XEVENT)	;call :XMN_DEFAULT_ACTION_CALLBACK
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select previous item in list and browse it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :BROWSE_PREV
      '(XEVENT)
      '(
	(send self :goto_prev)
	(send-super :call_action_proc "ListKbdActivate" XEVENT)	;call :XMN_DEFAULT_ACTION_CALLBACK
	))
