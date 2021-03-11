; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         string-br.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/string-br.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  String_Browser_Widget_Class, a subclass of XM_LIST_WIDGET_CLASS
;		(see comments below for details).
;
;		Note mouse bindings on browser override XM_LIST_WIDGET_CLASS's
;
;		* single left click   -- make item the current selection
;		* double left click, or single middle click
;				      -- select item and call Xt-action
;					 ListKbdActivate() which calls the
;					 :XMN_DEFAULT_ACTION_CALLBACK you 
;					 have defined on an instance.
;		* single right click  -- select item and call method
;		  			 :EDIT_SELECTED_ITEM, which may
;					 be redefined by superclasses.
;
;		Note key bindings on browser overriding XM_LIST_WIDGET_CLASS's:
;
;		* <Key>E:	      -- Call method :EDIT_SELECTED_ITEM
;		* Ctrl<Key>N or	Ctrl<Key>osfDown:
;				      -- Call method :GOTO_NEXT, which selects
;					 the next item in the list.
;		* Ctrl<Key>P, or Ctrl<Key>osfUp:
;				      -- Call method :GOTO_PREV, which selects
;					 the previous item in the list.
;		* <Key>N, or <Key>osfDown:
;				      -- Call method :BROWSE_NEXT which
;					 selects the next item in the list and
;				         calls ListKbdActivate() which calls
;					 the :XMN_DEFAULT_ACTION_CALLBACK.
;		* <Key>P, <Key>osfUp: -- Call method :BROWSE_PREV which
;					 selects the next item in the list and
;				         calls ListKbdActivate() which calls
;					 the :XMN_DEFAULT_ACTION_CALLBACK.
; Author:       Niels P. Mayer
; Created:      Sun Dec 29 19:32:28 1991
; Modified:     Mon Jun  6 01:09:53 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*, *MOTIF-1.0-P*

(if *MOTIF-1.0-P*			;specifically, :CALL_ACTION_PROC doesn't exist in X11r3/Motif-1.0
    (error "Applications depending on \"lib-widgets/string-br\" cannot be used with Motif 1.0.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make a subclass of XM_LIST_WIDGET_CLASS which holds additional instance
;; variables 'items' and 'selected_pos'. 'items' is an array of
;; STRINGs to be displayed in the XM_LIST_WIDGET_CLASS instance.
;; 'selected_pos' represents the index of the selected item within the
;; XM_LIST_WIDGET_CLASS; the item is selected via XmList's :browse_select
;; :XMN_SELECTION_POLICY.
;;
;; Subclasses of String_Browser_Widget_Class will store other kinds of objects
;; in 'items' and may override methods :SET_BROWSER_ITEMS and
;; :EDIT_SELECTED_ITEM such that these other kinds of objects can be displayed
;; in the XmList widget, and edited using the desired external viewer/editor
;; functionality.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar String_Browser_Widget_Class
  (send Class :new
	'(items selected_pos)		;new instance vars
	'()				;no class vars
	XM_LIST_WIDGET_CLASS))		;superclass

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (send String_Browser_Widget_Class :new [:managed/:unmanaged]
;; 	 [<name>]
;;	 <parent> 
;;	 [<resource_0> <value_0>]
;;	 . . .
;;	 [<resource_n> <value_n>])
;;
;; 	--> returns a new instance of class String_Browser_Widget_Class
;;	    this instatiates a Motif XM_LIST_WIDGET_CLASS.
;; 
;; 	The optional keyword-argument :MANAGED will cause a subsequent call
;; 	to XtManageChild() (see also method :MANAGE).
;; 	If the submessage :UNMANAGED is	present, or no submessage, then
;; 	XtManageChild() won't be called, and the resulting widget will be
;; 	returned unmanaged.
;; 
;; 	The optional argument <name> is a string which becomes the
;; 	name of the widget as used by the resource manager. If you
;; 	are not setting any widget resources via the resource
;; 	manager, you may leave this parameter out.
;; 
;; 	The argument <parent> is a WIDGETOBJ that will be managing
;; 	the widget to be created.
;; 
;; 	The optional arguments [<resource_i> <value_i>]... represent a series
;; 	of Motif resource name/value pairs -- upon creation of the
;; 	widget, each <resource_i> will be set to <value_i>. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :ISNEW
      '(managed_k widget_name widget_parent
		  &rest args)
      '(
	;; initialize the instance variables
	(setq items		nil
	      selected_pos	nil)

	;; initialize the widget...
	(apply #'send-super :isnew
	       managed_k :scrolled widget_name widget_parent
	       :XMN_SELECTION_POLICY :browse_select
	       args
	       )

	;; add a callback to set selected_pos instance var.
	(send-super :add_callback :XMN_BROWSE_SELECTION_CALLBACK '(CALLBACK_ITEM_POSITION)
		    '(
		      (setq selected_pos CALLBACK_ITEM_POSITION)
		      )
		    )

	;;
	;; ???Setting these at create-time generates the following warning:
	;; "Warning: Actions not found: ListBeginSelect, ListEndSelect, ListKbdActivate"
	;; (Same occurs for Motif 1.2)
	;; In Motif 1.2, doing
	;;       (send <w> :SET_VALUES :XMN_TRANSLATIONS "#override...")
	;; has additional problem of completely deleting the original
	;; translation table, therefore, we use
	;;       (send <w> :OVERRIDE_TRANSLATIONS "...")
	;; form below -- this works properly for both Motif 1.1 and 1.2.
	;;
	(send-super :OVERRIDE_TRANSLATIONS
		    "	<Btn2Down>:   	ListBeginSelect() \
			<Btn2Up>:	ListEndSelect() ListKbdActivate() \
			<Btn3Down>:	ListBeginSelect() \
			<Btn3Up>:	ListEndSelect() Lisp(send ACTION_WIDGET :edit_selected_item) \
			<Key>E:		Lisp(send ACTION_WIDGET :edit_selected_item) \
			Ctrl<Key>N:	Lisp(send ACTION_WIDGET :goto_next) \
			Ctrl<Key>osfDown: Lisp(send ACTION_WIDGET :goto_next) \
			Ctrl<Key>P:	Lisp(send ACTION_WIDGET :goto_prev) \
			Ctrl<Key>osfUp:	Lisp(send ACTION_WIDGET :goto_prev) \
			<Key>N:		Lisp(send ACTION_WIDGET :browse_next ACTION_XEVENT) \
			<Key>osfDown:	Lisp(send ACTION_WIDGET :browse_next ACTION_XEVENT) \
			<Key>P:		Lisp(send ACTION_WIDGET :browse_prev ACTION_XEVENT) \
			<Key>osfUp:	Lisp(send ACTION_WIDGET :browse_prev ACTION_XEVENT)"
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
		      :XMN_ITEMS	display_items
		      :XMN_ITEM_COUNT	items_end_idx
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
		      (min num-br-items (+ selected_pos (truncate num-br-visible 2))))
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
		(send self :set_pos (max 1 (- selected_pos (truncate visible-items 2))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No-Op method to throw away :EDIT_SELECTED_ITEM messages for
;; subclasses that don't define their own method. :EDIT_SELECTED_ITEM
;; is called from translations on this widget...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send String_Browser_Widget_Class :answer :EDIT_SELECTED_ITEM
      '()
      '(
	(X_BELL)			;SIGNAL ERROR -- BEEP
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/string-br")
