; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         object-br.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/object-br.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Object_Browser_Widget_Class, a subclass of
;		String_Browser_Widget_Class, a subclass of
;		XM_LIST_WIDGET_CLASS (see comments below for details  esp.
;		:EDIT_SELECTED_ITEM, :SET_BROWSER_ITEMS).
;		Note, the bindings inherited from String_Browser_Widget_Class:
;
;		Note mouse bindings on browser override XmList(3x)'s:
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
;		Note key bindings on browser overriding XmList(3x)'s:
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
; Modified:     Mon Jun  6 01:08:07 1994 (Niels Mayer) npm@indeed
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

(require "lib-widgets/string-br")	;define String_Browser_Widget_Class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This class is mostly the same as String_Browser_Widget_Class, except that
;; you give :set_browser_items method a list of OBJECTs, rather than STRINGs.
;;
;; The OBJECTs displayed in this widget may be of different classes, but they
;; must have a :DISPLAY_STRING method defined. An :EXTERNAL_EDIT method should
;; be defined on the OBJECTs as well, this method will allow the object to be
;; viewed/edited in an external editor. If no :EXTERNAL_EDIT functionality is
;; needed, the method should be defined anyways, with an empty body;
;; otherwise, an error will occur when 'e' or mouse-3 is entered on the 
;; object displayed in the widget. (see the translation table specified
;; by :OVERRIDE_TRANSLATIONS in "lib-widgets/string-br" for details).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Object_Browser_Widget_Class
  (send Class :new
	'()				;new instance vars
	'()				;no class vars
	String_Browser_Widget_Class))	;Object_Browser_Widget_Class-->String_Browser_Widget_Class-->XM_LIST_WIDGET_CLASS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (send Object_Browser_Widget_Class :new [:managed/:unmanaged]
;; 	 [<name>]
;;	 <parent> 
;;	 [<resource_0> <value_0>]
;;	 . . .
;;	 [<resource_n> <value_n>])
;;
;; 	--> returns a new instance of class Object_Browser_Widget_Class
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We add a method to set the items browsed by the list browser
;; and set the 'items' instance variable.
;;
;; (send <Object_Browser_Widget_Class> :set_browser_items <items_list>)
;; <items_list> is a list of arbitrary OBJECTS responding to :display_string
;; method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
		      :XMN_ITEMS	display_items
		      :XMN_ITEM_COUNT	items_end_idx
		      )
	  )

	;; clear out any previous selection...
	(send-super :clear_selection)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Object_Browser_Widget_Class :answer :EDIT_SELECTED_ITEM '()
      '(
 	(let ((sel-obj (send self :get_selected_item)))
 	  (if sel-obj			;set to NIL if no browsed file
	      (send sel-obj :external_edit)
 	    ))
 	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/object-br")
