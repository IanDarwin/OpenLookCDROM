; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         object-br.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/file-br.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  File_Browser_Widget_Class, a subclass of
;		String_Browser_Widget_Class, a subclass of XM_LIST_WIDGET_CLASS.
;		(see comments below for details, esp. :EDIT_SELECTED_ITEM).
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
;		  			 :EDIT_SELECTED_ITEM
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
; Modified:     Mon Jun  6 01:07:02 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define *SYSTEM-EDITOR*
(require "lib-widgets/string-br")	;define String_Browser_Widget_Class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File_Browser_Widget_Class --> String_Browser_Widget_Class --> XM_LIST_WIDGET_CLASS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This class is mostly the same as String_Browser_Widget_Class. The STRINGs
;; specified via String_Browser_Widget_Class method :SET_BROWSER_ITEMS should
;; be filenames relative to the directory specified via method :SET_DIRECTORY.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar File_Browser_Widget_Class
  (send Class :new
	'(directory)			;new instance vars
	'()				;no class vars
	String_Browser_Widget_Class))	;File_Browser_Widget_Class-->String_Browser_Widget_Class-->XM_LIST_WIDGET_CLASS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The :EDIT_SELECTED_ITEM method is called from String_Browser_Widget_Class
;; superclass when 'e' or mouse-3 is entered on the file displayed in the
;; widget. (see the translation table specified by :OVERRIDE_TRANSLATIONS in
;; "lib-widgets/string-br" for details).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send File_Browser_Widget_Class :answer :EDIT_SELECTED_ITEM '()
      '(
 	(let ((file (send self :get_selected_item)))
 	  (if file			;set to NIL if no browsed file
	      (if (consp directory)	;hack -- contains a dotted pair of dirs...
		  (system (format nil 
				  "~A ~A~A & \n ~A ~A~A &"
				  (if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
				  (car directory)
				  file
				  (if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
				  (cdr directory)
				  file
				  ))
		(system (format nil 
				"~A ~A~A &"
				(if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
				directory
				file
				))
		)
 	    ))
 	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve the directory STRING or CONS (dotted pair of STRINGs) set
;; previously by :SET_DIRECTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send File_Browser_Widget_Class :answer :GET_DIRECTORY '()
      '(
	directory
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the directory STRING or CONS (dotted pair of STRINGs). THe directories
;; must include the final "/", or :EDIT_SELECTED_ITEM will fail.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send File_Browser_Widget_Class :answer :SET_DIRECTORY '(d)
      '(
	(setq directory d)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/file-br")
