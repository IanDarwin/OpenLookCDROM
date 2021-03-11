; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         text-view.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/text-view.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Text_Display_Widget_Class a subclass of XM_TEXT_WIDGET_CLASS
;		with :SCROLLED option. (see comments below for details).
;		Note key bindings on browser overriding XmText(3x)'s:
;		* <Key>space:		next-page()
;		* <Key>osfBackSpace:	previous-page()
; Author:       Niels P. Mayer
; Created:      Sun Dec 29 19:32:28 1991
; Modified:     Mon Jun  6 01:10:07 1994 (Niels Mayer) npm@indeed
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

(defvar Text_Display_Widget_Class
  (send Class :new
	'(inspos)			;new instance vars
	'()				;no class vars
	XM_TEXT_WIDGET_CLASS))		;superclass

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Text_Display_Widget_Class :answer :ISNEW
      '(managed_k name parent_w &rest args)
      '(
	;; initialize the instance variables
	(setq inspos 0)

	;; initialize the widget...
	(apply #'send-super :isnew
	       managed_k :scrolled name parent_w
	       :XMN_STRING			""
	       :XMN_EDIT_MODE			:multi_line_edit
	       :XMN_EDITABLE			nil ;don't allow user to change text.
	       :XMN_AUTO_SHOW_CURSOR_POSITION	nil ;don't need to show where the cursor is
	       :XMN_CURSOR_POSITION_VISIBLE	t ;do show the cursor, incase user "cursoring around"
	       args
	       )

	;; This is a text-viewer, so make space and backspace do paging.
	;; And override all other self-insert() bound chars, to prevent editing.
	;; see note above about needing to kludge around :XMN_EDITABLE problem
	(send-super :OVERRIDE_TRANSLATIONS
		    "<Key>space:        next-page() \
                     <Key>osfBackSpace: previous-page()"
		    )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Text_Display_Widget_Class :answer :CLEAR
      '()
      '(
	(setq inspos 0)
	(send-super :set_string "")	;therefore inspos is also 0
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Text_Display_Widget_Class :answer :APPEND-STRING
      '(str)
      '(
	(let* ((len (length str)))
	  (send-super :replace inspos inspos str)
	  (setq inspos (+ len inspos))
	  (send-super :show_position inspos)
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Text_Display_Widget_Class :answer :SAVE-IN-FILE-DIALOG
      '(dialog-title-str)
      '(
	(let ((fsb_w
	       (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :unmanaged :dialog
		     "save-in-file-dialog" self
		     :XMN_AUTO_UNMANAGE	nil
		     :XMN_DIALOG_TITLE	dialog-title-str
		     :XMN_DIALOG_STYLE	:dialog_full_application_modal ;they've got to answer this dialog before doing anything else w/ WINTERP...
		     )))
	  (send (send fsb_w :get_child :DIALOG_HELP_BUTTON) :unmanage)

	  (send fsb_w :add_callback :XMN_OK_CALLBACK '(CALLBACK_VALUE)
		'(
		  (send-super :write_file (xm_string_get_l_to_r CALLBACK_VALUE))
		  (send fsb_w :destroy)
		  ))
	  (send fsb_w :add_callback :XMN_CANCEL_CALLBACK '()
		'(
		  (send fsb_w :destroy)
		  ))

	  (send fsb_w :manage)
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/text-view")
