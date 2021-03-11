;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         modal-dia.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/modal-dia.lsp,v 2.4 1994/06/06 14:43:08 npm Exp $
; Description:  Tests of Modal Dialogs through resource :XMN_DIALOG_STYLE
; Author:       Niels Mayer
; Created:      Mon Nov 20 18:13:23 1989
; Modified:     Sun Jun  5 19:05:28 1994 (Niels Mayer) npm@indeed
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-date ()
  (let*
      ((pipe (popen "date" :direction :input))
       (str (read-line pipe))
       )
    (pclose pipe)
    str))

(let*
    ((toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	    :XMN_TITLE		"WINTERP: Modal Dialog Tests"
	    :XMN_ICON_NAME	"W:modal-dia"
	    ))
     (scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
	    "sc" toplevel_w
	    :XMN_SCROLLING_POLICY :AUTOMATIC
	    :XMN_WIDTH		400
	    :XMN_HEIGHT		400
	    ))
     (rowcol_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "rc" scrl_w
	    :XMN_ORIENTATION :VERTICAL
	    :XMN_PACKING :PACK_TIGHT
	    :XMN_ENTRY_ALIGNMENT :ALIGNMENT_BEGINNING
	    ))
     (pb-modeless_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb_w" rowcol_w
	    :XMN_LABEL_STRING "Push Me (:DIALOG_MODELESS)"
	    ))

     (pb-full-app-modal_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb_w" rowcol_w
	    :XMN_LABEL_STRING "Push Me (:DIALOG_FULL_APPLICATION_MODAL)"
	    ))

     (pb-system-modal_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb_w" rowcol_w
	    :XMN_LABEL_STRING "Push Me (:DIALOG_SYSTEM_MODAL)"
	    ))

     (pb-primary-app-modal_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb_w" rowcol_w
	    :XMN_LABEL_STRING "Push Me (:DIALOG_PRIMARY_APPLICATION_MODAL)"
	    ))

     (pb-work-area_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "pb_w" rowcol_w
	    :XMN_LABEL_STRING "Push Me (:DIALOG_WORK_AREA)"
	    ))

     (qd-modeless_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :question_dialog
	    "modeless-question-dialog" toplevel_w
	    :XMN_MESSAGE_STRING "Do you want to create another item?"
	    :XMN_DIALOG_STYLE :DIALOG_MODELESS
	    ))

     (qd-full-app-modal_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :question_dialog
	    "full-app-modal-question-dialog" toplevel_w
	    :XMN_MESSAGE_STRING "Do you want to create another item?"
	    :XMN_DIALOG_STYLE :DIALOG_FULL_APPLICATION_MODAL
	    ))

     (qd-system-modal_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :question_dialog
	    "system-modal-question-dialog" toplevel_w
	    :XMN_MESSAGE_STRING "Do you want to create another item?"
	    :XMN_DIALOG_STYLE :DIALOG_SYSTEM_MODAL
	    ))

     (qd-primary-app-modal_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :question_dialog
	    "primary-app-modal-question-dialog" toplevel_w
	    :XMN_MESSAGE_STRING "Do you want to create another item?"
	    :XMN_DIALOG_STYLE :DIALOG_PRIMARY_APPLICATION_MODAL
	    ))

     (qd-work-area_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :question_dialog
	    "work-area-question-dialog" toplevel_w
	    :XMN_MESSAGE_STRING "Do you want to create another item?"
	    :XMN_DIALOG_STYLE :DIALOG_WORK_AREA
	    ))
     )

  (send pb-modeless_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send qd-modeless_w :manage)
	  ))

  (send pb-full-app-modal_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send qd-full-app-modal_w :manage)
	  ))

  (send pb-system-modal_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send qd-system-modal_w :manage)
	  ))

  (send pb-primary-app-modal_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send qd-primary-app-modal_w :manage)
	  ))

  (send pb-work-area_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send qd-work-area_w :manage)
	  ))

  (send qd-modeless_w :set_callback :XMN_OK_CALLBACK '()
	'(
	  (send XM_LABEL_WIDGET_CLASS :new :managed
		"label" rowcol_w
		:XMN_LABEL_STRING (get-date)
		)
	  (send qd-modeless_w :unmanage)
	  ))

  (send qd-full-app-modal_w :set_callback :XMN_OK_CALLBACK '()
	'(
	  (send XM_LABEL_WIDGET_CLASS :new :managed
		"label" rowcol_w
		:XMN_LABEL_STRING (get-date)
		)
	  (send qd-full-app-modal_w :unmanage)
	  ))

  (send qd-system-modal_w :set_callback :XMN_OK_CALLBACK '()
	'(
	  (send XM_LABEL_WIDGET_CLASS :new :managed
		"label" rowcol_w
		:XMN_LABEL_STRING (get-date)
		)
	  (send qd-system-modal_w :unmanage)
	  ))

  (send qd-primary-app-modal_w :set_callback :XMN_OK_CALLBACK '()
	'(
	  (send XM_LABEL_WIDGET_CLASS :new :managed
		"label" rowcol_w
		:XMN_LABEL_STRING (get-date)
		)
	  (send qd-primary-app-modal_w :unmanage)
	  ))

  (send qd-work-area_w :set_callback :XMN_OK_CALLBACK '()
	'(
	  (send XM_LABEL_WIDGET_CLASS :new :managed
		"label" rowcol_w
		:XMN_LABEL_STRING (get-date)
		)
	  (send qd-work-area_w :unmanage)
	  ))

  (send qd-modeless_w :set_callback :XMN_CANCEL_CALLBACK '()
	'(
	  (send qd-modeless_w :unmanage)
	  ))

  (send qd-full-app-modal_w :set_callback :XMN_CANCEL_CALLBACK '()
	'(
	  (send qd-full-app-modal_w :unmanage)
	  ))

  (send qd-system-modal_w :set_callback :XMN_CANCEL_CALLBACK '()
	'(
	  (send qd-system-modal_w :unmanage)
	  ))

  (send qd-primary-app-modal_w :set_callback :XMN_CANCEL_CALLBACK '()
	'(
	  (send qd-primary-app-modal_w :unmanage)
	  ))

  (send qd-work-area_w :set_callback :XMN_CANCEL_CALLBACK '()
	'(
	  (send qd-work-area_w :unmanage)
	  ))

  (send qd-modeless_w :set_callback :XMN_HELP_CALLBACK '()
	'(
	  (format T "Help not implemented yet...\n")
	  (send qd-modeless_w :unmanage)
	  ))

  (send qd-full-app-modal_w :set_callback :XMN_HELP_CALLBACK '()
	'(
	  (format T "Help not implemented yet...\n")
	  (send qd-full-app-modal_w :unmanage)
	  ))

  (send qd-system-modal_w :set_callback :XMN_HELP_CALLBACK '()
	'(
	  (format T "Help not implemented yet...\n")
	  (send qd-system-modal_w :unmanage)
	  ))

  (send qd-primary-app-modal_w :set_callback :XMN_HELP_CALLBACK '()
	'(
	  (format T "Help not implemented yet...\n")
	  (send qd-primary-app-modal_w :unmanage)
	  ))

  (send qd-work-area_w :set_callback :XMN_HELP_CALLBACK '()
	'(
	  (format T "Help not implemented yet...\n")
	  (send qd-work-area_w :unmanage)
	  ))

  (send toplevel_w :realize)
  )
