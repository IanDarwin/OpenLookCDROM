;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         dialogshel.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/dialogshel.lsp,v 2.4 1994/06/06 14:43:19 npm Exp $
; Description:  Demonstrates WINTERP's dialog shells, and what happens
;		when you manage/unmanage them. You may either load this file in
;		it's entirety, or interactively evaluate individual forms using
;		gnu-emacs or w_ctrlpnl.lsp.
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:54:12 1989
; Modified:     Sun Jun  5 18:35:44 1994 (Niels Mayer) npm@indeed
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

;; note use (hack) of unmapped *toplevel_widget* (winterp's main widget) as the
;; parent to pop up dialogues from.

(let ()
(setq bbd_w
      (send XM_BULLETIN_BOARD_WIDGET_CLASS :new :managed :dialog
	    "bulletin-board-dialog" *toplevel_widget*
	    ))
(setq fsb_w
      (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :managed :dialog
	    "file_selection_box" *toplevel_widget*
	    ))
(setq form_w
      (send XM_FORM_WIDGET_CLASS :new :managed :dialog
	    "form" *toplevel_widget*
	    ))
(setq seld_w
      (send XM_SELECTION_BOX_WIDGET_CLASS :new :managed :dialog
	    "selection-dialog" *toplevel_widget*
	    :xmn_list_items #("foo" "bar" "baz" "frop" "glop" "gloop" "gleep")
	    :xmn_list_item_count 7
	    ))
(setq prod_w
      (send XM_SELECTION_BOX_WIDGET_CLASS :new :managed :prompt_dialog
	    "selection-prompt-dialog" *toplevel_widget*
	    ))
;; (setq mbox_w
;;       (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed
;; 	    "message_box" *toplevel_widget*
;; 	    :XMN_MESSAGE_STRING "welcome to the night train"
;; 	    ))
(setq md_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :message_dialog
	    "message_dialog" *toplevel_widget*
	    :XMN_DIALOG_TITLE	"Message Dialog!"
	    :XMN_MESSAGE_STRING "the show is coming...."
	    ))
(setq ed_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :error_dialog
	    "error_dialog" *toplevel_widget*
	    :XMN_DIALOG_TITLE	"Error Dialog!"
	    :XMN_MESSAGE_STRING "snakeskin tracksuit"
	    ))
(setq id_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :information_dialog
	    "information_dialog" *toplevel_widget*
	    :XMN_DIALOG_TITLE	"Information Dialog!"
	    :XMN_MESSAGE_STRING "high protein snack"
	    ))
(setq qd_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :question_dialog
	    "question_dialog" *toplevel_widget*
	    :XMN_DIALOG_TITLE	"Question Dialog!"
	    :XMN_MESSAGE_STRING "get it together with the younger generation"
	    ))
(setq wrnd_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :warning_dialog
	    "warning_dialog" *toplevel_widget*
	    :XMN_DIALOG_TITLE	"Warning Dialog!"
	    :XMN_MESSAGE_STRING "beware of dub syndicate"
	    ))
(setq wrkd_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :working_dialog
	    "working_dialog" *toplevel_widget*
	    :XMN_DIALOG_TITLE	"Working Dialog!"
	    :XMN_MESSAGE_STRING "african head charge in effect"
	    ))
)

(mapcar
 #'(lambda (dialog-w)
     (send dialog-w :unmanage)
     )
 (list bbd_w fsb_w form_w seld_w prod_w 
       ;; mbox_w
       md_w ed_w id_w qd_w wrnd_w wrkd_w)
 )

(mapcar
 #'(lambda (dialog-w)
     (send dialog-w :manage)
     )
 (list bbd_w fsb_w form_w seld_w prod_w 
       ;; mbox_w
       md_w ed_w id_w qd_w wrnd_w wrkd_w)
 )

(mapcar
 #'(lambda (dialog-w)
     (send dialog-w :set_values :xmn_mapped_when_managed nil)
     )
 (list bbd_w fsb_w form_w seld_w prod_w 
       ;; mbox_w
       md_w ed_w id_w qd_w wrnd_w wrkd_w)
 )

(mapcar
 #'(lambda (dialog-w)
     (send dialog-w :set_values :xmn_mapped_when_managed t)
     )
 (list bbd_w fsb_w form_w seld_w prod_w 
       ;; mbox_w
       md_w ed_w id_w qd_w wrnd_w wrkd_w)
 )

(mapcar
 #'(lambda (dialog-w)
     (send (send dialog-w :parent) :set_values :xmn_mapped_when_managed nil)
     )
 (list bbd_w fsb_w form_w seld_w prod_w 
       ;; mbox_w
       md_w ed_w id_w qd_w wrnd_w wrkd_w)
 )

(mapcar
 #'(lambda (dialog-w)
     (send (send dialog-w :parent) :set_values :xmn_mapped_when_managed t)
     )
 (list bbd_w fsb_w form_w seld_w prod_w 
       ;; mbox_w
       md_w ed_w id_w qd_w wrnd_w wrkd_w)
 )

; (mapcar
;  #'(lambda (dialog-w)
;      (send (send dialog-w :parent) :manage)
;      )
;  (list bbd_w fsb_w form_w seld_w prod_w 
;        ;; mbox_w
;        md_w ed_w id_w qd_w wrnd_w wrkd_w)
;  )

(mapcar
 #'(lambda (dialog-w)
     (send (car (send dialog-w :get_values :XMN_DEFAULT_BUTTON nil))
	   :set_values :XMN_FONT_LIST "fixed")
     )
 (list
; bbd_w
  fsb_w
; form_w
  seld_w
  prod_w
; mbox_w
  md_w
  ed_w
  id_w
  qd_w
  wrnd_w
  wrkd_w)
 )

(mapcar
 #'(lambda (dialog-w)
     (send (car (send dialog-w :get_values :XMN_DEFAULT_BUTTON nil))
	   :set_values :XMN_FONT_LIST "variable")
     )
 (list
; bbd_w
  fsb_w
; form_w
  seld_w
  prod_w
; mbox_w
  md_w
  ed_w
  id_w
  qd_w
  wrnd_w
  wrkd_w)
 )

(mapcar
 #'(lambda (dialog-w)
     (format T "dialog-w=~A\n\tshell-w=~A\n\tdialog-managed=~A;shell-managed=~A\n\tdialog-mapped-when-managed=~A;shell-mapped-when-managed=~A\n\tdialog-realized=~A;shell-realized=~A\n"
	     dialog-w
	     (send dialog-w :parent)
	     (send dialog-w :is_managed)
	     (send (send dialog-w :parent) :is_managed)
	     (car (send dialog-w :get_values :xmn_mapped_when_managed nil))
	     (car (send (send dialog-w :parent) :get_values :xmn_mapped_when_managed nil))
	     (send dialog-w :is_realized)
	     (send (send dialog-w :parent) :is_realized)
	     )
     )
 (list bbd_w fsb_w form_w seld_w prod_w md_w ed_w id_w qd_w wrnd_w wrkd_w)
 )

(xt_add_timeout
 30000
 '(
   (mapcar
    #'(lambda (dialog-w)
	(send dialog-w :destroy)
	)
    (list bbd_w fsb_w form_w seld_w prod_w 
	  ;; mbox_w
	  md_w ed_w id_w qd_w wrnd_w wrkd_w)
    )
   )
 )
