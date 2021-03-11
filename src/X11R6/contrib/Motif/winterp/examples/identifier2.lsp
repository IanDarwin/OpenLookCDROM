;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         identifier.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/identifier2.lsp,v 2.4 1994/06/06 14:43:10 npm Exp $
; Description:  A useful UI debugging tool. Loading this file creates a panel
;		that allows you to click on a widget to identify it, click on
;		a widget to destroy it, or change the foreground and background
;		colors of the widget you click on. For Motif 1.1, the "Identify
;		Selected Widget" button becomes especially useful because it
;		will print out the fully qualified resource name -- this allows
;		setting up your X-resources on a per widget basis and allows you
;		to better understand which widgets are affected by a particular
;		setting in your ~/.Xdefaults...
; Author:       Niels Mayer
; Created:      Mon Oct 29 02:44:55 1990
; Modified:     Sun Jun  5 18:54:17 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*, *MOTIF-1.0-P*

(let* (toplevel_w
       rc_w identify_pb_w destroy_pb_w
       txlations_pb_w accels_pb_w
#|
       instaccels_pb_w
|#
       fg_rc_w fg_la_w fg_ed_w
       bg_rc_w bg_la_w bg_ed_w
       )

  (setq toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "identshl"
	      :XMN_TITLE	"WINTERP: Widget Operations (v2)"
	      :XMN_ICON_NAME	"W:identifier2"
	      ))
  (setq rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" toplevel_w
	      :XMN_PACKING		:pack_tight
	      :XMN_NUM_COLUMNS		1
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_ORIENTATION		:vertical
	      ))
  (setq identify_pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "identify" rc_w
	      :XMN_LABEL_STRING "Identify Selected Widget"
	      ))
  (setq destroy_pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "destroy" rc_w
	      :XMN_LABEL_STRING "Destroy Selected Widget"
	      ))
  (setq txlations_pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "txlations" rc_w
	      :XMN_LABEL_STRING "Display Translations of Sel. Widget"
	      ))
  (setq accels_pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "accels" rc_w
	      :XMN_LABEL_STRING "Display Accelerators of Sel. Widget"
	      ))
#|
  (setq instaccels_pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "instaccels" rc_w
	      :XMN_LABEL_STRING "Display Installed Accelerators of Sel. Widget"
	      ))
|#
  (setq fg_rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc_fgcolor" rc_w
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_ORIENTATION		:horizontal
	      ))
  (setq fg_la_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "label_fgcolor" fg_rc_w
	      :XMN_LABEL_STRING "Set Foreground Color\nof Selected Widget:"
	      ))
  (setq fg_ed_w
	(send XM_TEXT_WIDGET_CLASS :new :managed
	      "edit_fgcolor" fg_rc_w
	      :XMN_EDIT_MODE       :single_line_edit
	      ))
  (setq bg_rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc_bgcolor" rc_w
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_ORIENTATION		:horizontal
	      ))
  (setq bg_la_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "label_bgcolor" bg_rc_w
	      :XMN_LABEL_STRING "Set Background Color\nof Selected Widget:"
	      ))
  (setq bg_ed_w
	(send XM_TEXT_WIDGET_CLASS :new :managed
	      "edit_bgcolor" bg_rc_w
	      :XMN_EDIT_MODE       :SINGLE_LINE_EDIT
	      ))

  (send toplevel_w :realize)


  (if *MOTIF-1.0-P*
      ;; Motif 1.0 version -- method :NAME not def'd in X11r3
      ;; so we can't do all the fancy stuff as done below
      (send identify_pb_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	    '(
	      (let ((w (get_moused_widget))
		    height
		    width)

		(send w :get_values
		      :XMN_HEIGHT 'height
		      :XMN_WIDTH  'width)

		(format t "\nwidget=~A\n\tparent=~A\n\theight=~A\n\twidth=~A\n"
			w (send w :parent) height width)
		)))
    ;; Motif 1.1/X11r4 version -- attempts to print fully qualified
    ;; resource name. Note that more work needs to be done on resource
    ;; name printing... current logic was hacked, not designed.
    (send identify_pb_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	  '(
	    (let ((w (get_moused_widget))
		  height
		  width)

	      (send w :get_values
		    :XMN_HEIGHT 'height
		    :XMN_WIDTH  'width)

	      (format t "\nwidget=~A\n\tparent=~A\n\theight=~A\n\twidth=~A\n"
		      w (send w :parent) height width)

	      (let* ((name (send w :name))
		     (resname (if (string= name "") "*" name))
		     (wildcard-p nil))
  
		(loop

		 (if (null (setq w (send w :parent)))
		     (return (format t "\tX-resource = ~A\n" resname)))

		 (setq name (send w :name))
		 (cond ((string= name "")
			(cond ((not wildcard-p)
			       (setq resname (concatenate 'string "*" resname))
			       (setq wildcard-p t)))
			)
		       (t
			(cond (wildcard-p
			       (setq resname (concatenate 'string name resname))
			       (setq wildcard-p nil)
			       )
			      (T
			       (setq resname (concatenate 'string name "." resname))
			       )
			      ))
		       )
		 ))
	      )))
    )

  (send destroy_pb_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send (get_moused_widget) :destroy)
	  ))

  (send txlations_pb_w :set_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	'(
	  (let ((w (get_moused_widget)))
	    (format T "------------------------------------------------------------------------------\n")
	    (format T "XtDisplayTranslations(~A):\n" w)
	    (send w :CALL_ACTION_PROC "XtDisplayTranslations" CALLBACK_XEVENT)
	    (format T "------------------------------------------------------------------------------\n")
	    )))

  (send accels_pb_w :set_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	'(
	  (let ((w (get_moused_widget)))
	    (format T "------------------------------------------------------------------------------\n")
	    (format T "XtDisplayAccelerators(~A):\n" w)
	    (send w :CALL_ACTION_PROC "XtDisplayAccelerators" CALLBACK_XEVENT)
	    (format T "------------------------------------------------------------------------------\n")
	    )))
#|
  (send instaccels_pb_w :set_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	'(
	  (let ((w (get_moused_widget)))
	    (format T "------------------------------------------------------------------------------\n")
	    (format T "XtDisplayInstalledAccelerators(~A):\n" w)
	    (send w :CALL_ACTION_PROC "XtDisplayInstalledAccelerators"  CALLBACK_XEVENT)
	    (format T "------------------------------------------------------------------------------\n")
	    )))
|#
  (send fg_ed_w :set_callback :XMN_ACTIVATE_CALLBACK
	'(CALLBACK_WIDGET)
	'(
	  (send (get_moused_widget) :set_values
		:XMN_FOREGROUND (send CALLBACK_WIDGET :get_string)
		)
	  ))

  (send bg_ed_w :set_callback :XMN_ACTIVATE_CALLBACK
	'(CALLBACK_WIDGET)
	'(
	  (send (get_moused_widget) :set_values
		:XMN_BACKGROUND (send CALLBACK_WIDGET :get_string)
		)
	  ))

  )
