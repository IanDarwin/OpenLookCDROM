; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         epoch-test.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/epoch-test.lsp,v 2.2 1994/06/06 14:43:18 npm Exp $
; Description:  Test/demos of Epoch_Widget_Class (see
;		lib-widgets/epoch-text.lsp) a text editor widget created by
;		reparenting an EPOCH multiwindow emacs editor window inside
;		a WINTERP/Motif widget. This allows you to edit files with
;		a real text editor while placing the Epoch-edit windows into y
;		our WINTERP-based applications. Epoch is a multiwindow
;		gnu-emacs-based editor available for free by anonymous ftp
;		from cs.uiuc.edu. You must load epoch-widg.el into Epoch
;		first, as this file calls epoch-functions defined there.
;		This file also assumes that you have Andy Norman's gnuserv
;		package running under Epoch -- the program
;		/usr/local/epoch/bin/gnudoit is used to send emacs-lisp
;		commands to Epoch.
; Author:       Niels P. Mayer
; Created:      Tue Mar  9 13:56:08 1993
; Modified:     Sun Jun  5 18:38:02 1994 (Niels Mayer) npm@indeed
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

(require "lib-widgets/epoch-text")	;define Epoch_Widget_Class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq form_w
      (send XM_FORM_WIDGET_CLASS :new :managed :dialog
	    "form" *toplevel_widget*
	    :XMN_DELETE_RESPONSE :destroy
	    ))
(setq w1
      (send XM_FRAME_WIDGET_Class :new :managed "fepoch1" form_w
	    :xmn_shadow_type :shadow_in
	    :xmn_top_attachment :attach_form
	    :xmn_left_attachment :attach_form
	    :xmn_right_attachment :attach_form
	    ))
(setq e1
      (send Epoch_Widget_Class :new :managed "epoch1" w1
	    ))
(send e1 :debug t)

(setq w2
      (send XM_FRAME_Widget_Class :new :managed "f-epoch2" form_w
	    :xmn_shadow_type :shadow_in
	    :xmn_top_attachment :attach_widget
	    :xmn_top_widget w1
	    :xmn_left_attachment :attach_form
	    :xmn_right_attachment :attach_form
	    :xmn_bottom_attachment :attach_form
	    ))
(setq e2
      (send Epoch_Widget_Class :new :managed "epoch2" w2
	    ))
(send e2 :debug t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "epoch-reparent"
	    :XMN_GEOMETRY "500x500+1+1"
	    ))
(setq pa_w
      (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed "pa" top_w
	    ))
(setq filename_editor_w 
      (send XM_TEXT_WIDGET_CLASS :new :managed "filename" pa_w
            :XMN_EDIT_MODE       :SINGLE_LINE_EDIT
            ))
(setq linenum_editor_w 
      (send XM_TEXT_WIDGET_CLASS :new :managed "linenum" pa_w
            :XMN_EDIT_MODE       :SINGLE_LINE_EDIT
            ))
(setq e1
      (send Epoch_Widget_Class :new :managed "epoch1" pa_w
	    ))
(send e1 :debug t)

(setq e2
      (send Epoch_Widget_Class :new :managed "epoch2" pa_w
	    ))
(send e2 :debug t)

(setq e3
      (send Epoch_Widget_Class :new :managed "epoch3" pa_w
	    ))
(send e3 :debug t)

(send filename_editor_w :set_callback  :XMN_ACTIVATE_CALLBACK ;Name of callback list
      '(CALLBACK_WIDGET) ;list of callback data
      '(			
	(print
	 (send e1 :read_file (send CALLBACK_WIDGET :get_string))
	 )
	))
(send linenum_editor_w :set_callback  :XMN_ACTIVATE_CALLBACK ;Name of callback list
      '(CALLBACK_WIDGET)		;list of callback data
      '(	
	(print
	 (send e1 :goto_line
		      (setq x (read (make-string-input-stream (send CALLBACK_WIDGET :get_string))))
		      t)
		)
	))

(send top_w :realize)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "epoch-reparent"
;;          ;; this arg causes extra RESIZE-EPOCH-SCREEN! after doing CREATE-EPOCH-SCREEN!
;;	    :XMN_ALLOW_SHELL_RESIZE t
	    :XMN_GEOMETRY "500x500+1+1"
	    ))
(setq e1
      (send Epoch_Widget_Class :new :managed "epoch1" top_w
	    ))
(send e1 :debug t)

(send top_w :realize)

(send e1 :READ_FILE "/etc/passwd")

