;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         accel.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/accel.lsp,v 2.4 1994/06/06 14:43:23 npm Exp $
; Description:  Example of accelerator usage. Load this file, and type letters
;		[a-z] into any pushbutton widget. each pushbutton widget
;		has a single accelerator, one of key [a-z], and accelerators for
;		all other pushbuttons get installed on each pushbutton... THe
;		accelerator arms the pushbutton, and the pushbutton's arm
;		callback enters the typed character into the text widget.
;		Thus, this is a highly rube-goldbergian means of echoing
;		characters typed into the text widget...
;		(Note that focus must be on a pushbutton or the textwdget.)
; Author:       Niels Mayer
; Created:      Thu Feb 14 07:09:25 1991
; Modified:     Sun Jun  5 18:15:59 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.0-P*, *MOTIF-1.1-OR-LATER-P*

(if *MOTIF-1.0-P*
    (error "accel.lsp doesn't work with Motif 1.0 -- probably a Motif bug\nUse only with >= Motif 1.1")
  (let ()
    (setq top_w
	  (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "aclshl"
		:XMN_TITLE			"WINTERP:Accelerator Test"
		:XMN_ICON_NAME			"W:accel"
		:XMN_KEYBOARD_FOCUS_POLICY	:explicit
		))
    (setq paned_w
	  (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
		"pane" top_w
		))
    (setq rc_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
		"rc" paned_w
		:XMN_ORIENTATION	:horizontal
		:XMN_PACKING		:pack_column
		:XMN_NUM_COLUMNS	2
		:XMN_ADJUST_LAST	nil
		))

    ;;
    ;; For each letter in alphabet, create a pushbutton widget... install
    ;; accelerators from all other widgets onto each pushbutton widget.
    ;;
    (mapcar
     (lambda (widget)
       (send widget :install_all_accelerators top_w)
       )
     (mapcar
      (lambda (letter)
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "pb" rc_w
	      :XMN_LABEL_STRING (format nil "~A"
					letter)
	      :XMN_ACCELERATORS (format nil "<Key>~A: ArmAndActivate()"
					letter)
	      ))
      '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\* #\/ #\+
	#\- #\[ #\] #\; #\' #\` #\. #\~ #\! #\@ #\# #\$ #\%
	#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
     )

    ;;
    ;; turn this example into the worlds most inefficient version of 
    ;; keyboard echo... application will echo characters typed in via keyboard
    ;; to the text widget text_w...
    ;;
    (setq text_w
	  (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled
		"edit" paned_w
		))

    (let ((position 0))
      (send rc_w :add_callback :xmn_entry_callback '(CALLBACK_ENTRY_WIDGET)
	    '(
	      (send text_w :insert position
		    (xm_string_get_l_to_r
		     (car (send CALLBACK_ENTRY_WIDGET :get_values :xmn_label_string nil))))
	      (setq position (1+ position))
	      (send text_w :show_position position)
	      ))
      )

    (send text_w :uninstall_translations)
    (send text_w :install_all_accelerators top_w)

    (send top_w :realize)
    )
  )
