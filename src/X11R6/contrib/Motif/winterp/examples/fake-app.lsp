;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         fake-app.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/fake-app.lsp,v 2.7 1994/06/06 14:43:17 npm Exp $
; Description:  Example application using XM_MAIN_WINDOW_WIDGET_CLASS +
;               XM_ROW_COLUMN_WIDGET_CLASS/:simple_menu_bar +
;               XM_ROW_COLUMN_WIDGET_CLASS/:simple_pulldown_menu
;		to create a window with a menubar and pulldowns, etc.
; Author:       Niels Mayer
; Created:      Fri Feb  8 19:59:47 1991
; Modified:     Sun Jun  5 18:39:36 1994 (Niels Mayer) npm@indeed
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
(require "lib-widgets/timed-msg")	;define TIMED-MESSAGE-DISPLAY-WIDGET-CLASS
(require "lib-widgets/fileselect")	;define WINTERP:FILE-SELECTION-WIDGET and methods 
					; :set-file-selected-callback-closure, 
					; :set-dir-selected-callback-closure.

(if *MOTIF-1.0-P*
    (error "Most features in fake-app.lsp are present only in Motif >= 1.1\
            -- 1.0 doesn't have the required functionality."))

(let (toplevel_w main_w menubar_w paned_w commandwindow_w edit_w msg_w)

  (setq toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "fakeapp"
	      :XMN_TITLE	"WINTERP: Fake Application"
	      :XMN_ICON_NAME	"W:fake-app"
	      ))

  (setq main_w
	(send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
	      "mainw" toplevel_w
	      :XMN_SHOW_SEPARATOR T
	      ))

  (setq menubar_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
	      "menubar" main_w
	      :XMN_BUTTON_COUNT		5
	      :XMN_BUTTONS		#("Files" "Edit" "Fold" "Spindle" "Mutilate")
	      :XMN_BUTTON_MNEMONICS	#(#\F     #\E    #\o    #\S       #\M)
	      :XMN_BUTTON_TYPE		#(:CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON)
	      ))

  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "files" menubar_w
	 :XMN_POST_FROM_BUTTON	0	;post pulldown from menubar's "Files" button.
	 :XMN_BUTTON_COUNT	5	;create five buttons in this pulldown
	 :XMN_BUTTONS		        #("Quit"	"Open"		"Open in New Window"	"Save"		"Save As")
	 :XMN_BUTTON_MNEMONICS         #(#\Q           #\O                     #\N             #\S                  #\A)
	 :XMN_BUTTON_MNEMONIC_CHAR_SETS	#(""            ""              "ISO8859-1"             "ISO8859-1"      "ISO8859-1")
	 :XMN_BUTTON_ACCELERATORS       #("Ctrl<Key>C"  "Ctrl<Key>F"    "Ctrl<Key>O"            "Ctrl<Key>S"     "Ctrl<Key>W")
	 :XMN_BUTTON_ACCELERATOR_TEXT   #("^C"		"^F"		"^O"			"^S"		"^W")
	 :XMN_BUTTON_TYPE		#(:PUSHBUTTON   :PUSHBUTTON     :PUSHBUTTON             :PUSHBUTTON     :PUSHBUTTON)
	 )
   :add_callback :xmn_entry_callback	;use this instead of XmNsimpleCallback
   '(CALLBACK_ENTRY_WIDGET)
   '(
     ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
     ;; where <#> is 0 ... (button-count-1).
     ;; we use 'read' to return the FIXNUM <#> after truncating the
     ;; 7 chars "button_" from the front of the string.
     (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
	   (0 (send msg_w :display-string "Quit Function Called (NOT IMPLEMENTED)."))
	   (1 (send msg_w :display-string "Open Function Called (NOT IMPLEMENTED)."))
	   (2 (send msg_w :display-string "Open in New Window Function Called (NOT IMPLEMENTED)."))
	   (3 (send msg_w :display-string "Save Function Called (NOT IMPLEMENTED)."))
	   (4 (send msg_w :display-string "Save As Function Called (NOT IMPLEMENTED)."))
	   (T (send msg_w :error-display-string "Error")))
     ))

  (setq edit_pd_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	      "edit" menubar_w
	      :XMN_POST_FROM_BUTTON	1 ;post from menubar's "Edit"
	      :XMN_BUTTON_COUNT		8
	      :XMN_BUTTONS		#("One" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight")
	      :XMN_BUTTON_MNEMONICS	#(#\O   #\T   #\h     #\F    #\i    #\S   #\e     #\g)
	      :XMN_BUTTON_TYPE		#(:PUSHBUTTON :TOGGLEBUTTON :CHECKBUTTON :RADIOBUTTON :CASCADEBUTTON :SEPARATOR :DOUBLE_SEPARATOR :TITLE)
	      ))
  (send edit_pd_w :add_callback :xmn_entry_callback '(CALLBACK_ENTRY_WIDGET)
	'(
	  (process-pulldown-buttonclick CALLBACK_ENTRY_WIDGET msg_w)
	  ))

  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "five-submenu" edit_pd_w
	 :XMN_POST_FROM_BUTTON	4	;post from above simple menubar "Five"
	 :XMN_BUTTON_COUNT	8
	 :XMN_BUTTONS		#("One" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight")
	 :XMN_BUTTON_MNEMONICS	#(#\O   #\T   #\h     #\F    #\i    #\S   #\e     #\g)
	 :XMN_BUTTON_TYPE	#(:TITLE :DOUBLE_SEPARATOR :SEPARATOR :CASCADEBUTTON :RADIOBUTTON :CHECKBUTTON :TOGGLEBUTTON :PUSHBUTTON)
	 )
   :add_callback :xmn_entry_callback '(CALLBACK_ENTRY_WIDGET)
   '(
     (process-pulldown-buttonclick CALLBACK_ENTRY_WIDGET msg_w)
     ))


  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "fold" menubar_w
	 :XMN_POST_FROM_BUTTON	2	;post from menubar's "Fold"
	 :XMN_BUTTON_COUNT	5
	 :XMN_BUTTONS		#("One" "Two" "Three" "Four" "Five")
	 :XMN_BUTTON_MNEMONICS	#(#\O   #\T   #\h     #\F    #\i   )
	 :XMN_BUTTON_TYPE	#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	 )
   :add_callback :xmn_entry_callback '(CALLBACK_ENTRY_WIDGET)
   '(
     (process-pulldown-buttonclick CALLBACK_ENTRY_WIDGET msg_w)
     ))

  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "spindle" menubar_w
	 :XMN_POST_FROM_BUTTON	3	;post from menubar's "Spindle"
	 :XMN_BUTTON_COUNT	5
	 :XMN_BUTTONS		#("One" "Two" "Three" "Four" "Five")
	 :XMN_BUTTON_MNEMONICS	#(#\O   #\T   #\h     #\F    #\i   )
	 :XMN_BUTTON_TYPE	#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	 )
   :add_callback :xmn_entry_callback '(CALLBACK_ENTRY_WIDGET)
   '(
     (process-pulldown-buttonclick CALLBACK_ENTRY_WIDGET msg_w)
     ))

  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "mutilate" menubar_w
	 :XMN_POST_FROM_BUTTON	4	;post from menubar's "Mutilate"
	 :XMN_BUTTON_COUNT	5
	 :XMN_BUTTONS		#("One" "Two" "Three" "Four" "Five")
	 :XMN_BUTTON_MNEMONICS	#(#\O   #\T   #\h     #\F    #\i   )
	 :XMN_BUTTON_TYPE	#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	 )
   :add_callback :xmn_entry_callback '(CALLBACK_ENTRY_WIDGET)
   '(
     (process-pulldown-buttonclick CALLBACK_ENTRY_WIDGET msg_w)
     ))

  (setq paned_w
	(send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
	      "paned" main_w 
	      ))

  (setq commandwindow_w
	(send WINTERP:FILE-SELECTION-WIDGET :new :managed
	      "commandwindow" paned_w
	      :XMN_MARGIN_HEIGHT 2	;should be in app-defaults
	      :XMN_MARGIN_WIDTH  2	;should be in app-defaults
	      :XMN_LIST_VISIBLE_ITEM_COUNT 4 ;should be in app-defaults
	      ))

  (send commandwindow_w :set-file-selected-callback-closure
	(lambda (selected_file_str)
	  (send msg_w :display-string (format nil "viewing file: ~A" selected_file_str))
	  (send edit_w :read_file selected_file_str)
	  ))

  (send commandwindow_w :set-dir-selected-callback-closure
	(lambda (selected_dir_str)
	  (send msg_w :display-string (format nil "viewing directory: ~A" selected_dir_str))
	  ))

  (setq edit_w
	(send XM_TEXT_WIDGET_CLASS :new :managed :scrolled
	      "edit" paned_w
	      :XMN_EDIT_MODE	:MULTI_LINE_EDIT
	      :XMN_COLUMNS	80
	      :XMN_ROWS		24
	      ))

  (setq msg_w
	(send TIMED-MESSAGE-DISPLAY-WIDGET-CLASS :new :managed
	      "msg" main_w
	      ))

  ;; set areas for the XmMainWindow (see also method :set_areas)
  (send main_w :set_values
	:XMN_MENU_BAR		menubar_w
	:XMN_WORK_WINDOW	paned_w
	:XMN_MESSAGE_WINDOW	msg_w
	)

  (send toplevel_w :realize)
  )

(defun process-pulldown-buttonclick (w msg_w)
  (send msg_w :display-string
	(format nil "Name==~A Label==~A."
	  (send w :name)
	  (xm_string_get_l_to_r (car (send w :get_values :xmn_label_string nil))))
	)
  )
