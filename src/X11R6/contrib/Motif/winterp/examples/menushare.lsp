;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         menushare.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/menushare.lsp,v 2.7 1994/06/06 14:43:09 npm Exp $
; Description:  Demo of shared pulldown menu panes. Some versions of Motif
;		are buggy when using shared menu panes and may coredump
;		WINTERP when you destroy the windows created in this file.
; Author:       Niels Mayer
; Created:      Sat Oct  5 21:24:29 1991
; Modified:     Sun Jun  5 19:01:02 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define WIDGET_CLASS method :GET

(let (shared_pd_w)

  (let (toplevel_w main_w menubar0_w edit_w)
    (setq toplevel_w
	  (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
		"menushare"
		:XMN_TITLE	"WINTERP: Shared(0) Pulldown Menus"
		:XMN_ICON_NAME	"W:menushare"
	  ))
    (setq main_w
	  (send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
		"mainw" toplevel_w
		:XMN_SHOW_SEPARATOR T
		))
    (setq menubar0_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
		"menubar-0" main_w
		:XMN_BUTTON_COUNT	1
		:XMN_BUTTONS		#("Odin")
		:XMN_BUTTON_MNEMONICS	#(#\O)
		:XMN_BUTTON_TYPE	#(:CASCADEBUTTON)
		))
    (setq shared_pd_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		"shared-pulldown-menu" menubar0_w
		:XMN_POST_FROM_BUTTON	0 ;post pulldown from menubar's 0th item
		:XMN_BUTTON_COUNT	5 ;create five buttons in this pulldown
		:XMN_BUTTONS		#("xterm" "hpterm" "emacs" "xmh" "edit file in editor")
		:XMN_BUTTON_MNEMONICS	#(#\x     #\h      #\e     #\m    #\e)
		:XMN_BUTTON_TYPE	#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
		))
    (send shared_pd_w :set_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
	  '(CALLBACK_ENTRY_WIDGET CALLBACK_WIDGET)
	  '(
	    ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
	    ;; where <#> is 0 ... (button-count-1).
	    ;; we use 'read' to return the FIXNUM <#> after truncating the
	    ;; 7 chars "button_" from the front of the string.
	    (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		  (0
;;;	       (system "xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 /usr/bin/X11/xterm \" &")
		   (format T "'~A' selected from ~A\n"
			   (xm_string_get_l_to_r (send CALLBACK_ENTRY_WIDGET :get :XMN_LABEL_STRING))
			   (send (send shared_pd_w :get_posted_from_widget) :name))
		   )
		  (1
;;;	       (system "xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 /usr/bin/X11/hpterm \" &")
		   (format T "'~A' selected from ~A\n"
			   (xm_string_get_l_to_r (send CALLBACK_ENTRY_WIDGET :get :XMN_LABEL_STRING))
			   (send (send shared_pd_w :get_posted_from_widget) :name))
		   )
		  (2
;;;	       (system "xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 /usr/local/bin/x11emacs \" &")
		   (format T "'~A' selected from ~A\n"
			   (xm_string_get_l_to_r (send CALLBACK_ENTRY_WIDGET :get :XMN_LABEL_STRING))
			   (send (send shared_pd_w :get_posted_from_widget) :name))
		   )
		  (3
;;;	       (system "xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 /usr/local/bin/X11/xmh \" &")
		   (format T "'~A' selected from ~A\n"
			   (xm_string_get_l_to_r (send CALLBACK_ENTRY_WIDGET :get :XMN_LABEL_STRING))
			   (send (send shared_pd_w :get_posted_from_widget) :name))
		   )
		  (4
;;;	       (system (concatenate 'string
;;;			"xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 gnuclient -q"
;;;			(send edit_w :get_string)
;;;			" &"))
		   (format T "'~A' selected from ~A\n"
			   (xm_string_get_l_to_r (send CALLBACK_ENTRY_WIDGET :get :XMN_LABEL_STRING))
			   (send (send shared_pd_w :get_posted_from_widget) :name))
		   )
		  (T (system "Error\n")))
	    ))

    (setq edit_w
	  (send XM_TEXT_WIDGET_CLASS :new :managed "edit" main_w
		))
    (send main_w :set_values
	  :XMN_MENU_BAR	menubar0_w
	  :XMN_WORK_WINDOW	edit_w
	  )
    (send toplevel_w :realize)
    )

  (let (toplevel_w main_w menubar1_w edit_w)
    (setq toplevel_w
	  (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
		"menushare"
		:XMN_TITLE	"WINTERP: Shared(1) Pulldown Menus"
		:XMN_ICON_NAME	"W:menushare"
		))
    (setq main_w
	  (send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
		"mainw" toplevel_w
		:XMN_SHOW_SEPARATOR T
		))
    (setq menubar1_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
		"menubar-1" main_w
		:XMN_BUTTON_COUNT	1
		:XMN_BUTTONS		#("JHVH-1")
		:XMN_BUTTON_MNEMONICS	#(#\J)
		:XMN_BUTTON_TYPE	#(:CASCADEBUTTON)
		))

    (send (aref (send menubar1_w :get_children) 0) :set_values
	  :XMN_SUB_MENU_ID shared_pd_w)

    (setq edit_w
	  (send XM_TEXT_WIDGET_CLASS :new :managed "edit" main_w
		))
    (send main_w :set_values
	  :XMN_MENU_BAR	menubar1_w
	  :XMN_WORK_WINDOW	edit_w
	  )
    (send toplevel_w :realize)
    )

  (let (toplevel_w main_w menubar2_w edit_w)
    (setq toplevel_w
	  (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
		"menushare"
		:XMN_TITLE	"WINTERP: Shared(2) Pulldown Menus"
		:XMN_ICON_NAME	"W:menushare"
		))
    (setq main_w
	  (send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
		"mainw" toplevel_w
		:XMN_SHOW_SEPARATOR T
		))
    (setq menubar2_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
		"menubar-2" main_w
		:XMN_BUTTON_COUNT	1
		:XMN_BUTTONS		#("Wotan")
		:XMN_BUTTON_MNEMONICS	#(#\W)
		:XMN_BUTTON_TYPE	#(:CASCADEBUTTON)
		))

    (send (aref (send menubar2_w :get_children) 0) :set_values
	  :XMN_SUB_MENU_ID shared_pd_w)

    (setq edit_w
	  (send XM_TEXT_WIDGET_CLASS :new :managed "edit" main_w
		))
    (send main_w :set_values
	  :XMN_MENU_BAR	menubar2_w
	  :XMN_WORK_WINDOW	edit_w
	  )
    (send toplevel_w :realize)
    )

  )
