;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         menutree.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/menutree.lsp,v 2.7 1994/06/06 14:43:08 npm Exp $
; Description:  Simple menu tree program... see below
; Author:       Niels Mayer
; Created:      Sat Oct  5 21:24:29 1991
; Modified:     Sun Jun  5 19:04:54 1994 (Niels Mayer) npm@indeed
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

; To: davis@hplabs.hpl.hp.com (Jim Davis)
; Subject: Re: Want Menu-tree program 
; Newsgroups: comp.windows.x
; In-reply-to: Your message of 23 Aug 91 00:19:09 +0000
; Organization: Hewlett-Packard Labs, Software & Systems Lab, Palo Alto, CA.
; X-Mailer: mh6.7
; --------
; WINTERP is quite handy for building menu trees and associated actions
; without having to mess with C compiles and low-level junk. 
; 
; I have two possible solutions for you. One is the WINTERP Xmu menu server
; package that was contributed by Richard Hess of Consilium, Inc. I also
; quickly cobbled together a Winterp-Lisp file which does the basics of what
; you want.
; 
; > Where can I find such a program?  (Additional points are scored if the
; > program has a concept of string valued 'variables' that can be modified
; > through text widgets so I needn't keep using my cut buffer for such things.
; > More additional points for source, and someone to whom I can convey
; > enhancements.)
; 
; The winterp-lisp program below has this. I put a Motif text editor
; widget in there, and you can access the string in the widget by
; sending it message :GET_STRING. See code below for details.
; 
; 			--------------------
;
; Here's the primitive menu tree I cobbled together. It contains a menu-bar
; with cascade-buttons "hplstl" hplhcid" "hplptd" and each cascase button has
; a pulldown with entries "xterm" "hpterm" "emacs" "xmh" "edit file in
; editor". The latter will use the value you type into the text edit widget
; in order to determine which file to edit.... 
; 
; The menubar and pulldown entries are all mnemonically driven, which means
; you don't have to use a mouse if you don't want to.
; 

(let (toplevel_w main_w menubar_w edit_w)

  (setq toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	      "menutree"
	      :XMN_TITLE	"WINTERP: Menu Tree Demo"
	      :XMN_ICON_NAME	"W:menutree"
	      ))
  (setq main_w
	(send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
	      "mainw" toplevel_w
	      :XMN_SHOW_SEPARATOR T
	      ))
  (setq menubar_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
	      "menubar" main_w
	      :XMN_BUTTON_COUNT		3
	      :XMN_BUTTONS		#("hplstl" "hplhcid" "hplptd")
	      :XMN_BUTTON_MNEMONICS	#(#\s     #\h        #\p     )
	      :XMN_BUTTON_TYPE		#(:CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON)
	      ))
  (setq edit_w
	(send XM_TEXT_WIDGET_CLASS :new :managed "edit" main_w
	      ))

  (send main_w :set_values
	:XMN_MENU_BAR		menubar_w
	:XMN_WORK_WINDOW	edit_w
	)

  (send (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	      "hplstl" menubar_w
	      :XMN_POST_FROM_BUTTON	   0 ;post pulldown from menubar's 0th item
	      :XMN_BUTTON_COUNT		   5 ;create five buttons in this pulldown
	      :XMN_BUTTONS		   #("xterm" "hpterm" "emacs" "xmh" "edit file in editor")
	      :XMN_BUTTON_MNEMONICS	   #(#\x     #\h      #\e     #\m    #\e)
	      :XMN_BUTTON_TYPE		   #(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	      :XMN_BUTTON_ACCELERATORS	   #("Ctrl<Key>C" "Ctrl<Key>F" "Ctrl<Key>O" "Ctrl<Key>S" "Ctrl<Key>W")
	      :XMN_BUTTON_ACCELERATOR_TEXT #("^C" "^F" "^O" "^S" "^W")
	      )
	:add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
	'(CALLBACK_ENTRY_WIDGET)
	'(
	  ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
	  ;; where <#> is 0 ... (button-count-1).
	  ;; we use 'read' to return the FIXNUM <#> after truncating the
	  ;; 7 chars "button_" from the front of the string.
	  (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		(0 (system "echo 'xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 /usr/bin/X11/xterm \" &'"))
		(1 (system "echo 'xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 /usr/bin/X11/hpterm \" &'"))
		(2 (system "echo 'xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 /usr/local/bin/x11emacs \" &'"))
		(3 (system "echo 'xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 /usr/local/bin/X11/xmh \" &'"))
		(4 (system (concatenate 'string
					"echo 'xhost hplstl ; remsh hplstl \"env DISPLAY=hplnpm:0.0 gnuclient -q "
					(send edit_w :get_string)
					" &'")))
		(T (system "Error\n")))
	  ))


  (send (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	      "hplhcid" menubar_w
	      :XMN_POST_FROM_BUTTON	   1 ;post pulldown from menubar's first button.
	      :XMN_BUTTON_COUNT		   5 ;create five buttons in this pulldown
	      :XMN_BUTTONS		   #("xterm" "hpterm" "emacs" "xmh" "edit file in editor")
	      :XMN_BUTTON_MNEMONICS	   #(#\x     #\h      #\e     #\m    #\e)
	      :XMN_BUTTON_TYPE		   #(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	      :XMN_BUTTON_ACCELERATORS	   #("Ctrl<Key>C" "Ctrl<Key>F" "Ctrl<Key>O" "Ctrl<Key>S" "Ctrl<Key>W")
	      :XMN_BUTTON_ACCELERATOR_TEXT #("^C" "^F" "^O" "^S" "^W")
	      )
	:add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
	'(CALLBACK_ENTRY_WIDGET)
	'(
	  ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
	  ;; where <#> is 0 ... (button-count-1).
	  ;; we use 'read' to return the FIXNUM <#> after truncating the
	  ;; 7 chars "button_" from the front of the string.
	  (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		(0 (system "echo 'xhost hplhcid ; remsh hplhcid \"env DISPLAY=hplnpm:0.0 /usr/bin/X11/xterm \" &'"))
		(1 (system "echo 'xhost hplhcid ; remsh hplhcid \"env DISPLAY=hplnpm:0.0 /usr/bin/X11/hpterm \" &'"))
		(2 (system "echo 'xhost hplhcid ; remsh hplhcid \"env DISPLAY=hplnpm:0.0 /usr/local/bin/x11emacs \" &'"))
		(3 (system "echo 'xhost hplhcid ; remsh hplhcid \"env DISPLAY=hplnpm:0.0 /usr/local/bin/X11/xmh \" &'"))
		(4 (system (concatenate 'string
					"echo 'xhost hplhcid ; remsh hplhcid \"env DISPLAY=hplnpm:0.0 gnuclient -q "
					(send edit_w :get_string)
					" &'")))
		(T (system "Error\n")))
	  ))

  (send (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	      "hplptd" menubar_w
	      :XMN_POST_FROM_BUTTON	   2 ;post pulldown from menubar's second button.
	      :XMN_BUTTON_COUNT		   5 ;create five buttons in this pulldown
	      :XMN_BUTTONS		   #("xterm" "hpterm" "emacs" "xmh" "edit file in editor")
	      :XMN_BUTTON_MNEMONICS	   #(#\x     #\h      #\e     #\m    #\e)
	      :XMN_BUTTON_TYPE		   #(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	      :XMN_BUTTON_ACCELERATORS	   #("Ctrl<Key>C" "Ctrl<Key>F" "Ctrl<Key>O" "Ctrl<Key>S" "Ctrl<Key>W")
	      :XMN_BUTTON_ACCELERATOR_TEXT #("^C" "^F" "^O" "^S" "^W")
	      )
	:add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
	'(CALLBACK_ENTRY_WIDGET)
	'(
	  ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
	  ;; where <#> is 0 ... (button-count-1).
	  ;; we use 'read' to return the FIXNUM <#> after truncating the
	  ;; 7 chars "button_" from the front of the string.
	  (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		(0 (system "echo 'xhost hplptd ; remsh hplptd \"env DISPLAY=hplnpm:0.0 /usr/bin/X11/xterm \" &'"))
		(1 (system "echo 'xhost hplptd ; remsh hplptd \"env DISPLAY=hplnpm:0.0 /usr/bin/X11/hpterm \" &'"))
		(2 (system "echo 'xhost hplptd ; remsh hplptd \"env DISPLAY=hplnpm:0.0 /usr/local/bin/x11emacs \" &'"))
		(3 (system "echo 'xhost hplptd ; remsh hplptd \"env DISPLAY=hplnpm:0.0 /usr/local/bin/X11/xmh \" &'"))
		(4 (system (concatenate 'string
					"echo 'xhost hplptd ; remsh hplptd \"env DISPLAY=hplnpm:0.0 gnuclient -q "
					(send edit_w :get_string)
					" &'")))
		(T (system "Error\n")))
	  ))

  (send toplevel_w :realize)

  )
