;; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         menutear.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/menutear.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  menutree.lsp with tear-off menus enabled...
; Author:       Niels P. Mayer
; Created:      1994
; Modified:     Sun Jun  5 19:02:13 1994 (Niels Mayer) npm@indeed
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

;;; Note: some code in this file shouldn't be necessary -- it
;;; works around a BUG in SGI Irix 5.2 version of Motif 1.2.3:
;;; When toplevel window for this app is closed, while tearoffs
;;; are popped up, a core dump will occur. Closing the tear-off
;;; windows first will prevent core dump from occuring:
;;;
;;; indeed-6-.../widgit/examples> dbx `which winterp` core
;;; dbx version 3.18 Feb 14 1994 00:52:47
;;; Type 'help' for help.
;;; Process died at pc 0xf703b38 of signal : Bus error
;;; [using memory image in core]
;;; (dbx) where
;;; >  0 DestroyPassiveList(0xafa50004, 0xe3b5a18, 0x10042868, 0x10030f30, 0x100b459c) ["PassivGrab.c":445, 0xf703b34]
;;;    1 _XtDestroyServerGrabs(0xafa50004, 0x100a2aa0, 0x0, 0x10030f30, 0xfb5a230) ["PassivGrab.c":479, 0xf703c4c]
;;;    2 XtCallCallbackList(0xafa50004, 0x10, 0x100b76f8, 0x10030f30, 0x100b776c) ["Callback.c":528, 0xf6de070]
;;;    3 Phase2Callbacks(0xafa50004, 0xf6f20f0, 0x0, 0x10030f30, 0x0) ["Destroy.c":70, 0xf6f20ec]
;;;    4 Recursive(0xafa50004, 0x0, 0x0, 0x10030f30, 0x0) ["Destroy.c":57, 0xf6ea99c]
;;;    5 Recursive(0xafa50004, 0xf6cc7c0, 0x0, 0x10030f30, 0x0) ["Destroy.c":45, 0xf6ea918]
;;;    6 Recursive(0xafa50004, 0xf6e12c4, 0xe2d8c74, 0x10030f30, 0x10001) ["Destroy.c":52, 0xf6ea978]
;;;    7 XtPhase2Destroy(0xafa50004, 0xf6ca908, 0x100b7ed0, 0x10030f30, 0x1004360c) ["Destroy.c":171, 0xf6eac90]
;;;    8 _XtDoPhase2Destroy(0xafa50004, 0x1, 0x0, 0x10030f30, 0x100283e0) ["Destroy.c":237, 0xf6efa30]
;;;    9 XtDispatchEvent(0x7fffae38, 0x7fffae38, 0xf71fbf0, 0x10030f30, 0x0) ["Event.c":1081, 0xf6ce408]
;;;   10 XtAppMainLoop(0x7fffae38, 0xf700301, 0x38, 0x10030f30, 0x0) ["Event.c":1201, 0xf7003e8]
;;;   11 main(0x1, 0x7fffaf1c, 0x0, 0x10030f30, 0x100283e0) ["winterp.c":1080, 0x44dda8]
;;;
;;; --------------------
;;;
;;; The fix is to prevent the standard window manager destroy from occurring by using the following
;;; resources for creating this apps TOP_LEVEL_SHELL_WIDGET_CLASS instance
;;;	    :XMN_DELETE_RESPONSE	:DO_NOTHING
;;;	    :XMN_MWM_FUNCTIONS	(logior MWM_FUNC_RESIZE MWM_FUNC_MOVE
;;;					MWM_FUNC_MINIMIZE MWM_FUNC_MAXIMIZE)
;;; also a "Quit!" pushbutton below has a callback which explicitly destroys all the 
;;; pulldown menus first, then destroys the TOP_LEVEL_SHELL_WIDGET_CLASS instance...

(require "lib-utils/initialize")	;define WIDGET_CLASS method :GET
(require "lib-utils/motif-vers")

(if (not *MOTIF-1.2-OR-LATER-P*)
    (error "The functionality in file menutear.lsp requires Motif version >= 1.2.")
  )

 
(defun menutear:setup-menu-activate-deactivate-frobs (pd_w)
;;;(toplevel_w pd_w)
;;;  (let (callback-obj)
  (send pd_w :add_callback :XMN_TEAR_OFF_MENU_ACTIVATE_CALLBACK
	'(CALLBACK_WIDGET CALLBACK_REASON)
	`(
	  (format T ":XMN_TEAR_OFF_MENU_ACTIVATE_CALLBACK, widget=~A, reason=~A\n"
		  CALLBACK_WIDGET CALLBACK_REASON)
;;; -- Attempt to fix problem w/ coredumping on Irix 5.2 -- occurs only when
;;; -- toplevel window for this app is closed, while tearoffs are popped up.	    
;;;  	    (setq callback-obj
;;;  		  (send ,toplevel_w :add_callback :XMN_DESTROY_CALLBACK '()
;;;  			`((send ,pd_w :unmanage))))
	  ))
  (send pd_w :add_callback :XMN_TEAR_OFF_MENU_DEACTIVATE_CALLBACK
	'(CALLBACK_WIDGET CALLBACK_REASON)
	`(
	  (format T ":XMN_TEAR_OFF_MENU_DEACTIVATE_CALLBACK, widget=~A, reason=~A\n"
		  CALLBACK_WIDGET CALLBACK_REASON)

;;;	    (if (send pd_w :exists_p) (xt_remove_callback callback-obj))
	  ))
;;;    )
  )

(let (toplevel_w main_w menubar_w pd_0 pd_1 pd_2 edit_w pb_w)

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	    "menutree"
	    :XMN_TITLE	"WINTERP: Menu Tree Demo"
	    :XMN_ICON_NAME	"W:menutree"
	    :XMN_DELETE_RESPONSE	:DO_NOTHING
	    :XMN_MWM_FUNCTIONS	(logior MWM_FUNC_RESIZE MWM_FUNC_MOVE
					MWM_FUNC_MINIMIZE MWM_FUNC_MAXIMIZE)
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
(setq pb_w
      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	    "quit" main_w
	    :XMN_LABEL_TYPE :string
	    :XMN_LABEL_STRING "Quit!"
	    ))
(setq edit_w
      (send XM_TEXT_WIDGET_CLASS :new :managed "edit" main_w
	    ))

(send main_w :set_values
      :XMN_MENU_BAR	menubar_w
      :XMN_WORK_WINDOW	pb_w
      :XMN_MESSAGE_WINDOW edit_w
      )

(send pb_w :add_callback :xmn_activate_callback '()
      '(
	(format T "exiting menutear.lsp\n")
	(send pd_0 :destroy)
	(send pd_1 :destroy)
	(send pd_2 :destroy)
	(xt_add_timeout 0 '((send toplevel_w :destroy)))
	))

(setq pd_0
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	    "hplstl" menubar_w
	    :XMN_TEAR_OFF_MODEL	:tear_off_enabled
	    :XMN_POST_FROM_BUTTON	0 ;post pulldown from menubar's 0th item
	    :XMN_BUTTON_COUNT		5 ;create five buttons in this pulldown
	    :XMN_BUTTONS		#("xterm" "hpterm" "emacs" "xmh" "edit file in editor")
	    :XMN_BUTTON_MNEMONICS	#(#\x     #\h      #\e     #\m    #\e)
	    :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	    ))
(send pd_0 :add_callback :XMN_ENTRY_CALLBACK ;use this instead of XmNsimpleCallback
      '(CALLBACK_ENTRY_WIDGET)
      '(
	(case (send CALLBACK_ENTRY_WIDGET :get :XMN_POSITION_INDEX)
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

(menutear:setup-menu-activate-deactivate-frobs pd_0)

(setq pd_1
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	    "hplhcid" menubar_w
	    :XMN_TEAR_OFF_MODEL	:tear_off_enabled
	    :XMN_POST_FROM_BUTTON	1 ;post pulldown from menubar's first button.
	    :XMN_BUTTON_COUNT		5 ;create five buttons in this pulldown
	    :XMN_BUTTONS		#("xterm" "hpterm" "emacs" "xmh" "edit file in editor")
	    :XMN_BUTTON_MNEMONICS	#(#\x     #\h      #\e     #\m    #\e)
	    :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	    ))
(send pd_1 :add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
      '(CALLBACK_ENTRY_WIDGET)
      '(
	(case (send CALLBACK_ENTRY_WIDGET :get :XMN_POSITION_INDEX)
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

(menutear:setup-menu-activate-deactivate-frobs pd_1)

(setq pd_2
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	    "hplptd" menubar_w
	    :XMN_TEAR_OFF_MODEL	:tear_off_enabled
	    :XMN_POST_FROM_BUTTON	2 ;post pulldown from menubar's second button.
	    :XMN_BUTTON_COUNT		5 ;create five buttons in this pulldown
	    :XMN_BUTTONS		#("xterm" "hpterm" "emacs" "xmh" "edit file in editor")
	    :XMN_BUTTON_MNEMONICS	#(#\x     #\h      #\e     #\m    #\e)
	    :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	    ))
(send pd_2 :add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
      '(CALLBACK_ENTRY_WIDGET)
      '(
	(case (send CALLBACK_ENTRY_WIDGET :get :XMN_POSITION_INDEX)
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

(menutear:setup-menu-activate-deactivate-frobs pd_2)

(send toplevel_w :realize)

)
