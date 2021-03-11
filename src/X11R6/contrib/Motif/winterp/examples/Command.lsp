;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         Command.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/Command.lsp,v 2.4 1994/06/06 14:43:27 npm Exp $
; Description:  Demo of XM_COMMAND_WIDGET_CLASS
; Author:       Niels Mayer
; Created:      Sun Feb 10 20:32:15 1991
; Modified:     Sun Jun  5 17:29:03 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.2-OR-LATER-P*

(let ()

(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "command-test"
	    :XMN_TITLE		"WINTERP: Command Widget Test"
	    :XMN_ICON_NAME	"W:Command"
	    ))

(setq command_w
      (send XM_COMMAND_WIDGET_CLASS :new :managed
	    "command" top_w
	    :XMN_PROMPT_STRING		"Geo-Political choices"
	    :XMN_COMMAND		"quagmire"
	    :XMN_HISTORY_ITEMS		#("dogma"
					  "cold war"
					  "new world odor"
					  "orwell's 1984"
					  "certain doom"
					  "imperialism"
					  "stupidity"
					  "interventionism")
	    :XMN_HISTORY_ITEM_COUNT	6
	    :XMN_HISTORY_MAX_ITEMS	10
	    :XMN_HISTORY_VISIBLE_ITEM_COUNT 5
	    ))

(if *MOTIF-1.2-OR-LATER-P*
    (send XM_LABEL_GADGET_CLASS :new :managed "workarea" command_w
	  :XMN_LABEL_STRING "This is an extra\nworkarea child. Available\nin Motif >= 1.2"
	  )
  )

(send command_w :set_callback :XmN_command_Changed_Callback 
      '(CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT CALLBACK_VALUE CALLBACK_LENGTH)
      '(
	(format T "Command Callback occured.\n\twidget=~A;\n\treason=~A;\n\tevent=~A;\n\tvalue=~A;\n\txmstr-length=~A\n"
		CALLBACK_WIDGET
		CALLBACK_REASON
		CALLBACK_XEVENT
		(xm_string_get_l_to_r CALLBACK_VALUE)
		CALLBACK_LENGTH)
	))

(send command_w :set_callback :XmN_command_Entered_Callback
      '(CALLBACK_WIDGET CALLBACK_REASON	CALLBACK_XEVENT CALLBACK_VALUE CALLBACK_LENGTH)
      '(
	(format T "Command Callback occured.\n\twidget=~A;\n\treason=~A;\n\tevent=~A;\n\tvalue=~A;\n\txmstr-length=~A\nHistory Items:"
		CALLBACK_WIDGET
		CALLBACK_REASON
		CALLBACK_XEVENT
		(xm_string_get_l_to_r CALLBACK_VALUE)
		CALLBACK_LENGTH)

	(let* ((items_array (send command_w :get_history_items))
	       (items_length (length items_array))
	       )
	  (do ((i 0 (1+ i)))
	      ((= i items_length))
	      (format t "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
	  )
	))

(send command_w :get_child :DIALOG_COMMAND_TEXT)

(send command_w :get_child :DIALOG_HISTORY_LIST)

(send command_w :get_child :DIALOG_PROMPT_LABEL)

(if *MOTIF-1.2-OR-LATER-P*
    (send command_w :get_child :DIALOG_WORK_AREA)
  )

(send command_w :set_value "freedom ")

(send command_w :append_value "love ")
(send command_w :append_value "and equality ")


(let* ((items_array (send command_w :get_history_items))
       (items_length (length items_array))
       )
  (do ((i 0 (1+ i)))
       ((= i items_length))
       (print (xm_string_get_l_to_r (aref items_array i))))
  )

(send command_w :error "<<ERROR: invalid choice>>")

(send top_w :realize)

)
