;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         SelectioB.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/SelectioB.lsp,v 2.6 1994/06/06 14:43:25 npm Exp $
; Description:  tests XM_SELECTION_BOX_WIDGET_CLASS methods and callbacks.
;		Just load this file to see examples.
; Author:       Niels Mayer
; Created:      Sun Feb 10 20:34:42 1991
; Modified:     Sun Jun  5 18:12:30 1994 (Niels Mayer) npm@indeed
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

(let ()

(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "sbshl"
	    :XMN_TITLE		"WINTERP XmSelectionBox Widget Test"
	    :XMN_ICON_NAME	"W:SelectioB"
	    ))

(setq selection_w
      (send XM_SELECTION_BOX_WIDGET_CLASS :new :managed
	    "selection" top_w
	    :XMN_SELECTION_LABEL_STRING	"Geo-Political choices"
	    :XMN_LIST_ITEMS		#("dogma"
					  "cold war"
					  "new world odor"
					  "orwell's 1984"
					  "certain doom"
					  "imperialism"
					  "stupidity"
					  "interventionism")
	    :XMN_LIST_ITEM_COUNT	6
	    :XMN_LIST_VISIBLE_ITEM_COUNT 5
	    ))

(send (send selection_w :get_child :dialog_apply_button) :manage)

(send selection_w :set_callback :XMN_APPLY_CALLBACK
      '(CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT CALLBACK_VALUE CALLBACK_LENGTH)
      '(
	(format T "Apply Callback occured.\n\twidget=~A;\n\treason=~A;\n\tevent=~A;\n\tvalue=~A;\n\txmstr-length=~A\n"
		CALLBACK_WIDGET
		CALLBACK_REASON
		CALLBACK_XEVENT
		(xm_string_get_l_to_r CALLBACK_VALUE)
		CALLBACK_LENGTH)
	))

(send selection_w :set_callback :XMN_CANCEL_CALLBACK
      '(CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT CALLBACK_VALUE CALLBACK_LENGTH)
      '(
	(format T "Cancel Callback occured.\n\twidget=~A;\n\treason=~A;\n\tevent=~A;\n\tvalue=~A;\n\txmstr-length=~A\n"
		CALLBACK_WIDGET
		CALLBACK_REASON
		CALLBACK_XEVENT
		(xm_string_get_l_to_r CALLBACK_VALUE)
		CALLBACK_LENGTH)
	))

(send selection_w :set_callback :XMN_OK_CALLBACK
      '(CALLBACK_WIDGET CALLBACK_REASON	CALLBACK_XEVENT CALLBACK_VALUE CALLBACK_LENGTH)
      '(
	(format T "Ok Callback occured.\n\twidget=~A;\n\treason=~A;\n\tevent=~A;\n\tvalue=~A;\n\txmstr-length=~A\nHistory Items:"
		CALLBACK_WIDGET
		CALLBACK_REASON
		CALLBACK_XEVENT
		(xm_string_get_l_to_r CALLBACK_VALUE)
		CALLBACK_LENGTH)

	(let* ((items_array (send CALLBACK_WIDGET :get_list_items))
	       (items_length (length items_array))
	       )
	  (do ((i 0 (1+ i)))
	      ((= i items_length))
	      (format t "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
	  )
	))



(format T ":DIALOG_LIST==~A\n"
	(send selection_w :get_child :DIALOG_LIST))

(format T ":DIALOG_LIST_LABEL==~A\n"
	(send selection_w :get_child :DIALOG_LIST_LABEL))

(format T ":DIALOG_SELECTION_LABEL==~A\n"
	(send selection_w :get_child :DIALOG_SELECTION_LABEL))

(format T ":DIALOG_WORK_AREA==~A\n"
	(send selection_w :get_child :DIALOG_WORK_AREA))

(format T ":DIALOG_TEXT==~A\n"
	(send selection_w :get_child :DIALOG_TEXT))

(format T ":DIALOG_SEPARATOR==~A\n"
	(send selection_w :get_child :DIALOG_SEPARATOR))

(format T ":DIALOG_OK_BUTTON==~A\n"
	(send selection_w :get_child :DIALOG_OK_BUTTON))

(format T ":DIALOG_APPLY_BUTTON==~A\n"
	(send selection_w :get_child :DIALOG_APPLY_BUTTON))

(format T ":DIALOG_CANCEL_BUTTON==~A\n"
	(send selection_w :get_child :DIALOG_CANCEL_BUTTON))

(format T ":DIALOG_HELP_BUTTON==~A\n"
	(send selection_w :get_child :DIALOG_HELP_BUTTON))

(format T ":DIALOG_DEFAULT_BUTTON==~A\n"
	(send selection_w :get_child :DIALOG_DEFAULT_BUTTON))



(let* ((items_array (send selection_w :get_list_items))
       (items_length (length items_array))
       )
  (do ((i 0 (1+ i)))
      ((= i items_length))
      (print (xm_string_get_l_to_r (aref items_array i))))
  )

(send top_w :realize)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ()

(setq selection1_w
      (send XM_SELECTION_BOX_WIDGET_CLASS :new :managed :dialog
	    "selection" top_w
	    :XMN_SELECTION_LABEL_STRING	"Democracy implies freedom to choose from the above::"
	    :XMN_MUST_MATCH		t
	    :XMN_LIST_ITEMS		#("dogma"
					  "cold war"
					  "new world odor"
					  "orwell's 1984"
					  "certain doom"
					  "imperialism"
					  "stupidity"
					  "interventionism")
	    :XMN_LIST_ITEM_COUNT	6
	    :XMN_LIST_VISIBLE_ITEM_COUNT 5
	    ))

(send selection1_w :set_callback :XMN_NO_MATCH_CALLBACK
      '(CALLBACK_WIDGET CALLBACK_REASON	CALLBACK_XEVENT CALLBACK_VALUE CALLBACK_LENGTH)
      '(
	(format T "No Match Callback occured.\n\twidget=~A;\n\treason=~A;\n\tevent=~A;\n\tvalue=~A;\n\txmstr-length=~A\nHistory Items:"
		CALLBACK_WIDGET
		CALLBACK_REASON
		CALLBACK_XEVENT
		(xm_string_get_l_to_r CALLBACK_VALUE)
		CALLBACK_LENGTH)

	(let* ((items_array (send CALLBACK_WIDGET :get_list_items))
	       (items_length (length items_array))
	       )
	  (do ((i 0 (1+ i)))
	      ((= i items_length))
	      (format t "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
	  )
	(send CALLBACK_WIDGET :manage)
	))

(send selection1_w :manage)
(send selection1_w :set_callback :XMN_OK_CALLBACK
      '(CALLBACK_WIDGET CALLBACK_REASON	CALLBACK_XEVENT CALLBACK_VALUE CALLBACK_LENGTH)
      '(
	(format T "Ok Callback occured.\n\twidget=~A;\n\treason=~A;\n\tevent=~A;\n\tvalue=~A;\n\txmstr-length=~A\nHistory Items:"
		CALLBACK_WIDGET
		CALLBACK_REASON
		CALLBACK_XEVENT
		(xm_string_get_l_to_r CALLBACK_VALUE)
		CALLBACK_LENGTH)

	(let* ((items_array (send CALLBACK_WIDGET :get_list_items))
	       (items_length (length items_array))
	       )
	  (do ((i 0 (1+ i)))
	      ((= i items_length))
	      (format t "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
	  )
	))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ()

(setq selection2_w
      (send XM_SELECTION_BOX_WIDGET_CLASS :new :managed :prompt_dialog
	    "selection" top_w
	    :XMN_SELECTION_LABEL_STRING "Freedom of speech -- Just watch what you say:"
	    ))

(send selection2_w :set_callback :XMN_OK_CALLBACK
      '(CALLBACK_WIDGET CALLBACK_REASON	CALLBACK_XEVENT CALLBACK_VALUE CALLBACK_LENGTH)
      '(
	(format T "Ok Callback occured.\n\twidget=~A;\n\treason=~A;\n\tevent=~A;\n\tvalue=~A;\n\txmstr-length=~A\nHistory Items:"
		CALLBACK_WIDGET
		CALLBACK_REASON
		CALLBACK_XEVENT
		(xm_string_get_l_to_r CALLBACK_VALUE)
		CALLBACK_LENGTH)
	))

(format T ":DIALOG_LIST==~A\n"
	(send selection2_w :get_child :DIALOG_LIST))

(format T ":DIALOG_LIST_LABEL==~A\n"
	(send selection2_w :get_child :DIALOG_LIST_LABEL))

(format T ":DIALOG_SELECTION_LABEL==~A\n"
	(send selection2_w :get_child :DIALOG_SELECTION_LABEL))

(format T ":DIALOG_WORK_AREA==~A\n"
	(send selection2_w :get_child :DIALOG_WORK_AREA))

(format T ":DIALOG_TEXT==~A\n"
	(send selection2_w :get_child :DIALOG_TEXT))

(format T ":DIALOG_SEPARATOR==~A\n"
	(send selection2_w :get_child :DIALOG_SEPARATOR))

(format T ":DIALOG_OK_BUTTON==~A\n"
	(send selection2_w :get_child :DIALOG_OK_BUTTON))

(format T ":DIALOG_APPLY_BUTTON==~A\n"
	(send selection2_w :get_child :DIALOG_APPLY_BUTTON))

(format T ":DIALOG_CANCEL_BUTTON==~A\n"
	(send selection2_w :get_child :DIALOG_CANCEL_BUTTON))

(format T ":DIALOG_HELP_BUTTON==~A\n"
	(send selection2_w :get_child :DIALOG_HELP_BUTTON))

(format T ":DIALOG_DEFAULT_BUTTON==~A\n"
	(send selection2_w :get_child :DIALOG_DEFAULT_BUTTON))

)
