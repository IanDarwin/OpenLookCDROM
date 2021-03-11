;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         FileSB.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/FileSB.lsp,v 2.4 1994/06/06 14:43:28 npm Exp $
; Description:  Demo of XM_FILE_SELECTION_BOX_WIDGET_CLASS
; Author:       Niels Mayer
; Created:      Sun Feb 10 20:32:40 1991
; Modified:     Sun Jun  5 17:28:59 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*, etc.

(let ()

(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "fsb"
	    :XMN_TITLE		"WINTERP: FileSB Widget Test"
	    :XMN_ICON_NAME	"W:FileSB"
	    ))

(setq fsb_w 
      (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :managed
	    "files" top_w
    ))

(setq cb1 (send fsb_w :set_callback :XMN_APPLY_CALLBACK
		(if *MOTIF-1.1-OR-LATER-P*
		    '(			;for Motif >= 1.1
		      CALLBACK_WIDGET
		      CALLBACK_REASON
		      CALLBACK_XEVENT
		      CALLBACK_VALUE
		      CALLBACK_LENGTH
		      CALLBACK_MASK
		      CALLBACK_MASK_LENGTH
		      CALLBACK_DIR
		      CALLBACK_DIR_LENGTH
		      CALLBACK_PATTERN
		      CALLBACK_PATTERN_LENGTH
		      )
		  '(			;for Motif 1.0
		    CALLBACK_WIDGET
		    CALLBACK_REASON
		    CALLBACK_XEVENT
		    CALLBACK_VALUE
		    CALLBACK_LENGTH
		    CALLBACK_MASK
		    CALLBACK_MASK_LENGTH
		    )
		  )
		'(
		  (format T "-------------------------------------------\n")
		  (format T "widget=~A, reason=~A, xevent=~A\n" 
			  callback_widget callback_reason callback_xevent)
		  (format T "CALLBACK_VALUE=~A\n"
			  (xm_string_get_l_to_r CALLBACK_VALUE))
		  (format T "CALLBACK_LENGTH=~A\n"
			  CALLBACK_LENGTH)
		  (format T "CALLBACK_MASK=~A\n"
			  (xm_string_get_l_to_r CALLBACK_MASK))
		  (format T "CALLBACK_MASK_LENGTH=~A\n"
			  CALLBACK_MASK_LENGTH)
		  (cond (*MOTIF-1.1-OR-LATER-P*
			 (format T "CALLBACK_DIR=~A\n"
				 (xm_string_get_l_to_r CALLBACK_DIR))
			 (format T "CALLBACK_DIR_LENGTH=~A\n"
				 CALLBACK_DIR_LENGTH)
			 (format T "CALLBACK_PATTERN=~A\n"
				 (xm_string_get_l_to_r CALLBACK_PATTERN))
			 (format T "CALLBACK_PATTERN_LENGTH=~A\n"
				 CALLBACK_PATTERN_LENGTH)
			 (let* ((items_array (send fsb_w :GET_DIR_LIST_ITEMS))
				(items_length (length items_array))
				)
			   (format T "Dir-List Items:\n")
			   (do ((i 0 (1+ i)))
			       ((= i items_length))
			       (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
			   )
			 (let* ((items_array  (send fsb_w :GET_FILE_LIST_ITEMS))
				(items_length (length items_array))
				)
			   (format T "File-List Items:\n")
			   (do ((i 0 (1+ i)))
			       ((= i items_length))
			       (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
			   )
			 ))
		  )
		))

(setq cb2 (send fsb_w :set_callback :XMN_OK_CALLBACK
		(if *MOTIF-1.1-OR-LATER-P*
		    '(			;for Motif >= 1.1
		      CALLBACK_WIDGET
		      CALLBACK_REASON
		      CALLBACK_XEVENT
		      CALLBACK_VALUE
		      CALLBACK_LENGTH
		      CALLBACK_MASK
		      CALLBACK_MASK_LENGTH
		      CALLBACK_DIR
		      CALLBACK_DIR_LENGTH
		      CALLBACK_PATTERN
		      CALLBACK_PATTERN_LENGTH
		      )
		  '(			;for Motif 1.0
		    CALLBACK_WIDGET
		    CALLBACK_REASON
		    CALLBACK_XEVENT
		    CALLBACK_VALUE
		    CALLBACK_LENGTH
		    CALLBACK_MASK
		    CALLBACK_MASK_LENGTH
		    )
		  )
		'(
		  (format T "-------------------------------------------\n")
		  (format T "widget=~A, reason=~A, xevent=~A\n" 
			  callback_widget callback_reason callback_xevent)
		  (format T "CALLBACK_VALUE=~A\n"
			  (xm_string_get_l_to_r CALLBACK_VALUE))
		  (format T "CALLBACK_LENGTH=~A\n"
			  CALLBACK_LENGTH)
		  (format T "CALLBACK_MASK=~A\n"
			  (xm_string_get_l_to_r CALLBACK_MASK))
		  (format T "CALLBACK_MASK_LENGTH=~A\n"
			  CALLBACK_MASK_LENGTH)
		  (cond (*MOTIF-1.1-OR-LATER-P*
			 (format T "CALLBACK_DIR=~A\n"
				 (xm_string_get_l_to_r CALLBACK_DIR))
			 (format T "CALLBACK_DIR_LENGTH=~A\n"
				 CALLBACK_DIR_LENGTH)
			 (format T "CALLBACK_PATTERN=~A\n"
				 (xm_string_get_l_to_r CALLBACK_PATTERN))
			 (format T "CALLBACK_PATTERN_LENGTH=~A\n"
				 CALLBACK_PATTERN_LENGTH)
			 (let* ((items_array (send fsb_w :GET_DIR_LIST_ITEMS))
				(items_length (length items_array))
				)
			   (format T "Dir-List Items:\n")
			   (do ((i 0 (1+ i)))
			       ((= i items_length))
			       (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
			   )
			 (let* ((items_array  (send fsb_w :GET_FILE_LIST_ITEMS))
				(items_length (length items_array))
				)
			   (format T "File-List Items:\n")
			   (do ((i 0 (1+ i)))
			       ((= i items_length))
			       (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
			   )
			 ))
		  )
		))

(setq cb3 (send fsb_w :set_callback :XMN_CANCEL_CALLBACK
		(if *MOTIF-1.1-OR-LATER-P*
		    '(			;for Motif >= 1.1
		      CALLBACK_WIDGET
		      CALLBACK_REASON
		      CALLBACK_XEVENT
		      CALLBACK_VALUE
		      CALLBACK_LENGTH
		      CALLBACK_MASK
		      CALLBACK_MASK_LENGTH
		      CALLBACK_DIR
		      CALLBACK_DIR_LENGTH
		      CALLBACK_PATTERN
		      CALLBACK_PATTERN_LENGTH
		      )
		  '(			;for Motif 1.0
		    CALLBACK_WIDGET
		    CALLBACK_REASON
		    CALLBACK_XEVENT
		    CALLBACK_VALUE
		    CALLBACK_LENGTH
		    CALLBACK_MASK
		    CALLBACK_MASK_LENGTH
		    )
		  )
		'(
		  (format T "-------------------------------------------\n")
		  (format T "widget=~A, reason=~A, xevent=~A\n" 
			  callback_widget callback_reason callback_xevent)
		  (format T "CALLBACK_VALUE=~A\n"
			  (xm_string_get_l_to_r CALLBACK_VALUE))
		  (format T "CALLBACK_LENGTH=~A\n"
			  CALLBACK_LENGTH)
		  (format T "CALLBACK_MASK=~A\n"
			  (xm_string_get_l_to_r CALLBACK_MASK))
		  (format T "CALLBACK_MASK_LENGTH=~A\n"
			  CALLBACK_MASK_LENGTH)
		  (cond (*MOTIF-1.1-OR-LATER-P*
			 (format T "CALLBACK_DIR=~A\n"
				 (xm_string_get_l_to_r CALLBACK_DIR))
			 (format T "CALLBACK_DIR_LENGTH=~A\n"
				 CALLBACK_DIR_LENGTH)
			 (format T "CALLBACK_PATTERN=~A\n"
				 (xm_string_get_l_to_r CALLBACK_PATTERN))
			 (format T "CALLBACK_PATTERN_LENGTH=~A\n"
				 CALLBACK_PATTERN_LENGTH)
			 (let* ((items_array (send fsb_w :GET_DIR_LIST_ITEMS))
				(items_length (length items_array))
				)
			   (format T "Dir-List Items:\n")
			   (do ((i 0 (1+ i)))
			       ((= i items_length))
			       (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
			   )
			 (let* ((items_array  (send fsb_w :GET_FILE_LIST_ITEMS))
				(items_length (length items_array))
				)
			   (format T "File-List Items:\n")
			   (do ((i 0 (1+ i)))
			       ((= i items_length))
			       (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
			   )
			 ))
		  )
		))

(setq cb4 (send fsb_w :set_callback :XMN_NO_MATCH_CALLBACK
		(if *MOTIF-1.1-OR-LATER-P*
		    '(			;for Motif >= 1.1
		      CALLBACK_WIDGET
		      CALLBACK_REASON
		      CALLBACK_XEVENT
		      CALLBACK_VALUE
		      CALLBACK_LENGTH
		      CALLBACK_MASK
		      CALLBACK_MASK_LENGTH
		      CALLBACK_DIR
		      CALLBACK_DIR_LENGTH
		      CALLBACK_PATTERN
		      CALLBACK_PATTERN_LENGTH
		      )
		  '(			;for Motif 1.0
		    CALLBACK_WIDGET
		    CALLBACK_REASON
		    CALLBACK_XEVENT
		    CALLBACK_VALUE
		    CALLBACK_LENGTH
		    CALLBACK_MASK
		    CALLBACK_MASK_LENGTH
		    CALLBACK_DIR
		    CALLBACK_DIR_LENGTH
		    CALLBACK_PATTERN
		    CALLBACK_PATTERN_LENGTH
		    )
		  )
		'(
		  (format T "-------------------------------------------\n")
		  (format T "widget=~A, reason=~A, xevent=~A\n" 
			  callback_widget callback_reason callback_xevent)
		  (format T "CALLBACK_VALUE=~A\n"
			  (xm_string_get_l_to_r CALLBACK_VALUE))
		  (format T "CALLBACK_LENGTH=~A\n"
			  CALLBACK_LENGTH)
		  (format T "CALLBACK_MASK=~A\n"
			  (xm_string_get_l_to_r CALLBACK_MASK))
		  (format T "CALLBACK_MASK_LENGTH=~A\n"
			  CALLBACK_MASK_LENGTH)
		  (cond (*MOTIF-1.1-OR-LATER-P*
			 (format T "CALLBACK_DIR=~A\n"
				 (xm_string_get_l_to_r CALLBACK_DIR))
			 (format T "CALLBACK_DIR_LENGTH=~A\n"
				 CALLBACK_DIR_LENGTH)
			 (format T "CALLBACK_PATTERN=~A\n"
				 (xm_string_get_l_to_r CALLBACK_PATTERN))
			 (format T "CALLBACK_PATTERN_LENGTH=~A\n"
				 CALLBACK_PATTERN_LENGTH)
			 (let* ((items_array (send fsb_w :GET_DIR_LIST_ITEMS))
				(items_length (length items_array))
				)
			   (format T "Dir-List Items:\n")
			   (do ((i 0 (1+ i)))
			       ((= i items_length))
			       (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
			   )
			 (let* ((items_array  (send fsb_w :GET_FILE_LIST_ITEMS))
				(items_length (length items_array))
				)
			   (format T "File-List Items:\n")
			   (do ((i 0 (1+ i)))
			       ((= i items_length))
			       (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
			   )
			 ))

		  )
		))

(send top_w :realize)

(if *MOTIF-1.1-OR-LATER-P*
    (let* ((items_array (send fsb_w :GET_DIR_LIST_ITEMS))
	   (items_length (length items_array))
	   )
      (do ((i 0 (1+ i)))
	  ((= i items_length))
	  (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
      )
  )

(if *MOTIF-1.1-OR-LATER-P*
    (let* ((items_array  (send fsb_w :GET_FILE_LIST_ITEMS))
	   (items_length (length items_array))
	   )
      (do ((i 0 (1+ i)))
	  ((= i items_length))
	  (format T "\t~A\n" (xm_string_get_l_to_r (aref items_array i))))
      )
  )

)


(send fsb_w :do_search "/tmp/*")

(format T ":DIALOG_FILTER_LABEL==~A\n"
	(send fsb_w :get_child :DIALOG_FILTER_LABEL))
(format T ":DIALOG_FILTER_TEXT==~A\n"
	(send fsb_w :get_child :DIALOG_FILTER_TEXT))
(if *MOTIF-1.1-OR-LATER-P*
    (format T ":DIALOG_DIR_LIST==~A\n"
	    (send fsb_w :get_child :DIALOG_DIR_LIST)))
(if *MOTIF-1.1-OR-LATER-P*
    (format T ":DIALOG_DIR_LIST_LABEL==~A\n"
	    (send fsb_w :get_child :DIALOG_DIR_LIST_LABEL)))
(format T ":DIALOG_LIST==~A\n"
	(send fsb_w :get_child :DIALOG_LIST))
(format T ":DIALOG_LIST_LABEL==~A\n"
	(send fsb_w :get_child :DIALOG_LIST_LABEL))
(format T ":DIALOG_SELECTION_LABEL==~A\n"
	(send fsb_w :get_child :DIALOG_SELECTION_LABEL))
(format T ":DIALOG_WORK_AREA==~A\n"
	(send fsb_w :get_child :DIALOG_WORK_AREA))
(format T ":DIALOG_TEXT==~A\n"
	(send fsb_w :get_child :DIALOG_TEXT))
(format T ":DIALOG_SEPARATOR==~A\n"
	(send fsb_w :get_child :DIALOG_SEPARATOR))
(format T ":DIALOG_OK_BUTTON==~A\n"
	(send fsb_w :get_child :DIALOG_OK_BUTTON))
(format T ":DIALOG_APPLY_BUTTON==~A\n"
	(send fsb_w :get_child :DIALOG_APPLY_BUTTON))
(format T ":DIALOG_CANCEL_BUTTON==~A\n"
	(send fsb_w :get_child :DIALOG_CANCEL_BUTTON))
(format T ":DIALOG_HELP_BUTTON==~A\n"
	(send fsb_w :get_child :DIALOG_HELP_BUTTON))
(format T ":DIALOG_DEFAULT_BUTTON==~A\n"
	(send fsb_w :get_child :DIALOG_DEFAULT_BUTTON))

