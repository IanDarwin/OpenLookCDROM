;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         Text.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/Text.lsp,v 2.5 1994/06/06 14:43:23 npm Exp $
; Description:	Tests XM_TEXT_WIDGET_CLASS methods and callbacks. Also
;		demonstrates use of :CALL_ACTION_PROC to invoke a widget action
;		procedure programmatically. Just load this file to see examples.
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:30:56 1989
; Modified:     Sun Jun  5 18:15:18 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*

(defun find-file (file)
  (let*
      (;; loc vars
       (top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "edshl"
	      :XMN_GEOMETRY	"500x500+1+1"
	      :XMN_TITLE	file
	      :XMN_ICON_NAME	file
	      ))
       (te_w
	(send XM_TEXT_WIDGET_CLASS :new :managed :scrolled "text" top_w
	      :XMN_EDIT_MODE :MULTI_LINE_EDIT    
;;;	      :XMN_FONT_LIST "hp8.10x20b"
	      ))
       (fp
	(open file :direction :input)
	)
       (inspos 0)
       text_line
       )


    (if (null fp)
	(error "Can't open file." file))

    (loop
     (if (null (setq text_line (read-line fp)))
	 (return))
     (let* ((str (concatenate 'string text_line "\n"))
	    (len (length str)))
       (send te_w :replace inspos inspos str)
       (setq inspos (+ len inspos))
       (send te_w :show_position inspos)
       )
     )
    (close fp)
    (send top_w :realize)
    te_w				;return value
    )
  )

(setq tw (find-file "/etc/passwd"))

(send tw :set_callback :XMN_LOSING_FOCUS_CALLBACK
      '(CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_DOIT
	CALLBACK_CUR_INSERT
	CALLBACK_NEW_INSERT
	CALLBACK_START_POS
	CALLBACK_END_POS
;;;	CALLBACK_TEXT
	)
      '((format T "widget=~A\nreason=~A\nxevent=~A\n" 
		CALLBACK_WIDGET	CALLBACK_REASON	CALLBACK_XEVENT)
	(format T "cur_insert=~A\nnew_insert=~A\n" 
		CALLBACK_CUR_INSERT CALLBACK_NEW_INSERT)
	(format T "start_pos=~A\nend_pos=~A\n"
		CALLBACK_START_POS CALLBACK_END_POS)
	(format T "doit=~A\n"
		CALLBACK_DOIT)
;;;	(setq CALLBACK_DOIT nil)
	))

(send tw :set_callback :XMN_MODIFY_VERIFY_CALLBACK
      '(CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_DOIT
	CALLBACK_CUR_INSERT
	CALLBACK_NEW_INSERT
	CALLBACK_START_POS
	CALLBACK_END_POS
	CALLBACK_TEXT
	)
      '((format T "widget=~A\nreason=~A\nxevent=~A\n" 
		CALLBACK_WIDGET	CALLBACK_REASON	CALLBACK_XEVENT)
	(format T "cur_insert=~A\nnew_insert=~A\n" 
		CALLBACK_CUR_INSERT CALLBACK_NEW_INSERT)
	(format T "start_pos=~A\nend_pos=~A\n"
		CALLBACK_START_POS CALLBACK_END_POS)
	(format T "doit=~A\ntext='~A'\n"
		CALLBACK_DOIT CALLBACK_TEXT)
;;;	(setq CALLBACK_DOIT nil)
	))


(send tw :set_callback :XMN_MOTION_VERIFY_CALLBACK
      '(CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_DOIT
	CALLBACK_CUR_INSERT
	CALLBACK_NEW_INSERT
;;;	CALLBACK_START_POS
;;;	CALLBACK_END_POS
;;;	CALLBACK_TEXT
	)
      '((format T "widget=~A\nreason=~A\nxevent=~A\n" 
		CALLBACK_WIDGET	CALLBACK_REASON	CALLBACK_XEVENT)
	(format T "cur_insert=~A\nnew_insert=~A\n" 
		CALLBACK_CUR_INSERT CALLBACK_NEW_INSERT)
	(format T "doit=~A\n"
		CALLBACK_DOIT)
;;;	(setq CALLBACK_DOIT nil)
	))


(send tw :set_callback :XMN_VALUE_CHANGED_CALLBACK
      '(CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
;;;	CALLBACK_DOIT
;;;	CALLBACK_CUR_INSERT
;;;	CALLBACK_NEW_INSERT
;;;	CALLBACK_START_POS
;;;	CALLBACK_END_POS
;;;	CALLBACK_TEXT
	)
      '(
	(format T "widget=~A\nreason=~A\nxevent=~A\n" 
		CALLBACK_WIDGET	CALLBACK_REASON	CALLBACK_XEVENT)
	))


(send tw :set_callback :XMN_FOCUS_CALLBACK
      '(CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
;;;	CALLBACK_DOIT
;;;	CALLBACK_CUR_INSERT
;;;	CALLBACK_NEW_INSERT
;;;	CALLBACK_START_POS
;;;	CALLBACK_END_POS
;;;	CALLBACK_TEXT
	)
      '(
	(format T "widget=~A\nreason=~A\nxevent=~A\n" 
		CALLBACK_WIDGET	CALLBACK_REASON	CALLBACK_XEVENT)
	))

(let ()

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "ed2shl"
	    ))
(setq rc_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
		       "rc" toplevel_w
		       :XMN_ORIENTATION :VERTICAL
		       :XMN_PACKING :PACK_TIGHT
		       :XMN_ENTRY_ALIGNMENT :ALIGNMENT_CENTER
		       ))
(setq te_line_w
      (send XM_TEXT_WIDGET_CLASS :new :managed rc_w
	    :XMN_EDIT_MODE :SINGLE_LINE_EDIT
	    ))

(send te_line_w :set_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_WIDGET)
      '(
	(send te_w :read_file (send CALLBACK_WIDGET :get_string))
	))

(setq te_w
      (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled rc_w
	    :XMN_EDIT_MODE :MULTI_LINE_EDIT
	    :XMN_HEIGHT 400
	    :XMN_WIDTH  300
	    ))



(setq prevbut_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	    :XMN_LABEL_STRING "Prev Page"
	    ))

(send prevbut_w :set_callback :xmn_activate_callback '(CALLBACK_XEVENT)
      '(
	(send te_w :CALL_ACTION_PROC "previous-page"  CALLBACK_XEVENT)
	)
      )

(setq nextbut_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	    :XMN_LABEL_STRING "Next Page"
	    ))

(send nextbut_w :set_callback :xmn_activate_callback '(CALLBACK_XEVENT)
      '(
	(send te_w :CALL_ACTION_PROC "next-page"  CALLBACK_XEVENT)
	)
      )

(setq insertbut_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	    :XMN_LABEL_STRING "Insert A String"
	    ))

(send insertbut_w :set_callback :xmn_activate_callback '(CALLBACK_XEVENT)
      '(
	(send te_w :CALL_ACTION_PROC "insert-string"  CALLBACK_XEVENT "funky " "hype " "bumrush " "jimmy's " "def " "fresh " "frop\n" )
	)
      )


(send toplevel_w :realize)
)

(send te_line_w :set_values :XMN_EDIT_MODE :MULTI_LINE_EDIT)
(send te_line_w :get_values :XMN_EDIT_MODE nil)
(send te_line_w :set_values :XMN_EDIT_MODE :SINGLE_LINE_EDIT)
(send te_w :set_values :XMN_EDIT_MODE :SINGLE_LINE_EDIT)
(send te_w :get_values :XMN_EDIT_MODE nil)
(send te_w :set_values :XMN_EDIT_MODE :MULTI_LINE_EDIT)

(if *MOTIF-1.1-OR-LATER-P*
    (send te_w :set_values		;Motif >= 1.1
	  :xmn_selection_array #(:SELECT_POSITION
				 :SELECT_WHITESPACE
				 :SELECT_WORD
				 :SELECT_LINE
				 :SELECT_ALL
				 :SELECT_PARAGRAPH)
	  :xmn_selection_array_count 6)
  (send te_w :set_values		;Motif 1.0
	:xmn_selection_array #(:SELECT_POSITION
			       :SELECT_WHITESPACE
			       :SELECT_WORD
			       :SELECT_LINE
			       :SELECT_ALL)
	:xmn_selection_array_count 5)
  )


;;; (send te_line_w :set_callback :xmn_activate_callback
;;;       '(CALLBACK_WIDGET
;;; 	CALLBACK_REASON
;;; 	CALLBACK_XEVENT
;;; ;;;	CALLBACK_DOIT
;;; ;;;	CALLBACK_CUR_INSERT
;;; ;;;	CALLBACK_NEW_INSERT
;;; ;;;	CALLBACK_START_POS
;;; ;;;	CALLBACK_END_POS
;;; ;;;	CALLBACK_TEXT
;;; 	)
;;;       '(
;;; 	(format T "widget=~A\nreason=~A\nxevent=~A\n" 
;;; 		CALLBACK_WIDGET	CALLBACK_REASON	CALLBACK_XEVENT)
;;; 	(format T "contents of buffer='~A'\n" 
;;; 		(send CALLBACK_WIDGET :GET_STRING))
;;; 	))

(send te_w :GET_STRING)
(send te_w :GET_LAST_POSITION)
(send te_w :SET_STRING
      "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
(send te_w :REPLACE 3 7 "XXX")
(send te_w :GET_EDITABLE)
(send te_w :SET_EDITABLE nil)
(send te_w :SET_EDITABLE t)
(send te_w :GET_MAX_LENGTH)
(send te_w :SET_MAX_LENGTH 9999999)
(send te_w :GET_SELECTION)
(send te_w :SET_SELECTION 3 7)
(send te_w :CLEAR_SELECTION)
(send te_w :GET_TOP_CHARACTER)
(send te_w :SET_TOP_CHARACTER 0)
(send te_w :GET_INSERTION_POSITION)
(send te_w :SET_INSERTION_POSITION 202)
(send te_w :SET_INSERTION_POSITION (send te_w :XY_TO_POS 40 100))
(send te_w :GET_SELECTION_POSITION)
(send te_w :XY_TO_POS 40 100)		;weird? x,y values are in pixels!
(send te_w :POS_TO_XY (send te_w :GET_INSERTION_POSITION))
(send te_w :SHOW_POSITION 111)
(send te_w :SCROLL -5)
(send te_w :DISABLE_REDISPLAY)
(send te_w :ENABLE_REDISPLAY)
