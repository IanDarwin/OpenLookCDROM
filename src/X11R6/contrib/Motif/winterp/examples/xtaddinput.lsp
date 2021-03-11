;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xtaddinput.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/xtaddinput.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  Demo of XT_ADD_INPUT with :READ_LINE_TO_STRING 
;		versus :READ_SEXP_TO_USTREAM options. The program itself
;		is stupid -- I spawn cat(1) as a subprocess just to echo
;		the input to the subprocess back to the output, then use
;		XT_ADD_INPUT to read the output back line-by-line or
;		sexpr-by-sexpr. 
;		To use this example, WINTERP must be compiled with
;		"WANT_EXPECT_SUBPROCESS" enabled...
; Author:       Niels Mayer
; Created:      Sat Oct  5 18:51:56 1991
; Modified:     Sun Jun  5 19:26:45 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define unix:get-uname and other unixisms.

(let (					;declare local variables
      subproc-pty
      subproc-pid
      read-sexp-cb
      read-line-cb
      command-editor-w
      quit-button-w
      reader-options-w
      list-w
      top-w
      rc-w
      )

  ;;; Widgets

  (setq top-w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	      "subprocess-cat" "Subprocess-Cat"
	      :XMN_DELETE_RESPONSE :destroy
	      ))
  (setq rc-w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" top-w
	      :XMN_ORIENTATION		:vertical
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      ))
  (setq quit-button-w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "quit-button" rc-w
	      :XMN_LABEL_STRING		"Quit"
	      ))
  (setq reader-options-w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_option_menu
	    "reader-options" rc-w
	    :XMN_OPTION_LABEL		"Reader Options"
	    :XMN_OPTION_MNEMONIC	#\R
	    :XMN_BUTTON_COUNT		2
	    :XMN_BUTTONS		#("Read per S-Expression" "Read per Line")
	    :XMN_BUTTON_MNEMONICS	#(#\S #\L)
	    :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON)
	    :XMN_BUTTON_SET		0
	    ))
  (setq command-editor-w 
	(send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
	      "command-editor" rc-w
	      ))
  (setq list-w
	(send XM_LIST_WIDGET_CLASS :new :managed :scrolled
	      "list" rc-w
	      :XMN_VISIBLE_ITEM_COUNT	20
	      ))

  (send top-w :realize)

  ;;; Callbacks

  (send quit-button-w :set_callback	;XtAppAddCallback()
	:XMN_ACTIVATE_CALLBACK		;invoke when button pushed
	'()				;no local vars.
	`(				;callback code
	  (send ,top-w :destroy)	;destroy entire panel, also calls :XMN_DESTROY_CALLBACK below
	  ))

  (send quit-button-w :set_callback	;XtAppAddCallback()
	:XMN_DESTROY_CALLBACK		;invoke when button destroyed.
	'()				;no local vars.
	'(				;callback code
	  (if (and read-line-cb (input_active_p read-line-cb))
	      (xt_remove_input read-line-cb) ;must remove this before closing
	    )

	  (if (and read-sexp-cb (input_active_p read-sexp-cb))
	      (xt_remove_input read-sexp-cb) ;must remove this before closing
	    )

	  (if subproc-pty
	      (progn
		(exp_kill "INT" subproc-pid) ;OSF1 seems to need this, otherwise 'exp_wait' hangs
		(close subproc-pty)	;however, for most systems 'close' will stop the subprocess
		(setq subproc-pty nil)
		(exp_wait)		;wait on the subprocess
		))

;;	  (format T "deleting widget=~A, name=~A\n"
;;		  top-w (send top-w :name))

	  ))

  (send command-editor-w :set_callback	;XtAppAddCallback()
	:XMN_ACTIVATE_CALLBACK		;invoke when <return> ... hit.
	'(callback_widget)		;bound to the current value of command-editor-w
	'(				;code to execute
	  (format subproc-pty "~A\n"	;send text in editor to the subprocess
		  (send callback_widget :get_string))
	  )
	)

  (send (send reader-options-w :GET_SUB_MENU_WIDGET) ;must use :GET_SUB_MENU_WIDGET rather than :get_values/:xmn_sub_menu_id, since that one reveals 1.1 bug.
	:set_callback :XMN_ENTRY_CALLBACK
	'(CALLBACK_ENTRY_WIDGET)
	'(				;code to execute
	  ;; don't do anything unless the CALLBACK_ENTRY_WIDGET is different
	  ;; from the :XMN_MENU_HISTORY widget -- the XMN_ENTRY_CALLBACK on
	  ;; :simple_option_menu will fire on any click inside the option menu,
	  ;; but we only want the code to execute if an option changed...
	  (if (not (eq (car (send reader-options-w :get_values :XMN_MENU_HISTORY nil))
		       CALLBACK_ENTRY_WIDGET))
	      (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		    (0			;"Read per S-Expression"
		     ;; disable read-line input callback
		     (if (and read-line-cb (input_active_p read-line-cb))
			 (xt_remove_input read-line-cb)
		       )
		     ;; re-enable previously disabled read-sexp input callback
		     (if (not (input_active_p read-sexp-cb))
			 (xt_add_input read-sexp-cb))

		     )
		    (1			;"Read per Line"
		     ;; disable read-sexp input callback
		     (if (and read-sexp-cb (input_active_p read-sexp-cb))
			 (xt_remove_input read-sexp-cb)
		       )
		     (if (null read-line-cb) ;add read-line cb for the first time...
			 (setq read-line-cb	
			       (xt_add_input ;XtAppAddInput()
				subproc-pty :READ_LINE_TO_STRING
				;; READ_LINE_TO_STRING fires callback once per line, binding line to FDINPUTCB_STRING
				'(
				  (send list-w :add_item ;XmListAddItem()
					FDINPUTCB_STRING
					0)
				  (send list-w :set_bottom_pos 0) ;XmListSetBottomPos()
				  )
				))
		       ;; re-enable previously disabled read-line cb.
		       (if (not (input_active_p read-line-cb))
			   (xt_add_input read-line-cb))
		       )
		     )
		    ))
	  ))

  ;;; Subprocess
  (cond
   ((equal (unix:get-uname) "HP-UX")
    (exp_stty_init "9600 echo")		;HPUX seems to require echoing characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
    )
   (T					;default -- most systems don't require "echo"
    (exp_stty_init "9600 -echo")	;don't echo characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
    )
   )
  (setq subproc-pty (exp_spawn "cat" "cat")) ;create subprocess, cat(1).
  (setq subproc-pid (exp_get_pid))
  (setq read-sexp-cb	
	(xt_add_input			;XtAppAddInput()
	 subproc-pty :READ_SEXP_TO_USTREAM
	 '(;; :READ_SEXP_TO_USTREAM fires callback once per s-expression, binding FDINPUTCB_USTREAM
	   (send list-w :add_item	;XmListAddItem()
		 (format nil "~S" (read FDINPUTCB_USTREAM))
		 0)
	   (send list-w :set_bottom_pos 0) ;XmListSetBottomPos()
	   ))
	)

  )
