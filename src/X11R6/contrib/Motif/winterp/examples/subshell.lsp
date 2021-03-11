;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         subshell.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/subshell.lsp,v 2.3 1994/06/06 14:43:04 npm Exp $
; Description:  Demo of spawning an interactive shell subprocess and interacting
;		with the subrpocess through XT_ADD_INPUT. Subprocess can be
;		off doing a long computation while WINTERP GUI remains active.
;		To use this example, WINTERP must be compiled with
;		"WANT_EXPECT_SUBPROCESS" enabled...
; Author:       Niels Mayer
; Created:      Sat Oct  5 18:56:33 1991
; Modified:     Sun Jun  5 19:19:05 1994 (Niels Mayer) npm@indeed
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

(let (					;declare local variables
      subproc-pty subproc-pid input-cb command-editor-w quit-button-w list-w top-w rc-w
      )

  ;;; Widgets

  (setq top-w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "subshell"
	      ))
  (setq rc-w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" top-w
	      :XMN_ORIENTATION		:vertical
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      ))
  (setq quit-button-w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "quit-button" rc-w
	      :XMN_LABEL_STRING		"Quit"
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

  (send top-w :add_callback :XMN_DESTROY_CALLBACK '() ;invoke when widgetry gets destroyed by user 
	'(
	  (xt_remove_input input-cb)	;must remove this before closing
	  (exp_kill "INT" subproc-pid)	;OSF1 seems to need this, otherwise 'exp_wait' hangs
	  (close subproc-pty)		;however, for most systems 'close' will stop the subprocess
	  (exp_wait)			;wait(2) on the subprocess
	  ))

  (send quit-button-w :add_callback :XMN_ACTIVATE_CALLBACK '() ;invoke when "quit" pushed...
	'(
	  (send top-w :destroy)		;NOTE -- this also invokes top-w :XMN_DESTROY_CALLBACK...
	  ))

  (send command-editor-w :add_callback :XMN_ACTIVATE_CALLBACK ;invoke when <return> ... hit.
	'(callback_widget)		;bound to the current value of command-editor-w
	'(
	  (format subproc-pty "~A\n"	;"print" text-string in editor to the subprocess
		  (send callback_widget :get_string))
	  ))

  ;;; Subprocess

  (exp_stty_init "9600 echo")		;echo characters from exp_spawn'd subprocess, need to force this because running WINTERP as emacs subprocess doesn't echo, force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
  (setq subproc-pty (exp_spawn "sh" "sh")) ;create subprocess, the bourne shell
  (setq subproc-pid (exp_get_pid))
  (setq input-cb			
	(xt_add_input			;XtAppAddInput()
	 subproc-pty :READ_LINE_TO_STRING
	 '(;; READ_LINE_TO_STRING fires callback once per line, binding line to FDINPUTCB_STRING
	   (send list-w :add_item FDINPUTCB_STRING 0) ;XmListAddItem()
	   (send list-w :set_bottom_pos 0) ;XmListSetBottomPos()
	   )
	 ))
  )
