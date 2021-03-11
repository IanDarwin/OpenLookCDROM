;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         subcalcslo.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/subcalcslo.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  Demo of spawning an interactive subprocess and interacting
;		with the subrpocess through XT_ADD_INPUT/:READ. Subprocess
;		can be off doing a long calculation while WINTERP GUI remains
;		active.
;		A faster version of this same example is in "subcalc.lsp"
;		efficiency is improved by using
;		XT_ADD_INPUT/:READ_LINE_TO_STRING.... To use this example,
;		WINTERP must be	compiled with "WANT_EXPECT_SUBPROCESS"
;		enabled...
; Author:       Niels Mayer
; Created:      Sat Oct  5 18:51:56 1991
; Modified:     Sun Jun  5 19:17:52 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define unix:get-uname, and other unixisms.

(let (					;declare local variables
      subproc_pty subproc_pid input_cb command_editor_w quit_button_w list_w top_w rc_w
      )

  ;;; Widgets

  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "Calc" "calc"
	      ))
  (setq rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed "rc" top_w
	      :XMN_ORIENTATION		:vertical
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      ))
  (setq quit_button_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_STRING		"Quit"
	      ))
  (setq command_editor_w 
	(send XM_TEXT_FIELD_WIDGET_CLASS :new :managed rc_w
	      ))
  (setq list_w
	(send XM_LIST_WIDGET_CLASS :new :managed :scrolled rc_w
	      :XMN_VISIBLE_ITEM_COUNT	20
	      ))

  (send top_w :realize)

  ;;; Callbacks

  (send top_w :add_callback :XMN_DESTROY_CALLBACK '() ;invoke when widgetry gets destroyed by user 
	'(
	  (xt_remove_input input_cb)	;must remove this before closing
	  (exp_kill "INT" subproc_pid)	;OSF1 seems to need this, otherwise 'exp_wait' hangs
	  (close subproc_pty)		;however, for most systems 'close' will stop the subprocess
	  (exp_wait)			;wait(2) on the subprocess
	  ))

  (send quit_button_w :add_callback :XMN_ACTIVATE_CALLBACK '() ;invoke when "quit" pushed...
	'(
	  (send top_w :destroy)		;NOTE -- this also invokes top_w :XMN_DESTROY_CALLBACK...
	  ))

  (send command_editor_w :set_callback :XMN_ACTIVATE_CALLBACK ;invoke when <return> ... hit.
	'(callback_widget)
	'(
	  (format subproc_pty "~A\n"	;"print" text-string in editor to the subprocess
		  (send callback_widget :get_string))
	  )
	)

  ;;; Subprocess
  (cond
   ((equal (unix:get-uname) "HP-UX")
    (exp_stty_init "9600 echo")		;HPUX seems to require echoing characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
    )
   (T					;default -- most systems don't require "echo"
    (exp_stty_init "9600 -echo")	;don't echo characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
    )
   )
  (setq subproc_pty (exp_spawn "bc" "bc")) ;create subprocess, the bc(1) calculator
  (setq subproc_pid (exp_get_pid))
  (let ((line NIL))			;for every line output from bc(1), append result-
    (setq input_cb			;-to list widget...
	  (xt_add_input subproc_pty
			:read
			'(
			  (let ((c (read-char FDINPUTCB_FILE)))
			    (cond
			     ((char= c #\newline) 
			      (send list_w :add_item (concatenate 'string (reverse line)) 0)
			      (send list_w :set_bottom_pos 0)
			      (setq line nil)
			      )
			     (t
			      (setq line (cons c line))
			      ))
			    ))))
    )

  )
