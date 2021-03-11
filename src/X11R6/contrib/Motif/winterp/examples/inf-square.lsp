; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         inf-square.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/inf-square.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  Run "bc" calculator as subprocess, computing infinite number
;		of powers of 2 (or until user gets bored and quits). Displays
;		them in a row-column widget.
; Author:       Niels P. Mayer
; Created:      Sun Apr 17 20:50:52 1994 (Niels Mayer) npm@indeed
; Modified:     Sun Jun  5 18:54:44 1994 (Niels Mayer) npm@indeed
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

(require "lib-widgets/application")	;define WINTERP:APPLICATION-WIDGET-CLASS
(require "lib-utils/unixstuf")		;define unix:get-uname and other unixisms

(let*
    ((toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	    :XMN_WIDTH		500
	    :XMN_HEIGHT		500
	    :XMN_TITLE		"WINTERP: infinite squares"
	    :XMN_ICON_NAME	"W:inf-square"
	    ))
     (main_w
      (send WINTERP:APPLICATION-WIDGET-CLASS :new :managed
	    "main" toplevel_w
	    ))
     (scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
	    "sc" main_w
	    :XMN_SCROLLING_POLICY	:automatic
	    ))
     (rc_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "rc" scrl_w
	    :XMN_ORIENTATION		:vertical
	    :XMN_PACKING		:pack_tight
	    :XMN_ENTRY_ALIGNMENT	:alignment_beginning
	    ))
     bc_proc_io
     bc_proc_pid
     input-cb1
     )

  (send toplevel_w :add_callback :XMN_DESTROY_CALLBACK '()
	'(
	  (if (and input-cb1 (input_active_p input-cb1))
	      (xt_remove_input input-cb1))
	  (if bc_proc_io
	      (progn
		(exp_kill "INT" bc_proc_pid) ;OSF1 seems to need this, otherwise 'exp_wait' hangs
		(close bc_proc_io)	;however, for most systems 'close' will stop the subprocess
		(exp_wait)
		))
	  ))

  (send main_w :add-menu-entry "System"
	:mnemonic             #\S)
  (send main_w :add-menu-entry '("System" "Quit")
	:mnemonic                        #\Q
	:callback	#'(lambda (w x) (send toplevel_w :destroy)))

  (send main_w :make-menus)
  (send main_w :set-work-area scrl_w)

  (cond
   ((equal (unix:get-uname) "HP-UX")
    (exp_stty_init "9600 echo")		;HPUX seems to require echoing characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
    )
   (T					;default -- most systems don't require "echo"
    (exp_stty_init "9600 -echo")	;don't echo characters from exp_spawn'd subprocess, need to force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
    )
   )

  (setq bc_proc_io (exp_spawn "bc" "bc"))
  (setq bc_proc_pid (exp_get_pid))

  (setq input-cb1
	(xt_add_input			;XtAppAddInput()
	 bc_proc_io :READ_LINE_TO_STRING
	 '(;; READ_LINE_TO_STRING fires callback once per line, binding line to FDINPUTCB_STRING
	   (send scrl_w :scroll_visible
		 (send XM_LABEL_WIDGET_CLASS :new :managed rc_w
		       :XMN_LABEL_STRING	FDINPUTCB_STRING
		       :XMN_ALIGNMENT		:ALIGNMENT_BEGINNING
		       )
		 5 5)
	   ))
	)

  (format bc_proc_io "2\n")

  (dotimes (i 50)
	   (format bc_proc_io ". ^ 2\n"))


  (send toplevel_w :realize)
  )
