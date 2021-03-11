;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         hostlookup.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/hostlookup.lsp,v 2.5 1994/06/06 14:43:11 npm Exp $
; Description:  A lamo application that uses method :FORCED_EXPOSE_UPDATE to 
;		popup and display contents of a "working dialog"
;		before a time-consuming subprocess begins to execute.
; Author:       Niels Mayer
; Created:      Fri Feb  8 19:59:47 1991
; Modified:     Sun Jun  5 18:52:46 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/show-busy")		;define WINTERP-SHOW-BUSY-PROGN

(let (top-w edit-w dialog-w)

  (setq top-w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "top-w"
	      :XMN_TITLE	"WINTERP: Hostname lookup"
	      :XMN_ICON_NAME	"W:hostlookup"
	      ))
  (setq edit-w
	(send XM_TEXT_WIDGET_CLASS :new :managed
	      "edit-w" top-w
	      :XMN_EDIT_MODE       :SINGLE_LINE_EDIT
	      :XMN_COLUMNS         80
	      ))

  (send edit-w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (winterp-show-busy-progn
	   (let ((hostname (send edit-w :get_string)))
	     (send dialog-w :set_values 
		   :XMN_MESSAGE_STRING (concatenate 'string "Looking up hostname " hostname)
		   )
	     (send dialog-w :manage)
	     (send dialog-w :FORCED_EXPOSE_UPDATE) ;force expose and update

	     (let* (
		    (pipe (popen (concatenate 'string "grep -i '" hostname "' /etc/hosts")
				 :direction :input))
		    (res-str (read-line pipe))
		    )
	       (if res-str
		   (send edit-w :set_string res-str)
		 (send edit-w :set_string
		       (format nil " << error: '~A' NOT FOUND >>" hostname))
		 )
	       (pclose pipe)
	       )
	     )
	   (send dialog-w :unmanage)
	   )
	  ))
   
  (send top-w :realize)

  (setq dialog-w
	(send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :working_dialog
	      "working_dialog" top-w
	      ))
  (send (send dialog-w :get_child :DIALOG_OK_BUTTON) :unmanage)
  (send (send dialog-w :get_child :DIALOG_CANCEL_BUTTON) :unmanage)
  (send (send dialog-w :get_child :DIALOG_HELP_BUTTON) :unmanage)
  (send (send dialog-w :get_child :DIALOG_SEPARATOR) :unmanage)



  )
