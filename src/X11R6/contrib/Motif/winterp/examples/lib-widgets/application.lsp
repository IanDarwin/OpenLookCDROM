; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         application.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/application.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  WINTERP:APPLICATION-WIDGET-CLASS a subclass of
;		XM_MAIN_WINDOW_WIDGET_CLASS which contains a message area
;		instance of TIMED-MESSAGE-DISPLAY-WIDGET-CLASS and has
;		a pulldown menu system created from a WINTERP:MENU-BAR-WIDGET-CLASS
;		instance.
; Author:       Niels P. Mayer
; Created:      Sun Apr 17 20:50:52 1994 (Niels Mayer) npm@indeed
; Modified:     Mon Jun  6 01:06:13 1994 (Niels Mayer) npm@indeed
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

(require "lib-widgets/timed-msg")	;define TIMED-MESSAGE-DISPLAY-WIDGET-CLASS
(require "lib-widgets/simple-RC")	;define WINTERP:MENU-BAR-WIDGET-CLASS

;;;
;;; WINTERP:APPLICATION-WIDGET-CLASS -- a subclass of XM_MAIN_WINDOW_WIDGET_CLASS
;;;
(setq WINTERP:APPLICATION-WIDGET-CLASS		;name of the new subclass
      (send Class :new
            '(				;inst variables for subclass
	      ivar_menubar_w		;XM_ROW_COLUMN_WIDGET_CLASS/:simple_menu_bar entry
	      ivar_msg_w		;TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
	      )
            '()                         ;no class variables for subclass
            XM_MAIN_WINDOW_WIDGET_CLASS ;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent &rest args)
      '(
	;; create 'self', an instance of XM_MAIN_WINDOW_WIDGET_CLASS
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k widget_name widget_parent
	       args
	       )

	;; widget superclass not instatiated until :MAKE-MENUS called...
	(setq ivar_menubar_w		
	      (send WINTERP:MENU-BAR-WIDGET-CLASS :new :managed
		    "menubar" self
		    ))

	(setq ivar_msg_w
	      (send TIMED-MESSAGE-DISPLAY-WIDGET-CLASS :new :managed
		    "msg" self
		    ))

	(send-super :set_values
		    :XMN_MESSAGE_WINDOW	ivar_msg_w
		    )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Call this after calling all needed :ADD-MENU-ENTRY methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :MAKE-MENUS
      '()
      '(
	;; go through toplevel menu and call :recursive-make-menus
	(send ivar_menubar_w :recursive-make-menus)
	(send-super :set_values
		    :XMN_MENU_BAR ivar_menubar_w
		    )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makes the last entry in the menu-bar the widget for :XMN_MENU_HELP_WIDGET.
;; This forces the "Help" menu bar entry to the far-right of the menu-bar.
;; This is optional because some applications may not want a help widget in
;; the menu bar.
;; One should only call this after calling :MAKE-MENUS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :SET-MENU-HELP-WIDGET
      '()
      '(
	(send ivar_menubar_w :set-menu-help-widget)
	))

;;;
;;; (send xxx :ADD-MENU-ENTRY "Files"
;;;                         :mnemonic #\F
;;;                         :type :CASCADEBUTTON)
;;;
;;; (send xxx :ADD-MENU-ENTRY '("Files" "Quit")
;;;                         :mnemonic		#\Q
;;;			    :mnemonic-charset	"ISO8859-1"
;;;                         :type		:PUSHBUTTON
;;;			    :accelerator	"Ctrl<Key>C")
;;;			    :accelerator-text	"^C"
;;;			    :callback		#'(lambda () ...)
;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :ADD-MENU-ENTRY
      '(pulldown-name			;STRING or LIST...
	&key
	((:mnemonic		k_mnemonic_char)	NIL)
	((:mnemonic-charset	k_mnemonic_charset_str)	NIL)
	((:type			k_type_kwd)		NIL)
	((:accelerator		k_accelerator_str)	NIL)
	((:accelerator-text	k_accelerator_text_str)	NIL)
	((:callback		k_callback_funct)	NIL)
	)
      '(
	(send ivar_menubar_w :add-menu-entry pulldown-name
	      :mnemonic		k_mnemonic_char
	      :mnemonic-charset	k_mnemonic_charset_str
	      :type		k_type_kwd
	      :accelerator	k_accelerator_str
	      :accelerator-text	k_accelerator_text_str
	      :callback		k_callback_funct
	      )
	))

;;;
;;;
;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :SET-WORK-AREA
      '(workarea_w)
      '(
	(send-super :set_values
		    :XMN_WORK_WINDOW	workarea_w
		    )
	))

;;;
;;; Method :DISPLAY-STRING -- temporarily display a string in message area
;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :DISPLAY-STRING
      '(string &optional time)
      '(
	(send ivar_msg_w :display-string string time)
	))


;;;
;;; Method :ERROR-DISPLAY-STRING -- temporarily display an error msg in message area
;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :ERROR-DISPLAY-STRING
      '(string &optional time)
      '(
	(send ivar_msg_w :error-display-string string time)
	))

;;;
;;; Method :GET-MSG-WIDGET -- return instance of TIMED-MESSAGE-DISPLAY-WIDGET-CLASS
;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :GET-MSG-WIDGET
      '()
      '(
	ivar_msg_w
	))

;;;
;;; Method :GET-MENUBAR-WIDGET -- return instance of WINTERP:SIMPLE-MENUBAR-WIDGET
;;;
(send WINTERP:APPLICATION-WIDGET-CLASS :answer :GET-MENUBAR-WIDGET
      '()
      '(
	ivar_menubar_w
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/application")
