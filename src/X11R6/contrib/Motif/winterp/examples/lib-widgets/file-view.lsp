; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         file-view.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/file-view.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  File_Viewer_Widget_Class a subclass of XM_TEXT_WIDGET_CLASS
;		with :SCROLLED option. (see comments below for details).
;		Note key bindings on viewer overriding XmText(3x)'s:
;		* <Key>space:		next-page()
;		* <Key>osfBackSpace:	previous-page()
; Author:       Niels P. Mayer
; Created:      Sun Dec 29 19:32:28 1991
; Modified:     Mon Jun  6 01:07:32 1994 (Niels Mayer) npm@indeed
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

;;
;; Make a subclass of XM_TEXT_WIDGET_CLASS which holds an additional
;; instance variable 'file-path'. 'file-path' is a string representing
;; the full name of the file in the text editor widget.
;;
;; Method :FIND_FILE uses this filename to decide whether it must
;; read the file into the text widget, or whether it's already 
;; there. We don't want it to reread uncecessarily since for large
;; files, this can be slow...
;;
(defvar File_Viewer_Widget_Class
  (send Class :new
	'(file-path)			;new instance vars
	'()				;no class vars
	XM_TEXT_WIDGET_CLASS))		;superclass

;;
;; Override superclass's instance initializer so we can set
;; instance variable, and supply some default arguments.
;;
(send File_Viewer_Widget_Class :answer :ISNEW
      '(managed_k widget_name widget_parent &rest args)
      '(
	(setq file-path "")		;initialize instance var

	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k :scrolled widget_name widget_parent
	       :XMN_EDIT_MODE			:multi_line_edit
	       :XMN_EDITABLE			nil ;don't allow user to change text.
	       :XMN_AUTO_SHOW_CURSOR_POSITION	nil ;don't need to show where the cursor is
	       :XMN_CURSOR_POSITION_VISIBLE	t ;do show the cursor, incase user "cursoring around"
	       args
	       )

	;; this is a file-viewer, so make space and backspace do paging.
	;; (overriding the translations below shouldn't cause any bad
	;; interactions w/ existing translation tables since the text in
	;; the file-viewer is not is not editable in the first place).
	(send-super :OVERRIDE_TRANSLATIONS
		    "<Key>space:        next-page() \
                     <Key>osfBackSpace: previous-page()"
		    )
	))

;;
;; accessor method...
;;
(send File_Viewer_Widget_Class :answer :GET_CUR_FILENAME
      '()
      '(
	file-path
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add a :FIND_FILE method to the Motif Text widget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send File_Viewer_Widget_Class :answer :FIND_FILE
      '(filename &optional (linenum 0))
      '(
	(cond
	 ((string= filename file-path)	;if the file was already read into widget
	  (send-super :goto_line linenum t) ;just go to the line number...
	  )
	 (t				;else read the file into the widget...
	  (send-super :read_file_goto_line filename linenum t)
	  (setq file-path filename)
	  )
	 )
	)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/file-view")
