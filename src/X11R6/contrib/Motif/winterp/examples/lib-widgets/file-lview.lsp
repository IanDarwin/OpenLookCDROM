; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         file-lview.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/file-lview.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Labelled_File_Viewer_Widget_Class a subclass of
;		XM_FORM_WIDGET_CLASS.  (see comments below for details).
;		Note key bindings on viewer overriding XmText(3x)'s:
;		* <Key>space:		next-page()
;		* <Key>osfBackSpace:	previous-page()
; Author:       Niels P. Mayer
; Created:      Sun Dec 29 19:32:28 1991
; Modified:     Mon Jun  6 01:07:20 1994 (Niels Mayer) npm@indeed
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

(require "lib-widgets/file-view")	;define File_Viewer_Widget_Class

;;
;; Labelled_File_Viewer_Widget_Class a composite widget consisting of
;; a form widget, containing a label and a text viewer. It is a subclass
;; of Form by virtue of it's internal widgetry being contained within 
;; a form....
;;
(defvar Labelled_File_Viewer_Widget_Class
  (send Class :new
	'(label_w edit_w)		;new instance vars
	'()				;no class vars
	XM_FORM_WIDGET_CLASS		;superclass
	))

;;
;; Labelled_File_Viewer_Widget_Class Instance Initializer
;;
(send Labelled_File_Viewer_Widget_Class :answer :ISNEW
      '(managed_k widget_name widget_parent &rest args)
      '(
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k widget_name widget_parent
	       args			;merge-in creation resources (e.g. form constraints)
	       )
	(setq label_w
	      (send XM_LABEL_WIDGET_CLASS :new :managed
		    (concatenate 'string widget_name "_label") self
		    :XMN_LABEL_STRING		"<none>"
		    :XMN_ALIGNMENT		:alignment_beginning
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    ))
	(setq edit_w
	      (send File_Viewer_Widget_Class :new :managed
		    (concatenate 'string widget_name "_edit") self
		    :XMN_TOP_ATTACHMENT		:attach_widget
		    :XMN_TOP_WIDGET		label_w
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    ))

	self
	))

;;
;; Add a :FIND_FILE method as wrapper to File_Viewer_Widget_Class :FIND_FILE
;;
(send Labelled_File_Viewer_Widget_Class :answer :FIND_FILE
      '(filename &optional (linenum 0))
      '(
	(if (string/= filename (send edit_w :get_cur_filename))
	   (send label_w :set_values :XMN_LABEL_STRING filename))
	(send edit_w :find_file filename linenum)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/file-lview")
