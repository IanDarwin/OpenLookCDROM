; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         help.lsp
; RCS:          $Header: $
; Description:  Context Sensitive Help System
; Author:       Niels Mayer, HPLabs
; Created:      Mon Aug 24 22:10:36 1992
; Modified:     Thu Sep  2 03:23:25 1993 (Niels Mayer) mayer@hplnpm
; Language:     Lisp
; Package:      N/A
; Status:       X11r5 contrib tape release
;
; WINTERP Copyright 1989-1992 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and Niels Mayer not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and Niels Mayer
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; this global constant needs to be changed if the location of the 
;;; location of the help file changes. 
;;;
(defvar *help-file-directory* "")

;;; *SYSTEM-EDITOR*:
;;; if NIL, then edit functionality will use editor set in environment variable 
;;; $EDITOR. If set to a string, then that string will be used as the name of
;;; the editor to use for the "Edit" button.
(defvar *SYSTEM-EDITOR* nil)

;;;
;;; hack global variable used only in this file...
;;;
(defvar *help-edit-mode-p* NIL)		;if T, then call up $EDITOR on the help file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help_Dialog_Widget_Class:
;; define a subclass of XM_MESSAGE_BOX_WIDGET_CLASS/:INFORMATION_DIALOG
;; which will look up help for a particular widget in *help-file-directory*
;; the name of the help file is named by the widget's name (as returned by
;; (send widget :name)==XtName().
;; 
;; Use 'add-help-to-widget' to register a widget for context-sensitive
;; help using the aforementioned subclass.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; This function adds a help callback to a particular widget...
;;
(defun add-help-to-widget (widget)
  (send widget :add_callback :XMN_HELP_CALLBACK '()
	`(
	  (send Help_Dialog_Widget_Class :new :managed ,widget)
	  )) 
  )

;;
;; (context-sensitive-help <confine-to-w>)
;;
;; Call this function to allow user to pick the widget on which to 
;; get help information.
;;
;; <confine-to-w> is the widget in which the pointer is confined to
;; during the "pick" opeation.
;; <xevent> is the xevent passed in from the callback that invoked
;; this function -- it is needed by :call_action_proc...
;;
(defun context-sensitive-help (confine-to-widget xevent)
  (let ((picked-w 
	 (xm_tracking_locate
	  confine-to-widget		;confine modalness to this app-window
	  92				;X11/cursorfont.h:#define XC_question_arrow 92
	  t)))				;force confine of modalness to app-window
;;;     (cond
;;;      ((send picked-w :is_manager)
;;;       (send picked-w :call_action_proc "ManagerGadgetHelp" xevent)
;;;       )
;;;      ((send picked-w :is_primitive)
;;;       (send picked-w :call_action_proc "PrimitiveHelp" xevent)
;;;       )
;;;      ((send picked-w :is_gadget)	;XtCallActionProc() not defined to work on gadgets...
;;;       (send (send picked-w :parent) :call_action_proc "ManagerGadgetHelp" xevent)
;;;       )
;;;      (t
;;;       (send picked-w :call_action_proc "Help" xevent)
;;;       )
;;;      )

    ;; for some reason, Motif 1.2 doesn't allow 'help' to be called via
    ;; :CALL_ACTION_PROC (see comment-out above). So instead of calling the
    ;; :XMN_HELP_CALLBACK via :CALL_ACTION_PROC, we just create the help dialog
    ;; directly. The help dialog info is based on the widget name... 
    ;; Note that there may be widgets for which help callback hasn't been defined
    ;; and therefore help dialog won't find help text. For such widgets,
    ;; we go up the parent widget chain until we find one which does.

    (do ((w picked-w (send w :parent)))
	((and w (eq (send w :has_callbacks :XMN_HELP_CALLBACK)
		    'CALLBACK_HAS_SOME))
	 (if w (send Help_Dialog_Widget_Class :new :managed w))
	 ))
    ))

(defun edit-context-sensitive-help (confine-to-widget xevent)
  (setq *help-edit-mode-p* T)
  (progv '(*breakenable*) '(nil)
	 (unwind-protect		;trap any errors...
	     (context-sensitive-help confine-to-widget xevent) 
	   (setq *help-edit-mode-p* NIL) ;unwind always
	   ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Help_Dialog_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  )
	'()				;no class vars
	XM_MESSAGE_BOX_WIDGET_CLASS	;superclass
	))

(send Help_Dialog_Widget_Class :answer :ISNEW
      '(managed-kwd parent-w &rest args) ;note: widget name generated automatically from parent-w
      '(
	(cond
	 (*help-edit-mode-p*		;if this global got set by 'edit-context-sensitive-help', then edit...
	  (system (format nil 
			  "~A ~A &"
			  (if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
			  (strcat *help-file-directory* (send parent-w :name))
			  ))
	  )
	 (T				;else display the help file
	  ;; create the XM_MESSAGE_BOX_WIDGET_CLASS inst by sending :isnew to superclass
	  (apply 'send-super		;widget-inst is now bound to <self>
		 :ISNEW managed-kwd	;splice in method arguments passed in above
		 :INFORMATION_DIALOG
		 (send parent-w :name) ;name of help dialog is name of widget help called 
		 (if (send parent-w :is_gadget) ;handle case where help widget is a gadget...
		     (send parent-w :parent)
		   parent-w)
		 :XMN_AUTO_UNMANAGE nil
		 :XMN_MESSAGE_STRING
		 (read-help-file-into-string (send parent-w :name))
		 args		
		 )
	  (send (send self :get_child :DIALOG_HELP_BUTTON) :unmanage) ;no help on help...
	  (send self :add_callback :XMN_CANCEL_CALLBACK '()
		'(
		  (send self :destroy)
		  ))
	  (send self :add_callback :XMN_OK_CALLBACK '()
		'(
		  (send self :destroy)
		  ))
	  ))
	))

;; file reader function used above by Help_Dialog_Widget_Class...
(defun read-help-file-into-string (help-file)
  (let
      ((fp (open (strcat *help-file-directory* help-file)
		 :direction :input))
       (res-list '())
       linestr
       )
    (if (null fp)
	(format nil "Help file '~A' not found" ;return this msg to help-dialog-box on failure
		(strcat *help-file-directory* help-file))
      (progn
	(loop
	 (if (null (setq linestr (read-line fp)))
	     (return))
	 (setq res-list (cons (strcat linestr "\n") res-list))
	 )
	(close fp)
	(apply #'strcat (reverse res-list)) ;return this on success
	))
    ))


;;
;; incase they didn't pre-load initialize.lsp or proper ~/.winterp
;;
(unless (fboundp 'strcat)		; backwards compatibility if COMMONLISP defined
	(defun strcat (&rest str) 
	  (apply #'concatenate 'string str)))
