; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         fileselect.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/fileselect.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  WINTERP:FILE-SELECTION-WIDGET a subclass of
;		XM_FILE_SELECTION_BOX_WIDGET_CLASS. (see comments below for
;		details).
; Author:       Niels P. Mayer
; Created:      Sun Dec 29 19:32:28 1991
; Modified:     Mon Jun  6 01:07:42 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WINTERP:FILE-SELECTION-WIDGET -- a subclass of
;;; XM_FILE_SELECTION_BOX_WIDGET_CLASSS, optimized for displaying within
;;; another manager widget (as opposed to a stand-alone dialog box).
;;;
;;; Unfortunately, the current Motif implementations (1.0, 1.1, 1.2) have the
;;; feature that they don't call the :XMN_OK_CALLBACK or the
;;; :XMN_APPLY_CALLBACK when a selection is made by (1) double clicking on
;;; a file in the "files" list; (2) double clicking on a directory in the
;;; "directories" list; (3) typing text and hitting <return> in either the
;;; "selection" or "filter" text-fields. This subclass offers workarounds
;;; to those problems via the "callbacks" set up by methods
;;; :SET-FILE-SELECTED-CALLBACK-CLOSURE, :SET-DIR-SELECTED-CALLBACK-CLOSURE.
;;;
;;; Example usage of those methods:
;;;   (send fs_w :set-file-selected-callback-closure
;;; 	(lambda (selected_file_str)
;;; 	  (send msg_w :set_string (format nil "viewing file: ~A" selected_file_str))
;;; 	  (send edit_w :read_file selected_file_str)
;;; 	  ))
;;; 
;;;   (send fs_w :set-dir-selected-callback-closure
;;; 	(lambda (selected_dir_str)
;;; 	  (send msg_w :set_string (format nil "viewing directory: ~A" selected_dir_str))
;;; 	  ))

(require "lib-utils/unixstuf")		;define FILE:GET-PATH

;;;
;;; Create WINTERP:FILE-SELECTION-WIDGET
;;;
(setq WINTERP:FILE-SELECTION-WIDGET	;name of the new subclass
      (send Class :new
            '(				;new instance vars for subclass.
	      ivar_file_selected_callproc
	      ivar_dir_selected_callproc
	      )
            '()                         ;no class variables for subclass
            XM_FILE_SELECTION_BOX_WIDGET_CLASS ;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:FILE-SELECTION-WIDGET :answer :ISNEW
      '(managed_k widget_name widget_parent
		  &rest args)
      '(
	(setq ivar_file_selected_callproc NIL
	      ivar_dir_selected_callproc  NIL)
	 
	;; create 'self', an instance of XM_FILE_SELECTION_BOX_WIDGET_CLASS
	(apply #'send-super :isnew	;call superclass's init to create widget
	       :unmanaged		;don't manage till through twiddling geometry by unmanaging children
	       widget_name widget_parent
	       args)			;splice in method arguments passed in above

	;;
	;; we don't want these fileselbox widgets around because they take up
	;; too much space and don't provide useful functionality.
	;; Unfortunately, if :DIALOG_APPLY_BUTTON and :DIALOG_OK_BUTTON are
	;; not managed, the fsb_w's "Ok" and "Filter" actions will not be available
	;; through the "Directories" and "Files" list widgets, nor through the 
	;; "Filter" and "Selection" text widgets. This is lameness on the part of
	;; Motif
	;;

	(send (send-super :get_child :DIALOG_HELP_BUTTON)	:unmanage)
	(send (send-super :get_child :DIALOG_CANCEL_BUTTON)	:unmanage)
	(send (send-super :get_child :DIALOG_OK_BUTTON)		:unmanage)
	(send (send-super :get_child :DIALOG_APPLY_BUTTON)	:unmanage)
	(send (send-super :get_child :DIALOG_SEPARATOR)		:unmanage)
	(if (eq :managed managed_k)
	    (send-super :manage)	;manage the parent only when finished removing children
	  )

	;; Since the "Ok" button is unmanaged, the double clicking an entry
	;; in the files-list and "selection" text-field will not call the
	;; :XMN_OK_CALLBACK (which is the default behavior for XmFileSelectionBox)
	;; The following callbacks work around the limitation by placing
	;; callbacks on the widgets themselves. When fired, those callbacks
	;; call the closure set up by :SET-FILE-SELECTED-CALLBACK-CLOSURE.

	;; Callback that fires on double-click of item in "files" list.
	(send (send-super :get_child :DIALOG_LIST) :add_callback
	      :XMN_DEFAULT_ACTION_CALLBACK '()
	      '(
		(if ivar_file_selected_callproc
		    (funcall ivar_file_selected_callproc
			     (xm_string_get_l_to_r
			      (car
			       (send-super :get_values :XMN_DIR_SPEC nil)))
			     )
		  )
		))

	;; Callback that fires when <return> entered into the "selection" text-field.
	(send (send-super :get_child :DIALOG_TEXT) :add_callback
	      :XMN_ACTIVATE_CALLBACK '()
	      '(
		(if ivar_file_selected_callproc
		    (funcall ivar_file_selected_callproc
			     (xm_string_get_l_to_r
			      (car
			       (send-super :get_values :XMN_DIR_SPEC nil)))
			     )
		  )
		))

	;; Since the "Apply" (aka "Filter") button is unmanaged, double clicking
	;; an entry in the directory-list or the "filter" text-field will not
	;; call the :XMN_APPLY_CALLBACK, nor will it update the directories/files
	;; being displayed in the list widgets (which is default behavior for 
	;; XmFileSelectionBox). The following callbacks work around the limitation
	;; by placing callbacks on the widgets themselves. When fired, those callbacks
	;; call :DO_SEARCH to update the directories/files in the browser,
	;; and then call the closure set up by :SET-DIR-SELECTED-CALLBACK-CLOSURE.

	;; Callback that fires on double-click of item in "directories" list.
	;; (Note: Don't set up :DIALOG_DIR_LIST callback unless using
	;; Motif 1.1 or greater. Motif 1.0 doesn't have :DIALOG_DIR_LIST
	;; in the file selection box...)

	;; a double click in the "directories" list widget inside fsb_w
	;; will cause the directory to be browsed...
	(send (send-super :get_child :DIALOG_DIR_LIST) :add_callback
	      :XMN_DEFAULT_ACTION_CALLBACK '(CALLBACK_ITEM)
	      '(
;;; :DO_SEARCH doesn't work on SGI IRIX's Motif (Grrr!!)
;;;		(send-super :do_search)
		(let ((dir (xm_string_get_l_to_r CALLBACK_ITEM)))
		  (send-super :set_values :XMN_DIRECTORY dir)
		  (if ivar_dir_selected_callproc
		      (funcall ivar_dir_selected_callproc dir))
		  )
		))

	;; Callback that fires when <return> entered into the "filter" text-field.
	(send (send-super :get_child :DIALOG_FILTER_TEXT) :add_callback
	      :XMN_ACTIVATE_CALLBACK '(CALLBACK_WIDGET)
	      '(
;;; :DO_SEARCH doesn't work on SGI IRIX's Motif (Grrr!!)
;;;		(send-super :do_search)
		(let ((dir (file:get-path (send CALLBACK_WIDGET :get_string))))
		  (send-super :set_values :XMN_DIRECTORY dir)
		  (if ivar_dir_selected_callproc
		      (funcall ivar_dir_selected_callproc dir))
		  )
		))
	))

;;;
;;;
;;;
(send WINTERP:FILE-SELECTION-WIDGET :answer :SET-FILE-SELECTED-CALLBACK-CLOSURE
      '(callproc)
      '(
	(setq ivar_file_selected_callproc callproc)
	))

;;;
;;;
;;;
(send WINTERP:FILE-SELECTION-WIDGET :answer :SET-DIR-SELECTED-CALLBACK-CLOSURE
      '(callproc)
      '(
	(setq ivar_dir_selected_callproc callproc)
	))

;;;
;;;
;;;
(send WINTERP:FILE-SELECTION-WIDGET :answer :GET-DIRECTORY-STR
      '()
      '(
	(xm_string_get_l_to_r
	 (car
	  (send-super :get_values :XMN_DIRECTORY nil)))
	))

;;;
;;;
;;;
(send WINTERP:FILE-SELECTION-WIDGET :answer :GET-FILEPATH-STR
      '()
      '(
	(xm_string_get_l_to_r
	 (car
	  (send-super :get_values :XMN_DIR_SPEC nil)))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/fileselect")
