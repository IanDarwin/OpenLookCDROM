; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         epoch-text.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/epoch-text.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  define Epoch_Widget_Class -- a text editor widget created by
;		reparenting an EPOCH multiwindow emacs editor window inside
;		a WINTERP/Motif widget. This allows you to edit files with a real
;		text editor while placing the Epoch-edit windows into your
;		WINTERP-based applications. Epoch is a multiwindow
;		gnu-emacs-based editor available for free by anonymous ftp
;		from cs.uiuc.edu. You must load epoch-widg.el into Epoch
;		first, as this file calls epoch-functions defined there.
;		This file also assumes that you have Andy Norman's gnuserv
;		package running under Epoch -- the program
;		/usr/local/epoch/bin/gnudoit is used to send emacs-lisp
;		commands to Epoch.
;		See also ../epoch-test.lsp and ../epoch-widg.el ...
; Author:       Niels P. Mayer
; Created:      Tue Mar  9 13:56:08 1993
; Modified:     Mon Jun  6 01:06:40 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define read-exec-cmd and other unixisms...

;; TODO -- be able to specify XMN_ROWS and XMN_COLUMNS
;; 	  (send Text_Viewer_Widget_Class :new :managed
;; 		"viewtext_w" paned_w
;; 		:XMN_ROWS		10
;; 		:XMN_COLUMNS		80
;; 		))

;; TODO -- :goto_line should highlight the current line in some way


(setq Epoch_Widget_Class
      (send Class :new
	    '(				;new instance vars
	      ;; This is used as a predicate to determine whether epoch-window has been
	      ;; initialized. It is used/set/cleared in the three callbacks below.
	      epoch-scrn-parent-win-id
	      ;; predicate -- if T, then prints out resize/expose callbacks
	      print-debug-msg-p
	      )
	    '()				;no class vars
	    XM_DRAWING_AREA_WIDGET_CLASS)) ;superclass

;;
;; Initialization method.
;;
(send Epoch_Widget_Class :answer :ISNEW
      '(managed_k name parent_w &rest args)
      '(
	;; initialize the instance variables
	(setq epoch-scrn-parent-win-id nil) ;initially set to NIL ==> initialization needed
	(setq print-debug-msg-p nil)

	;; Initialize the DRAWING_AREA_WIDGET -- we don't actually "draw" into
	;; the drawing area widget, instead, it becomes the parent window for
	;; the epoch screen. Handles resize and expose callbacks.
	(apply #'send-super :isnew managed_k name parent_w args) ;call superclass's init to create widget

	(send-super :add_callback :XMN_EXPOSE_CALLBACK
		    '(CALLBACK_WIDGET CALLBACK_WINDOW CALLBACK_REASON)
		    '(
		      (if (null epoch-scrn-parent-win-id) ;IF ... NOT INITIALIZED 
			  ;; THEN ... ON FIRST EXPOSE, CREATE EPOCH SCREEN, TRY TO SIZE IT?
			  (progn
			    ;; retrieve the x-window-id from the WINDOWOBJ retrieved from
			    ;; CALLBACK_WINDOW. The window-id is represented as an integer.
			    (setq epoch-scrn-parent-win-id (generic CALLBACK_WINDOW))
			    (send self :debug-prt "CREATE-EPOCH-SCREEN! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
				  epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
			    (read-exec-cmd
			     (format nil
				     "/usr/local/epoch/bin/gnudoit '(win-create-scrn \"~A\")'"
				     epoch-scrn-parent-win-id))

			    ;; THE FOLLOWING SETS THE SIZE OF THE WIDGET TO THE SIZE OF THE EPOCH WINDOW
			    (let ((epoch-screen-info ;#(x y width height border-w border-h map-state)
				   (read (make-string-input-stream 
					  (concatenate 'string "#" ;read it as array
						  (read-exec-cmd ;returns string "(x y width height border-w border-h map-state)"
						   (format nil
							   "/usr/local/epoch/bin/gnudoit '(epoch::screen-information (win-id-to-scrn \"~A\"))'"
							   epoch-scrn-parent-win-id)
						   )))))
				  )
			      (send CALLBACK_WIDGET :set_values
				    :XMN_WIDTH (aref epoch-screen-info 2) 
				    :XMN_HEIGHT (aref epoch-screen-info 3)
				    )
			      )
			    
			    ;; for cases where the widget size change above
			    ;; doesn't work, resize epoch to widget-size
			    (send self :resize-epoch-screen-to-widget-size)
			    )
			;; ELSE ... ONCE INITIALIZED, IGNORE EXPOSE EVENTS SINCE EPOCH TAKES CARE
			;; OF REFRESHING ITSELF
			(send self :debug-prt "IGNORE-EXPOSE-EPOCH-SCREEN! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
			      epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
			)
		      ))
	
	(send-super :add_callback :XMN_RESIZE_CALLBACK
		    '(CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
		    '(
		      (cond
		       ((and (null epoch-scrn-parent-win-id) (null CALLBACK_WINDOW)) ;NOT INITIALIZED and CALLBACK_WINDOW not valid
			(send self :debug-prt "INIT-RESIZE-SCREEN(IGNORED) window=~A, widget=~A, reason=~A\n"
			      CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
			)
		       ((null epoch-scrn-parent-win-id) ;NOT INITIALIZED, but CALLBACK_WINDOW is valid
			;; retrieve the x-window-id from the WINDOWOBJ retrieved from
			;; CALLBACK_WINDOW. The window-id is represented as an integer.
			(setq epoch-scrn-parent-win-id (generic CALLBACK_WINDOW))

			(send self :debug-prt "INIT-RESIZE-SCREEN! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
			      epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)

			(read-exec-cmd
			 (format nil
				 "/usr/local/epoch/bin/gnudoit '(win-create-scrn \"~A\")'"
				 epoch-scrn-parent-win-id))

			(let ((epoch-screen-info ;#(x y width height border-w border-h map-state)
			       (read (make-string-input-stream 
				      (concatenate 'string "#" ;read it as array
					      (read-exec-cmd ;returns string "(x y width height border-w border-h map-state)"
					       (format nil
						       "/usr/local/epoch/bin/gnudoit '(epoch::screen-information (win-id-to-scrn \"~A\"))'"
						       epoch-scrn-parent-win-id)
					       )))))
			      )
			  (send CALLBACK_WIDGET :set_values
				:XMN_WIDTH (aref epoch-screen-info 2) 
				:XMN_HEIGHT (aref epoch-screen-info 3)
				)
			  )
			)
		       ;; ELSE ... ON SUBSEQUENT RESIZES, UPDATE THE SIZE OF THE EPOCH WINDOW.
		       (T
			(send self :debug-prt "RESIZE-EPOCH-SCREEN! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
			      epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
			(send self :resize-epoch-screen-to-widget-size)
			)
		       )
		      ))

	(send-super :add_callback :XMN_DESTROY_CALLBACK
		    '(CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
		    '(
		      (send self :debug-prt "DESTROY-EPOCH-SCREEN!! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
			    epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)

		      (if epoch-scrn-parent-win-id
			  (progn
			    (read-exec-cmd
			     (format nil
				     "/usr/local/epoch/bin/gnudoit '(win-destroy-scrn \"~A\")'"
				     epoch-scrn-parent-win-id))
			    (setq epoch-scrn-parent-win-id nil)
			    ))
		      ))

	self
	))

;;
;;
;;
(send Epoch_Widget_Class :answer :DEBUG-PRT '(&rest args)
      '(
	(if print-debug-msg-p
	    (progn
	      (apply 'format `(T ,@args))
	      (fflush *terminal-io*)
	      ))
	))

;;
;;
;;
(send Epoch_Widget_Class :answer :DEBUG '(debug_p)
      '(
	(setq print-debug-msg-p debug_p)
	))

;;
;;
;;
(send Epoch_Widget_Class :answer :RESIZE-EPOCH-SCREEN-TO-WIDGET-SIZE '()
      '(
	(let* ((epoch-screen-info	;"(char-width char-height x y width height border-w border-h map-state)"
		(read (make-string-input-stream 
		       (concatenate 'string "#"	;read it as 9 element array "(char-width char-height x y width height border-w border-h map-state)"
			       (read-exec-cmd
				(format nil
					"/usr/local/epoch/bin/gnudoit '(win-get-epoch-screen-info \"~A\")'"
					epoch-scrn-parent-win-id)
				)
			       )
		       ))
		)
	       widget-pixel-width
	       widget-pixel-height
	       )

	  (send-super :get_values
		:XMN_WIDTH 'widget-pixel-width
		:XMN_HEIGHT 'widget-pixel-height
		)

	  (read-exec-cmd
	   (format nil
		   "/usr/local/epoch/bin/gnudoit '(epoch::change-screen-size ~A ~A (win-id-to-scrn \"~A\"))'"
		   (truncate (* widget-pixel-width
				(/
				 (float (aref epoch-screen-info 0)) ;epoch-screen-char-width
				 (aref epoch-screen-info 4) ;epoch-screen-pixel-width
				 )))
		   (truncate (* widget-pixel-height
				(/
				 (float (aref epoch-screen-info 1)) ;epoch-screen-char-height
				 (aref epoch-screen-info 5) ;epoch-screen-pixel-height
				 )))
		   epoch-scrn-parent-win-id
		   ))
	  )
	))


; /*****************************************************************************
;  * (send <text_widget> :WRITE_FILE <filename>)
;  *	--> on success, returns a FIXNUM -- the length of the file;
;  *	    on failure to write file, signals an error.
;  *
;  * Portions of this were taken from OReilly-Motif/ch15/editor.c:
;  * Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
;  * This program is freely distributable without licensing fees and
;  * is provided without guarantee or warrantee expressed or implied.
;  * This program is -not- in the public domain.
;  ****************************************************************************/
; 
; /*****************************************************************************
;  * (send <text_widget> :READ_FILE_GOTO_LINE <filename> <linenum> <highlight_p>)
;  *	--> on success, returns a FIXNUM -- the length of the file;
;  *	    on failure to read file, signals an error.
;  * <filename> is a string, the full path to the file to be read.
;  * <linenum> is a fixnum >=0, the line to display.
;  * <higlight_p> is a boolean, if non-NIL the highlight the selected line.
;  ****************************************************************************/
; 
; /*****************************************************************************
;  * (send <text_widget> :SEARCH <search-str>)
;  * 	--> returns NIL if <search-str> not found in <text-w>
;  *	    returns a FIXNUM == the position of the text on success.
;  *
;  * Portions of this were taken from OReilly-Motif/ch15/search_text.c:
;  * Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
;  * This program is freely distributable without licensing fees and
;  * is provided without guarantee or warrantee expressed or implied.
;  * This program is -not- in the public domain.
;  ****************************************************************************/

;; Similar to wc_Text.c:
;;/*****************************************************************************
;; * (send <text_widget> :READ_FILE <filename>)
;; *	--> on success, returns a FIXNUM -- the length of the file;
;; *	    on failure to read file, signals an error.
;; ****************************************************************************/
(send Epoch_Widget_Class :answer :READ_FILE '(filename)
      '(
	(if epoch-scrn-parent-win-id
	    ;;IF INITIALIZED THEN
	    (read-fixnum-exec-cmd	; to make this method work like XM_TEXT_WIDGET_CLASS', must return-
	     (format nil		;-FIXNUM representing the length of the file.
		     "/usr/local/epoch/bin/gnudoit '(win-find-file-in-screen \"~A\" \"~A\")'"
		     epoch-scrn-parent-win-id
		     filename
		     ))
	  ;;ELSE
	  (error "Epoch_Widget_Class -- internal error: uninitialized epoch widget")
	  )
	))

; /*****************************************************************************
;  * (send <text_widget> :GOTO_LINE <linenum> <highlight_p>)
;  *	--> returns the FIXNUM position of the desired current line.
;  * <linenum> is a fixnum >=0, the line to display.
;  * <higlight_p> is a boolean, if non-NIL the highlight the selected line.
;  ****************************************************************************/
(send Epoch_Widget_Class :answer :GOTO_LINE '(linenum highlight_p)
      '(
	(if highlight_p
	    (setq highlight_p "t")	;emacs is case sensitive t != T
	  (setq highlight_p "nil")	;emacs is case sensitive nil != NIL
	  )
	(if epoch-scrn-parent-win-id
	    ;;IF INITIALIZED THEN
	    (read-fixnum-exec-cmd	; to make this method work like XM_TEXT_WIDGET_CLASS', must return-
	     (format nil		;-FIXNUM representing the character posn of the current line
		     "/usr/local/epoch/bin/gnudoit '(win-goto-line-in-screen \"~A\" ~A ~A)' 2> /dev/null"
		     epoch-scrn-parent-win-id linenum highlight_p))
	  ;;ELSE
	  (error "Epoch_Widget_Class -- internal error: uninitialized epoch widget")
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/epoch-text")
