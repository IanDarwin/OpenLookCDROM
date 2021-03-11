; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_sim-progview.lsp
; RCS:          $Header: $
; Description:  Simulator_Program_Viewer_Widget_Class and Methods
; Author:       Niels Mayer, HPLabs
; Created:      Thu Aug 27 23:25:42 1992
; Modified:     Sun Jun  5 16:33:56 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       Experimental (Do Not Distribute)
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE Simulator_Program_Viewer_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Simulator_Program_Viewer_Widget_Class
  (send Class :new
	'(				;new instance vars
	  newline-array			;array of positions such that the elt at
					;  newline-array[j-1] is the posn of line 'j'
	  file-path			;path+name (string) to file being displayed
	  
	  current-line-num		;the line-number where the point is currently at.
	  ) 
	'()				;no class vars
	XM_TEXT_WIDGET_CLASS		;superclass
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR Simulator_Message_Area_Widget_Class
;;
;; (send Simulator_Message_Area_Widget_Class :new {:managed,:unmanaged}
;;       <name-str> <parent-w>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Program_Viewer_Widget_Class :answer :ISNEW
      '(managed-kwd name-str parent-w &rest args)
      '(
	(apply 'send-super
	       `(:isnew ,managed-kwd	;splice in method arguments passed in above
			:scrolled	;this is a scrolled text widget...
			,name-str ,parent-w
			;; XmText resources
			:XMN_EDIT_MODE			:multi_line_edit
			:XMN_EDITABLE			nil ;don't allow user to change text.
			:XMN_AUTO_SHOW_CURSOR_POSITION	t
			:XMN_CURSOR_POSITION_VISIBLE	t
			,@args
			))

	(setq newline-array nil)
	(setq file-path nil)
	(setq current-line-num nil)

	(send self :add-all-callbacks)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK INITIALIZATION METHOD FOR Simulator_Program_Viewer_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Program_Viewer_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	;; this callback will, upon mouse-click or cursor position movement,
	;; set the ivar current-line-num to the selected line.
	(send self :add_callback :XMN_MOTION_VERIFY_CALLBACK
	      '(CALLBACK_NEW_INSERT)
	      '(
		(if newline-array	;don't do anything if :find_file hasn't been called...
		    ;; figure out which line the cursor is on
		    (let ((len (length newline-array)))
		      (do ((cur-line 0 (1+ cur-line)) ;iterators
			   )
			  ((or		;test & return...
			    (>= cur-line len)
			    (>= (aref newline-array cur-line) CALLBACK_NEW_INSERT))
			   (if (= cur-line 0)
			       (setq current-line-num 1) ;edge-condition hack for when CALLBACK_NEW_INSERT==0
			     (setq current-line-num cur-line)
			     )
			   )
			  ;; no do-loop body
			  ))
		  )
		))
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :FIND_FILE
;;
;; (send <prog-vu-w> :find_file <filename-str>)
;;
;; Displays file <filename-str> in the text widget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Program_Viewer_Widget_Class :answer :FIND_FILE '(filename)
      '(
	(cond
	 ((string= filename file-path)	;if the file was already read into widget
	  nil				;do nothing
	  )
	 (t				;else read the file into the widget...
	  (send-super :set_highlight 0 (send-super :get_last_position) :highlight_normal) ;clear out old highlights
	  (setq newline-array nil)
	  (setq current-line-num nil)
	  (send-super :read_file filename) ;this reads the file into the XmText widget
	  (setq file-path filename)

	  ;; setup newline-array s.t. newline-array[j-1] == posn of line 'j'
	  (let* ((newline-list '(0))	;first line is at position 0...
		 (str (send-super :get_string))	;get the string in the XmText widget
		 (len (length str))
		 )	
	    (do ((i 0 (1+ i))		;iterators
		 )
		((>= i len)		;test &
		 nil			;return
		 )
		;; do-loop body
		(if (char= (char str i) #\newline)
		    (setq newline-list (cons i newline-list))
		  )
		)
	    (setq newline-array (map 'array #'(lambda (x) x) (reverse newline-list)))
	    )
	  )
	 )
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :HIGHLIGHT-CURRENT-LINE
;;
;; (send <prog-vu-w> :highlight-current-line)
;;	--> returns the current line, or NIL if none set.
;;
;; Highlights the line containing the cursor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Program_Viewer_Widget_Class :answer :HIGHLIGHT-CURRENT-LINE '()
      '(
	(if (and current-line-num newline-array)
	    (cond
	     ((= current-line-num 1)
	      (send-super :set_highlight
			  0
			  (1+ (aref newline-array 1))
			  :highlight_selected)
	      )
	     ((>= current-line-num (length newline-array))
	      (send-super :set_highlight
			  (1+ (aref newline-array (1- current-line-num)))
			  (send-super :get_last_position)
			  :highlight_selected)
	      )
	     (T
	      (send-super :set_highlight
			  (1+ (aref newline-array (1- current-line-num)))
			  (1+ (aref newline-array current-line-num))
			  :highlight_selected)
	      )
	     ))
	current-line-num		;return
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :UNHIGHLIGHT-CURRENT-LINE
;;
;; (send <prog-vu-w> :unhighlight-current-line)
;; 	--> returns the current line, or NIL if none set.
;;
;; Un-Highlights the line containing the cursor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Program_Viewer_Widget_Class :answer :UNHIGHLIGHT-CURRENT-LINE '()
      '(
	(if (and current-line-num newline-array)
	    (cond
	     ((= current-line-num 1)
	      (send-super :set_highlight
			  0
			  (1+ (aref newline-array 1))
			  :highlight_normal)
	      )
	     ((>= current-line-num (length newline-array))
	      (send-super :set_highlight
			  (1+ (aref newline-array (1- current-line-num)))
			  (send-super :get_last_position)
			  :highlight_normal)
	      )
	     (T
	      (send-super :set_highlight
			  (1+ (aref newline-array (1- current-line-num)))
			  (1+ (aref newline-array current-line-num))
			  :highlight_normal)
	      )
	     ))
	current-line-num		;return
	))
