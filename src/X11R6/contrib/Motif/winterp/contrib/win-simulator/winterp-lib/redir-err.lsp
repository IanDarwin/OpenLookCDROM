;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         redir-err.lsp
; RCS:          $Header: redir-err.lsp,v 1.3 92/04/20 19:37:10 mayer Exp $
; Description:  After loading this file, all output to stderr from WINTERP and
;		it's subprocesses (via exp_popen exp_spawn system popen) gets 
;		output to a XmText widget. If popped-down, this window pops
;		up upon new activity on stderr.
; Author:       Niels Mayer, HPLabs
; Created:      January 1992
; Modified:     Tue Feb 18 22:40:37 1992 (Niels Mayer) mayer@hplnpm
; Language:     Lisp
; Status:       X11r5 contrib tape release
;
; WINTERP Copyright 1989, 1990, 1991, 1992 Hewlett-Packard Company (by Niels Mayer).
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

;;; Icon stolen from HP Softbench
;;; Mwm*winterp-stderr-warn-shell*iconImage: usr/softbench/icons/action.h

(let*
    (;; loc vars
     (top-w
      (send TOP_LEVEL_POPUP_SHELL_WIDGET_CLASS :new 
	    "winterp-stderr-warn-shell" *TOPLEVEL_WIDGET*
	    :XMN_GEOMETRY		"+100+100"
	    :XMN_TITLE			"Winterp: Stderr Output Warning"
	    :XMN_ICON_NAME		"W:Stderr"
	    :XMN_DELETE_RESPONSE	:UNMAP
;;;	    :XMN_ICON_PIXMAP		(xm_get_pixmap "/usr/local/include/X11/bitmaps-softbench/action.h" "white" "black")
;;;	    :XMN_ICON_MASK		(xm_get_pixmap "/usr/local/include/X11/bitmaps-softbench/actionMask.h" "white" "black")
	    ))
     (form-w
      (send XM_FORM_WIDGET_CLASS :new :managed
	    "form" top-w
	    ))
     (fix-form-fuckup-rc-w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "fix-form-fuckup-rc" form-w
	    :XMN_ORIENTATION		:horizontal
	    :XMN_PACKING		:pack_tight
	    :XMN_ENTRY_BORDER		0
	    :XMN_MARGIN_HEIGHT		0
	    :XMN_MARGIN_WIDTH		0
	    :XMN_TOP_ATTACHMENT		:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    ))
     (closbut-w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed ;NOTE--THIS MUST BE A WIDGET, else :call_action_proc "ArmAndActivate" below won't work
	    "close-but" fix-form-fuckup-rc-w
	    :XMN_LABEL_STRING		"Close Window"
	    :XMN_ALIGNMENT		:alignment_center
	    :XMN_FILL_ON_ARM		t
	    :XMN_SHOW_AS_DEFAULT	t
	    :XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
	    ))
     (msg-frame-w
      (send XM_FRAME_WIDGET_CLASS :new :managed
	    "msg-frame" fix-form-fuckup-rc-w
	    :XMN_MARGIN_HEIGHT		0
	    :XMN_MARGIN_WIDTH		0
	    ))
     (msg-rc-w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "msg-rc" msg-frame-w
	    :XMN_ORIENTATION		:horizontal
	    :XMN_PACKING		:pack_tight
	    :XMN_ENTRY_BORDER		0
	    :XMN_MARGIN_HEIGHT		0
	    :XMN_MARGIN_WIDTH		0
	    ))
     (msg-sym-w
      (send XM_LABEL_GADGET_CLASS :new :managed
	    "msg-sym" msg-rc-w
     	    :XMN_LABEL_TYPE		:pixmap
     	    :XMN_LABEL_PIXMAP		"default_xm_warning"
	    :XMN_ALIGNMENT		:alignment_center
     	    ))
     (msg-label-w
      (send XM_LABEL_GADGET_CLASS :new :managed
	    "msg-label" msg-rc-w
     	    :XMN_LABEL_TYPE		:string
     	    :XMN_LABEL_STRING		"Warning -- Error messages on stderr:"
	    :XMN_ALIGNMENT		:alignment_beginning
     	    ))
     (clearbut-w
      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	    "clear-but" fix-form-fuckup-rc-w
	    :XMN_LABEL_STRING		"Clear messages"
	    :XMN_ALIGNMENT		:alignment_center
	    :XMN_DEFAULT_BUTTON_SHADOW_THICKNESS 2
	    ))
     (sep-w
      (send XM_SEPARATOR_GADGET_CLASS :new :managed
	    "sep" form-w
	    :XMN_ORIENTATION		:horizontal
	    :XMN_TOP_ATTACHMENT         :attach_widget
	    :XMN_TOP_WIDGET		fix-form-fuckup-rc-w
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    ))
     (te-w
      (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled
	    "text" form-w
	    :XMN_EDIT_MODE		:multi_line_edit    
	    :XMN_AUTO_SHOW_CURSOR_POSITION T
	    :XMN_EDITABLE		nil ;don't allow user to change text.
	    :XMN_ROWS			4 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
	    :XMN_COLUMNS		80 ;!!SHOULD PROBABLY BE SET IN APP-DEFAULT!!
	    :XMN_TOP_ATTACHMENT		:attach_widget
	    :XMN_TOP_WIDGET		sep-w
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_BOTTOM_ATTACHMENT	:attach_form
	    ))
     (inspos 0)
     )

  ;;
  ;; This callback clears out previous text in the text widget...
  ;;
  (send clearbut-w :add_callback :xmn_activate_callback '() 
	'(
	  (send te-w :set_string "")
	  (setq inspos 0)
	  ))

  ;;
  ;; this causes the ok button to be pressed when <return> is entered within 
  ;; XmForm-->XmBulletinBoard 
  ;;
  (send form-w :set_values :xmn_default_button closbut-w)
  (send closbut-w :add_callback :xmn_activate_callback '() 
	'(
	  (send top-w :popdown)
	  ))

  ;;
  ;; The following two statements cause the ok button to be pressed when
  ;; <return> is entered in the text widget. Return doesn't get set in XmText
  ;; despite XmNdefaultButton setting in XmForm-->XmBulletinBoard parent...
  ;;
  (send te-w :override_translations "<Key>Return: activate()")
  (send te-w :add_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	'(
	  (send closbut-w :call_action_proc "ArmAndActivate" CALLBACK_XEVENT)
	  ))

  (xt_add_input				;add input callback that fires whenever data readable
   (REDIRECT_STDERR)
   :read
   '(					;FDINPUTCB_FILE is locally bound inside inputcb
     ;; reset insert position to saved position incase user changed it...
     (send te-w :set_insertion_position inspos)
     ;; read as much as we can in one chunk using fscanf-string...
     (let ((s (fscanf-string FDINPUTCB_FILE "%[^\n]")))
;;;    (format t "called input callback -- '~A' \n" s)
       (if s
	   (progn
	     (send te-w :replace inspos inspos s)
	     (setq inspos (send te-w :get_insertion_position))
	     )))

     ;; fscanf-string didn't necessarily read full line due to asynch nature of input
     ;; and use of nonblocing files in REDIRECT_STDERR -- thus we must read everything
     ;; after fscanf-sring char-by-character until we hit EOF or a new-line
     (do ((c (read-char FDINPUTCB_FILE)
	     (read-char FDINPUTCB_FILE)
	     ))
	 ((or (null c) (char= c #\newline)) ;terminate do loop at EOF or EOL
	  (if c				;if we hit newline, put newline into te-w
	      (progn (send te-w :replace inspos inspos (format nil "~A" c))
		     (setq inspos (send te-w :get_insertion_position)))
	    )
	  )
	 ;; append chars into te-w
	 (send te-w :replace inspos inspos (format nil "~A" c))
	 (setq inspos (send te-w :get_insertion_position))
	 )

     (send te-w :show_position inspos)

     (send top-w :popup :grab_none)
     )
   )
  )
