;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         w_ctrlpnl.lsp
; RCS:          $Header: w_ctrlpnl.lsp,v 1.13 92/04/30 16:32:10 mayer Exp $
; Description:  A control panel for WINTERP, including a rudimentary way to edit
;               and send lisp to winterp's xlisp evaluator without having to use
;               the gnuemacs interface (src-client/winterp.el) or src-client/wl.c.
; Author:       Niels Mayer, HPLabs
; Created:      Thu Jun 14 17:26:59 1990
; Modified:     Fri Apr 24 08:52:01 1992 (Niels Mayer) mayer@hplmmws
; Language:     Lisp
; Package:      N/A
; Status:       X11r5 contrib tape release
;
; WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
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

;;; Example resource settings to put in ~/.Xdefaults or xrdb(1):
;;; WinterpCtrlPnl.title: Winterp Control Panel
;;; WinterpCtrlPnl.iconName: WinCtrlPnl
;;; WinterpCtrlPnl.geometry: -1+1
;;; WinterpCtrlPnl*edit*rows: 5
;;; WinterpCtrlPnl*edit*columns: 80
;;; WinterpCtrlPnl*files*listVisibleItemCount: 5
;;; WinterpCtrlPnl*files.foreground: white
;;; WinterpCtrlPnl*files.background:  dimgrey
;;; WinterpCtrlPnl*controlpanel.foreground: black
;;; WinterpCtrlPnl*controlpanel.background: lightgrey
;;; ! WinterpCtrlPnl*XmText*fontList: 6x10
;;; ! WinterpCtrlPnl*XmList*fontList: 6x10
;;; ! WinterpCtrlPnl*XmToggleButtonGadget*fontList: 6x10
;;; ! WinterpCtrlPnl*XmToggleButton*fontList: 6x10
;;; ! WinterpCtrlPnl*XmPushButtonGadget*fontList: 6x10
;;; ! WinterpCtrlPnl*XmPushButton*fontList: 6x10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  TO-DO --
;;;
;;; Recode callbacks for <--, -->, and "eval @ point" such that they use
;;; in ivar on editor_w holding the string-contents displayed in text widget,
;;; instead of calling :get_string each time needed. set up a modify callback
;;; such that if text is modified in the te widget, then ivar is set to NIL.
;;; any procs needing the text-str will note the NIL,  and replace it w/
;;; with current result of :get_string.
;;;
;;; Don't scan to end of file if parens mismatched... use heuristics
;;;
;;; get evaluator working right -- goes astray if file has #\( or #\)
;;; 
;;; get forw and prev working right -- gives out of bounds error unless
;;; cursor is in sexp, can't move between sexp's as in emacs C-M-F, C-M-B.
;;; 
;;; add eval-current-buffer.
;;; 
;;; add quit button (or change string "close" in wm-pulldown to indicate that
;;; it will quit WINTERP (due to using application-shell...)
;;; 
;;; BUG: if the code you're evaluating via "eval defun" button causes an error,
;;; you will end up seeing a backtrace going all the way back to the callback.
;;; Need to hotwire this so that evaluation actually calls the same evaluator
;;; loop in winterp.c:main(). Totally grody. (Note -- this is partially fixed by
;;; Motif 1.1 recursive eventloop bug workaround...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *SYSTEM-EDITOR*:
;;; if NIL, then edit functionality will use editor set in environment variable 
;;; $EDITOR. If set to a string, then that string will be used as the name of
;;; the editor to use for the "Edit" button.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *SYSTEM-EDITOR* nil)

;; For use in (send *w_ctrlpnl* :action) in txlation/accelerators on text widget
(setq *w_ctrlpnl:class*
      (send Class :new
	    '()				;no ivars
	    '()				;no cvars
	    ))
(setq *w_ctrlpnl:instance* (send *w_ctrlpnl:class* :new))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prev_paren(str pos)
  (let*					
      ;; local loop vars
      (
       (i pos)
       cur_char
       (paren_count 0)
       )

    (loop

     (setq cur_char (char str i))

     (cond 
      ((char= cur_char #\) ) 
       (setq paren_count (1+ paren_count))
       )
      ((and (> paren_count 0) (char= cur_char #\( ))
       (setq paren_count (1- paren_count))
       )
      )

     (if (and (zerop paren_count) (char= cur_char #\( ))
	 (return i))

     (setq i (1- i))

     (if (< i 0)
	 (return 'error))
     )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun next_paren(str pos)
  (let*					
      ;; local loop vars
      (
       (i pos)
       cur_char
       (paren_count 0)
       (str_length (length str))
       )

    (loop

     (setq cur_char (char str i))

     (if (and (zerop paren_count) (char= cur_char #\) ))
	 (return (1+ i)))

     (cond 
      ((char= cur_char #\( ) 
       (setq paren_count (1+ paren_count))
       )
      ((and (> paren_count 0) (char= cur_char #\) ))
       (setq paren_count (1- paren_count))
       )
      )

     (setq i (1+ i))

     (if (> i str_length)
	 (return 'error))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* (top_w 
       paned_w
       fsb_w
       controlpanel_w
       editor_w
       editfile_button_w
       loadfile_button_w
       savefile_button_w
       eval_button_w
       prev_button_w
       next_button_w
       debug_togglebutton_w
       trace_togglebutton_w
       gcmsg_togglebutton_w
#|
       continue_button_w
       go_prevlevel_button_w
       go_toplevel_button_w
|#
                           )

  (setq top_w
	(send APPLICATION_SHELL_WIDGET_CLASS :new
	      "winterpCtrlPnl" "WinterpCtrlPnl"
	      ))
  (setq paned_w
	(send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
	      "pane" top_w
	      ))
  (setq fsb_w
	(send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :managed
	      "files" paned_w
	      :XMN_DIR_MASK "*.lsp"
	      :XMN_MARGIN_HEIGHT 2 ;should be in app-defaults
	      :XMN_MARGIN_WIDTH  2 ;should be in app-defaults
	      :XMN_LIST_VISIBLE_ITEM_COUNT 4 ;should be in app-defaults
	      :XMN_ALLOW_RESIZE t	;paned_w constraint resource
	      :XMN_SKIP_ADJUST nil	;paned_w constraint resource
	      ))
  ;;
  ;; we don't want these fileselbox widgets around because they take up
  ;; too much space and don't provide useful functionality.
  ;; Unfortunately, if :DIALOG_APPLY_BUTTON and :DIALOG_OK_BUTTON are
  ;; not managed, the fsb_w's "Ok" and "Filter" actions will not be available
  ;; through the "Directories" and "Files" list widgets, nor through the 
  ;; "Filter" and "Selection" text widgets. This is lameness on the part of
  ;; Motif
  ;;
  (send (send fsb_w :get_child :DIALOG_OK_BUTTON) :unmanage)
  (send (send fsb_w :get_child :DIALOG_APPLY_BUTTON) :unmanage)
  (send (send fsb_w :get_child :DIALOG_CANCEL_BUTTON) :unmanage)
  (send (send fsb_w :get_child :DIALOG_HELP_BUTTON) :unmanage)
  (send (send fsb_w :get_child :DIALOG_SEPARATOR) :unmanage)

  (setq controlpanel_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "controlpanel" paned_w
	      :XMN_ADJUST_LAST      NIL
	      :XMN_ENTRY_ALIGNMENT  :ALIGNMENT_CENTER
	      :XMN_ORIENTATION      :HORIZONTAL
	      :XMN_PACKING          :NO_PACKING
	      ))
  (setq editor_w
	(send XM_TEXT_WIDGET_CLASS :new :managed :scrolled
	      "edit" paned_w
	      :XMN_EDIT_MODE :MULTI_LINE_EDIT
	      ))
  (setq eval_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "eval_button" controlpanel_w
	      :XMN_LABEL_STRING "Eval @ Point\n< C-M-X >"
	      ))
  (setq prev_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "prev_paren_button" controlpanel_w
	      :XMN_ARROW_DIRECTION :arrow_left
	      ))
  (setq next_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "next_paren_button" controlpanel_w
	      :XMN_ARROW_DIRECTION :arrow_right
	      ))
  (setq editfile_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "edit_button" controlpanel_w
	      :XMN_LABEL_STRING "Edit($EDITOR)\n < C-M-E >"
	      ))
  (setq loadfile_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "load_file_button" controlpanel_w
	      :XMN_LABEL_STRING "Load File\n< C-M-L >"
	      ))
  (setq savefile_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "save_file_button" controlpanel_w
	      :XMN_LABEL_STRING "Save File\n< C-M-S >"
	      ))
  (setq debug_togglebutton_w
	(send XM_TOGGLE_BUTTON_GADGET_CLASS :new :managed
	      "debug_tottlebutton" controlpanel_w
	      :XMN_LABEL_STRING "Debug"
	      :XMN_SET *breakenable*
	      ))
  (setq trace_togglebutton_w
	(send XM_TOGGLE_BUTTON_GADGET_CLASS :new :managed
	      "trace_togglebutton" controlpanel_w
	      :XMN_LABEL_STRING "Trace"
	      :XMN_SET *tracenable*
	      ))
  (setq gcmsg_togglebutton_w
	(send XM_TOGGLE_BUTTON_GADGET_CLASS :new :managed
	      "gcmsg_togglebutton" controlpanel_w
	      :XMN_LABEL_STRING "GC Msgs"
	      :XMN_SET *gc-flag*
	      ))
#|
  (setq continue_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
              "continue_button" controlpanel_w
	      :XMN_LABEL_STRING "Err-Cont"
	      ))
|#
#|
  (setq go_prevlevel_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
              "cleanup_button" controlpanel_w
	      :XMN_LABEL_STRING "Err-^Level"
	      ))
|#
#|
  (setq go_toplevel_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
              "toplevel_button" controlpanel_w
	      :XMN_LABEL_STRING "Err-~Level"
	      ))
|#

  (send top_w :realize)

  ;;
  ;; set constraint resources on controlpanel so that paned window
  ;; doesn't give it resize sashes.
  ;;
  (let (height)
    (send controlpanel_w :get_values :xmn_height 'height)

    ;; In the code below, the kludgery
    ;; "(if (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 0)) ...)"
    ;; is there to work around a name change between Motif 1.0 and 1.1:
    ;; :XMN_MAXIMUM --> :XMN_PANE_MAXIMUM and :XMN_MINIMUM -->:XMN_PANE_MINIMUM
    (send controlpanel_w :set_values
	  (if (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 0))
	      :XMN_MAXIMUM :XMN_PANE_MAXIMUM)
	  height
	  (if (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 0))
	      :XMN_MINIMUM :XMN_PANE_MINIMUM)
	  height
 	  ))
  (let (height)
    (send editor_w :get_values :xmn_height 'height)
    (send editor_w :set_values
	  (if (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 0))
	      :XMN_MAXIMUM :XMN_PANE_MAXIMUM)
	  height
	  (if (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 0))
	      :XMN_MINIMUM :XMN_PANE_MINIMUM)
	  height
	  ))

  ;; Since the "Ok" button is unmanaged, you can't get the default action
  ;; of the file selection box widget, nor the selection text. (The
  ;; "Ok" button is unmanaged because it takes up too much space in my
  ;; opinion...)
  ;; These callbacks come as close to that as possible:
  (let (
	(callback_sexp			;share same callback code bet two cbs
	 `(
	   (progv '(*breakenable*) '(nil)
		  (winterp_show_busy t)
		  (unwind-protect
		      (send ,editor_w :read_file
			    (xm_string_get_l_to_r (car (send ,fsb_w :get_values :XMN_TEXT_STRING nil)))
			    )
		    ;; unwind always
		    (winterp_show_busy nil)
		    ))
	   )
	 ))
    ;; a double click in the "files" list widget inside fsb_w
    ;; will cause the file to be shown in the viewer (editor_w).
    (send (send fsb_w :get_child :DIALOG_LIST) :set_callback
	  :xmn_default_action_callback '()
	  callback_sexp
	  )
    ;; <Return> in the selection text widget inside fsb_w
    ;; will cause the file to be show in the viewer (editor_w).
    (send (send fsb_w :get_child :DIALOG_TEXT) :set_callback
	  :xmn_activate_callback '()
	  callback_sexp
	  )
    )

  ;; Since the "Apply" button is unmanaged, you can't get access it's callback
  ;; which causes the files list to get refreshed with new data corresponding
  ;; to the filter text string and the chosen directory.... (The
  ;; "Apply" button is unmanaged because it takes up too much space in my
  ;; opinion...)
  ;; These callbacks come as close to that as possible:
  (let (
	(callback_sexp			;share same callback code bet two cbs
	 `((send ,fsb_w :do_search))
	 ))
    ;; Don't set up :DIALOG_DIR_LIST callback unless using
    ;; Motif 1.1 or greater. Motif 1.0 doesn't have :DIALOG_DIR_LIST
    ;; in the file selection box...
    (if (and (eq *MOTIF_VERSION* 1) (>= *MOTIF_REVISION* 1))
	;; a double click in the "files" list widget inside fsb_w
	;; will cause the file to be show in the viewer (editor_w).
	(send (send fsb_w :get_child :DIALOG_DIR_LIST) :set_callback
	      :xmn_default_action_callback '()
	      callback_sexp
	      )
      )
    ;; <Return> in the selection text widget inside fsb_w
    ;; will cause the file to be show in the viewer (editor_w).
    (send (send fsb_w :get_child :DIALOG_FILTER_TEXT) :set_callback
 	  :xmn_activate_callback '()
 	  callback_sexp
 	  )
    )

  (send editor_w :override_translations
      "Ctrl Meta <Key>B: Lisp(send *w_ctrlpnl:instance* :prev_paren)\
       Ctrl Meta <Key>F: Lisp(send *w_ctrlpnl:instance* :next_paren)\
       Ctrl Meta <Key>X: Lisp(send *w_ctrlpnl:instance* :eval_defun)\
       Ctrl Meta <Key>E: Lisp(send *w_ctrlpnl:instance* :edit_file)\
       Ctrl Meta <Key>S: Lisp(send *w_ctrlpnl:instance* :save_file)\
       Ctrl Meta <Key>L: Lisp(send *w_ctrlpnl:instance* :load_file)"
	)

  ;; Callback for "Edit File"
  (send *w_ctrlpnl:class* :answer :edit_file '()
	'(
	  (system (strcat
		   (if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
		   " "
		   (xm_string_get_l_to_r (car (send fsb_w :get_values :XMN_TEXT_STRING nil)))
		   " &"			;run it in the background so that winterp don't block...
		   ))
	  ))
  (send editfile_button_w :set_callback :xmn_activate_callback '()
	'(
	  (send *w_ctrlpnl:instance* :edit_file)
	  ))

  ;; Callback for "Load File"
  (send *w_ctrlpnl:class* :answer :load_file '()
	'(
;;	  (progv '(*breakenable*) '(nil)
		 (winterp_show_busy t)
		 (unwind-protect
		     (progn
		       (load (xm_string_get_l_to_r (car (send fsb_w :get_values :XMN_TEXT_STRING nil)))
			     :verbose t :print t)
		       (format T "; Done Loading\n\n")
		       )
		   ;; unwind always
		   (winterp_show_busy nil)
		   )
;;		 )
	  ))
  (send loadfile_button_w :set_callback :xmn_activate_callback '()
	'(				;Note: load_file is within "global" lexical scope
	  (send *w_ctrlpnl:instance* :load_file)
	  ))

  ;; Callback for "Save file"
  (send *w_ctrlpnl:class* :answer :save_file '()
	`(
	  (send ,editor_w :write_file
		(xm_string_get_l_to_r (car (send ,fsb_w :get_values :XMN_TEXT_STRING nil)))
		)
	  ))
  (send savefile_button_w :set_callback :xmn_activate_callback '()
	'(
	  (send *w_ctrlpnl:instance* :save_file)
	  ))

  ;; Callback for "Evaluate 'defun'"
  (send *w_ctrlpnl:class* :answer :eval_defun '()
	'(
;;	  (progv '(*breakenable*) '(nil)
		 (winterp_show_busy t)
		 (unwind-protect
		     (let*
			 ((str (send editor_w :get_string))
			  (cur_pos (send editor_w :GET_INSERTION_POSITION))
			  (begin_pos (prev_paren str cur_pos))
			  (end_pos (next_paren str cur_pos))
			  )
		       (send editor_w :SET_SELECTION begin_pos end_pos)
		       (send editor_w :SET_INSERTION_POSITION cur_pos)
		       (READ_EVAL_PRINT (make-string-input-stream (subseq str begin_pos end_pos)))
		       )
		   ;; unwind always
		   (winterp_show_busy nil)
		   )
;;		 )
	  ))
  (send eval_button_w :set_callback :xmn_activate_callback '()
	'(
	  (send *w_ctrlpnl:instance* :eval_defun)
	  ))

  ;; Callback for "( <--"
  (send *w_ctrlpnl:class* :answer :prev_paren '()
	'(
	  (progv '(*breakenable*) '(nil)
		 (winterp_show_busy t)
		 (unwind-protect
		     (send editor_w :SET_INSERTION_POSITION
			   (prev_paren 
			    (send editor_w :get_string)
			    (1- (send editor_w :GET_INSERTION_POSITION))))
		   ;; unwind always
		   (winterp_show_busy nil)
		   ))
	  ))
  (send prev_button_w :set_callback :xmn_activate_callback '()
	'(
	  (send *w_ctrlpnl:instance* :prev_paren)
	  ))

  ;; Callback for "--> )"
  (send *w_ctrlpnl:class* :answer :next_paren '()
	'(
	  (progv '(*breakenable*) '(nil)
		 (winterp_show_busy t)
		 (unwind-protect
		     (send editor_w :SET_INSERTION_POSITION
			   (next_paren 
			    (send editor_w :get_string)
			    (1+ (send editor_w :GET_INSERTION_POSITION))))
		   ;; unwind always
		   (winterp_show_busy nil)
		   ))
	  ))
  (send next_button_w :set_callback :xmn_activate_callback '()
	'(
	  (send *w_ctrlpnl:instance* :next_paren)
	  ))

  ;; Callback for "Debug"
  (send debug_togglebutton_w :set_callback :xmn_value_changed_callback '(callback_set)
	'(
	  (setq *breakenable* callback_set)
	  ))

  ;; Callback for "Trace"
  (send trace_togglebutton_w :set_callback :xmn_value_changed_callback '(callback_set)
	'(
	  (setq *tracenable* callback_set)
	  ))

  ;; Callback for "GC Msgs"
  (send gcmsg_togglebutton_w :set_callback :xmn_value_changed_callback '(callback_set)
	'(
	  (setq *gc-flag* callback_set)
	  ))

#|
  (if (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 1))
      ;; Motif 1.1 Callback for "Err-Cont" -- partial work around to X11r4 recursive event loop bugs
      (send continue_button_w :set_callback :xmn_activate_callback '()
	    '(
	      (system "wl '(continue)'") ;assumes 'wl' is on path...
	      ))
    ;; Motif 1.0 Callback for "Err-Cont".
    (send continue_button_w :set_callback :xmn_activate_callback '()
	  '(
	    (continue)
	    ))
    )
|#

#|
  (if (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 1))
      ;; Motif 1.1 Callback for "Err-^Level" -- partial work around to X11r4 recursive event loop bugs
      (send go_prevlevel_button_w :set_callback :xmn_activate_callback '()
	    '(
	      (system "wl '(clean-up)'") ;assumes 'wl' is on path...
	      ))
    ;; Motif 1.0 Callback for "Err-^Level"
    (send go_prevlevel_button_w :set_callback :xmn_activate_callback '()
	  '(
	    (clean-up)
	    ))
    )
|#

#|
  (if (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 1))
      ;; Motif 1.1 Callback for "Err-~Level" -- partial work around to X11r4 recursive event loop bugs
      (send go_toplevel_button_w :set_callback :xmn_activate_callback '()
	    '(
	      (system "wl '(top-level)'") ;assumes 'wl' is on path...
	      ))
    ;; Motif 1.0 Callback for "Err-~Level".
    (send go_toplevel_button_w :set_callback :xmn_activate_callback '()
	  '(
	    (top-level)
	    ))
    )
|#
  )
