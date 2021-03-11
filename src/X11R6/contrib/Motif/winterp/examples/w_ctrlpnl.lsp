;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         w_ctrlpnl.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/w_ctrlpnl.lsp,v 2.9 1994/06/06 14:43:03 npm Exp $
; Description:  A control panel for WINTERP, including a rudimentary way to edit
;               and send lisp to winterp's xlisp evaluator without having to use
;               the gnuemacs interface (src-client/winterp.el) or src-client/wl.c.
;		For details on this application, see ../doc/winterp.doc section
;		<<Interacting with WINTERP via the "Winterp Control Panel":>>.
; Author:       Niels Mayer
; Created:      Thu Jun 14 17:26:59 1990
; Modified:     Sun Jun  5 19:24:06 1994 (Niels Mayer) npm@indeed
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

(require "lib-widgets/fileselect")	;define WINTERP:FILE-SELECTION-WIDGET, :set-file-selected-callback-closure, :set-dir-selected-callback-closure, :get-filepath-str
(require "lib-utils/show-busy")		;define WINTERP-SHOW-BUSY-PROGN
(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*

;;; Example resource settings to put in ~/.Xdefaults or xrdb(1):

;;; WinterpCtrlPnl.iconic: false
;;; WinterpCtrlPnl.geometry: -1+1
;;; WinterpCtrlPnl*edit*rows: 24
;;; WinterpCtrlPnl*edit*columns: 80
;;; WinterpCtrlPnl*files*listVisibleItemCount: 9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  TO-DO --
;;;
;;; * Recode callbacks for <--, -->, and "eval @ point" such that they use
;;; in ivar on editor_w holding the string-contents displayed in text widget,
;;; instead of calling :get_string each time needed. set up a modify callback
;;; such that if text is modified in the te widget, then ivar is set to NIL.
;;; any procs needing the text-str will note the NIL,  and replace it w/
;;; with current result of :get_string.
;;;
;;; * Don't scan to end of file if parens mismatched... use heuristics
;;;
;;; * get evaluator working right -- goes astray if file has #\( or #\)
;;; 
;;; * add eval-current-buffer.
;;; 
;;; * add quit button (or change string "close" in wm-pulldown to indicate that
;;; it will quit WINTERP (due to using application-shell...)
;;; 
;;; * BUG: if the code you're evaluating via "eval defun" button causes an error,
;;; you will end up seeing a backtrace going all the way back to the callback.
;;; Need to hotwire this so that evaluation actually calls the same evaluator
;;; loop in winterp.c:main().
;;;
;;; * If you click on "Eval(<-->)" "(<--" "-->)" and "Format(<-->)" the position
;;; of the opening and closing paren should flash. Unfortunately, some revision
;;; of Motif 1.2 turn off the cursor in the text widget unless it has focus.
;;; Therefore, you only see what s-expression is being "pointed-at" when your
;;; focus is in the text widget. I used to have the selected s-expression highlight
;;; but that would have weird bugs in some versions of Motif as well...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SET XLISP GLOBAL *breakenable*: don't allow entry into brkloop by default
;; because break will cause winterp to go x-uninteractive. Going x-uninteractive
;; may startle new users....
(setq *breakenable* nil)		
				
;; SET XLISP GLOBAL *tracenable*: when debug-toggle set or *breakenable*==T
;; with *tracenable*==T, you'll get a backtrace, otherwise you won't.
(setq *tracenable* t)			

;; '(debug)' defined in lib-utils/initialize.lsp: "(debug) - enable debug breaks".
;; override here to sets the controlpanel toggle buttons appropriately.
(defun debug ()
  ;; :SET-DEBUG-STATE will toggle *breakenable*
  (if *WINTERP_CTRL_PNL*
      (send *WINTERP_CTRL_PNL* :SET-DEBUG-STATE t)
    ))
  
;; '(nodebug)' defined in lib-utils/initialize.lsp: "(nodebug) - disable debug breaks".
;; override here to set the controlpanel toggle button too.
(defun nodebug ()
  ;; :SET-DEBUG-STATE will toggle *breakenable*
  (if *WINTERP_CTRL_PNL*
      (send *WINTERP_CTRL_PNL* :SET-DEBUG-STATE nil)
    ))

;; an easy way to pop up the control panel from the terminal or emacs,
;; even if iconified.
(defun ctrlpnl ()
  (if *WINTERP_CTRL_PNL*
      (send *WINTERP_CTRL_PNL* :map_raised)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *SYSTEM-EDITOR*:
;;; if NIL, then edit functionality will use editor set in environment variable 
;;; $EDITOR. If set to a string, then that string will be used as the name of
;;; the editor to use for the "Edit" button.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *SYSTEM-EDITOR* nil)

(defvar *WINTERP_CTRL_PNL* nil)		;below, bound to inst of *w_ctrlpnl:class*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; *W_CTRLPNL:EDIT-WIDGET-CLASS* ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *w_ctrlpnl:edit-widget-class*
      (send Class :new
	    '(				;IVARS
	      app_w			;instance of *w_ctrlpnl:class*
	      )				
	    '()				;no CVARS
	    XM_TEXT_WIDGET_CLASS	;superclass
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR *w_ctrlpnl:edit-widget-class*
;;
;; (send *w_ctrlpnl:edit-widget-class* :new [:managed|:unmanaged]
;;       <name-str> <parent-w>
;;	 <application-widget> -- NOTE SPECIAL ARG, INSTANCE OF *w_ctrlpnl:class*
;;	 [[resource-name resource-value] ...]
;;  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send *w_ctrlpnl:edit-widget-class* :answer :ISNEW
      '(managed-k name-str parent-w
		  application-widget	;NOTE SPECIAL ARG, INSTANCE OF *w_ctrlpnl:class*
		  &rest args)
      '(
	;; initialize instance vars
	(setq app_w application-widget)

	;; create 'self', an instance of XM_TEXT_WIDGET_CLASS
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed-k :scrolled
	       name-str parent-w
	       :XMN_EDIT_MODE	:multi_line_edit
	       :XMN_EDITABLE	t
	       :XMN_BLINK_RATE	0	;flashing interferes w/ "flash" of matching parens on C-M-F/C-M-B
	       :XMN_CURSOR_POSITION_VISIBLE t
	       :XMN_AUTO_SHOW_CURSOR_POSITION t
	       args)			;splice in method arguments passed in above

	(send-super :override_translations
	      "Ctrl Meta <Key>B: Lisp(send ACTION_WIDGET :prev_paren)  \
               Ctrl Meta <Key>F: Lisp(send ACTION_WIDGET :next_paren)  \
               Ctrl Meta <Key>X: Lisp(send ACTION_WIDGET :eval_defun)  \
               Ctrl Meta <Key>Q: Lisp(send ACTION_WIDGET :format_defun)\
               Ctrl Meta <Key>E: Lisp(send ACTION_WIDGET :edit_file)   \
               Ctrl Meta <Key>S: Lisp(send ACTION_WIDGET :save_file)   \
               Ctrl Meta <Key>L: Lisp(send ACTION_WIDGET :load_file)   "
	      )
	))

;; Callback for "Evaluate 'defun'"
(send *w_ctrlpnl:edit-widget-class* :answer :EVAL_DEFUN '()
      '(
;;;	(progv '(*breakenable*) '(nil)
	(winterp_show_busy t)
	(unwind-protect
	    (let*
		((str       (send-super :get_string))
		 (max_pos   (1- (length str)))
		 (cur_pos   (min (send-super :get_insertion_position) max_pos))
		 (begin_pos (if (char= #\( (char str cur_pos))
				cur_pos
			      (prev_paren str cur_pos)))
		 (end_pos   (if (char= #\) (char str cur_pos))
				cur_pos
			      (if (eq begin_pos 'error)
				  'error
				(next_paren str begin_pos)
				)))
		 )
	      (cond
	       ((or (eq begin_pos 'error) (eq end_pos 'error))
		(X_BELL)
		(send-super :show_position cur_pos)
		(send-super :set_insertion_position cur_pos)
		)
	       (t
		(send-super :show_position end_pos)
		(send-super :set_insertion_position end_pos)
		(send-super :update_display 250000)
		(send-super :show_position begin_pos)
		(send-super :set_insertion_position begin_pos)
		(send-super :update_display 250000)
		(unwind-protect
		    (READ_EVAL_PRINT
		     (make-string-input-stream
		      (subseq str begin_pos (1+ end_pos))))
		  ;; unwind always
		  (send-super :show_position cur_pos)
		  (send-super :set_insertion_position cur_pos)
		  )
		)
	       )
	      )
	  ;; unwind always
	  (winterp_show_busy nil)
	  )
;;;	)
	))

;; Callback for "Format 'defun'" -- pretty print it...
;; this is currently somewhat hoaky as XLISP-PLUS's pretty printer
;; doesn't format things very nicely as compared to gnuemacs' 
;; indent-sexp command.
;; BUG: activating this causes you to lose
;; 	(1) case information -- XLISP's reader is case-insensitive.
;; 	(2) comments -- XLISP's reader ignores comments
;; 	(3) formatting information, such as tabs.
;; however, for people too stupid to use gnuemacs, this is better
;; than nothing at all.
(send *w_ctrlpnl:edit-widget-class* :answer :FORMAT_DEFUN '()
      `(
;;;	       (progv '(*breakenable*) '(nil)
	(winterp_show_busy t)
	(require "xlisp-2.1d/classes")
	(require "xlisp-2.1d/pp")
	(unwind-protect
	    (let*
		((str       (send-super :get_string))
		 (max_pos   (1- (length str)))
		 (cur_pos   (min (send-super :get_insertion_position) max_pos))
		 (begin_pos (if (char= #\( (char str cur_pos))
				cur_pos
			      (prev_paren str cur_pos)))
		 (end_pos   (if (char= #\) (char str cur_pos))
				cur_pos
			      (if (eq begin_pos 'error)
				  'error
				(next_paren str begin_pos)
				)))
		 )
	      (cond
	       ((or (eq begin_pos 'error) (eq end_pos 'error))
		(X_BELL)
		(send-super :show_position cur_pos)
		(send-super :set_insertion_position cur_pos)
		)
	       (t
		(send-super :show_position end_pos)
		(send-super :set_insertion_position end_pos)
		(send-super :update_display 250000)
		(send-super :show_position begin_pos)
		(send-super :set_insertion_position begin_pos)
		(send-super :update_display 250000)
		(let ((strm (make-string-output-stream)))
		  (pp (read (make-string-input-stream
			     (subseq str begin_pos (1+ end_pos))))
		      strm)
		  (send-super :replace begin_pos (1+ end_pos)
			(let ((st (get-output-stream-string strm)))
			  (subseq st 0 (1- (length st))) ;truncate last char, since it's a newline
			  ))
		  )
		))
	      )
	  ;; unwind always
	  (winterp_show_busy nil)
	  )
;;;		 )
	))

;; Callback for "( <--"
(send *w_ctrlpnl:edit-widget-class* :answer :PREV_PAREN '()
      `(
	(winterp-show-busy-progn
	 (let*
	     ((str       (send-super :get_string))
	      (max_pos   (1- (length str)))
	      (cur_pos   (min (send-super :get_insertion_position) max_pos))
	      (pre_pos   (prev_paren str
				     (if (char= #\( (char str cur_pos))
					 (1- cur_pos)
				       cur_pos)
				     ))
	      )
	   (if (eq 'error pre_pos)
	       (X_BELL)
	     (send-super :set_insertion_position pre_pos))
	   ))
	))

;; Callback for "--> )"
(send *w_ctrlpnl:edit-widget-class* :answer :NEXT_PAREN '()
      '(
	(winterp-show-busy-progn
	 (let*
	     ((str       (send-super :get_string))
	      (max_pos   (1- (length str)))
	      (cur_pos   (min (send-super :get_insertion_position) max_pos))
	      (nex_pos	 (next_paren str cur_pos))
	      )
	   (if (eq nex_pos 'error)
	       (X_BELL)
	     (send-super :set_insertion_position nex_pos))
	   ))
	))

(send *w_ctrlpnl:edit-widget-class* :answer :LOAD_FILE '()
      '(
	(send app_w :load_file)
	))

(send *w_ctrlpnl:edit-widget-class* :answer :SAVE_FILE '()
      '(
	(send app_w :save_file)
	))

(send *w_ctrlpnl:edit-widget-class* :answer :EDIT_FILE '()
      '(
	(send app_w :edit_file)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prev_paren(str pos)
  (let*					
      ;; local loop vars
      (
       (i pos)
       (paren_count 0)
       cur_char
       )

    (if (<= pos 0)
	'error				;RETURN
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
	   (return i))			;RETURN

       (setq i (1- i))

       (if (< i 0)
	   (return 'error))		;RETURN
       )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun next_paren(str pos)
  (let*					
      ;; local loop vars
      ((i pos)
       (paren_count 0)
       (max_pos (1- (length str)))
       cur_char
       )
    (if (> pos max_pos)
	'error				;RETURN
      (loop
       (setq cur_char (char str i))
       (cond 
	((and (eq 1 paren_count) (char= cur_char #\) ))
	   (return i)			;RETURN
	   ) 
	((char= cur_char #\( ) 
	 (setq paren_count (1+ paren_count))
	 )
	((and (> paren_count 0) (char= cur_char #\) ))
	 (setq paren_count (1- paren_count))
	 )
	)

       (setq i (1+ i))

       (if (> i max_pos)
	   (return 'error))		;RETURN
       )
      ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *W_CTRLPNL:CLASS* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *w_ctrlpnl:class*
      (send Class :new
	    '(				;IVARS
	      debug_options_w
	      editor_w
	      fsb_w
	      )				
	    '()				;no CVARS
	    APPLICATION_SHELL_WIDGET_CLASS ;superclass
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR *w_ctrlpnl:class*
;;
;; (send *w_ctrlpnl:class* :new
;;       <name-str>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send *w_ctrlpnl:class* :answer :ISNEW
      '(name-str class-str &rest args)
      '(
	;; create 'self', an instance of APPLICATION_SHELL_WIDGET_CLASS
	(apply #'send-super :isnew	;call superclass's init to create widget
	       name-str class-str
	       :XMN_ALLOW_SHELL_RESIZE T ;allow temporary resize to display longer string in message area
	       args)			;splice in method arguments passed in above

	(let (paned_w controlpanel_w editfile_button_w
		      loadfile_button_w showfile_button_w
		      savefile_button_w eval_button_w fmt_button_w
		      prev_button_w next_button_w)

	  (setq paned_w
		(send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
		      "pane" self
		      ))
	  (setq fsb_w
		(send WINTERP:FILE-SELECTION-WIDGET :new :managed
		      "files" paned_w
		      :XMN_PATTERN		 "*.lsp"
		      :XMN_MARGIN_HEIGHT	 2 ;should be in app-defaults
		      :XMN_MARGIN_WIDTH		 2 ;should be in app-defaults
		      :XMN_LIST_VISIBLE_ITEM_COUNT 4 ;should be in app-defaults
		      :XMN_ALLOW_RESIZE		 t ;paned_w constraint resource
		      :XMN_SKIP_ADJUST		 nil ;paned_w constraint resource
		      ))
	  (setq controlpanel_w
		(send XM_FORM_WIDGET_CLASS :new :managed
		      "controlpanel" paned_w
		      :XMN_FRACTION_BASE	30
		      ))
	  (setq editor_w
		(send *w_ctrlpnl:edit-widget-class* :new :managed
		      "edit" paned_w
		      self		;pass in instance of *w_ctrlpnl:class*
		      ))
	  (setq eval_button_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "eval_button" controlpanel_w
		      :XMN_LABEL_STRING		"Eval (<-->)\n< C-M-X >"
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	0
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	5
		      :XMN_TOP_ATTACHMENT	:attach_position
		      :XMN_TOP_POSITION		0
		      :XMN_BOTTOM_ATTACHMENT	:attach_position
		      :XMN_BOTTOM_POSITION	15
		      ))
	  (setq prev_button_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "prev_paren_button" controlpanel_w
		      :XMN_LABEL_STRING "( <---\n< C-M-B >"
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	5
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	10
		      :XMN_TOP_ATTACHMENT	:attach_position
		      :XMN_TOP_POSITION		0
		      :XMN_BOTTOM_ATTACHMENT	:attach_position
		      :XMN_BOTTOM_POSITION	15
		      ))
	  (setq next_button_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "next_paren_button" controlpanel_w
		      :XMN_LABEL_STRING "---> )\n< C-M-F > "
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	10
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	15
		      :XMN_TOP_ATTACHMENT	:attach_position
		      :XMN_TOP_POSITION		0
		      :XMN_BOTTOM_ATTACHMENT	:attach_position
		      :XMN_BOTTOM_POSITION	15
		      ))
	  (setq fmt_button_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "fmt_button" controlpanel_w
		      :XMN_LABEL_STRING "Format (<-->)\n< C-M-Q >"
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	15
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	20
		      :XMN_TOP_ATTACHMENT	:attach_position
		      :XMN_TOP_POSITION		0
		      :XMN_BOTTOM_ATTACHMENT	:attach_position
		      :XMN_BOTTOM_POSITION	15
		      ))
	  (setq editfile_button_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "edit_button" controlpanel_w
		      :XMN_LABEL_STRING		"Edit($EDITOR)\n < C-M-E >"
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	22
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	26
		      :XMN_TOP_ATTACHMENT	:attach_position
		      :XMN_TOP_POSITION		0
		      :XMN_BOTTOM_ATTACHMENT	:attach_position
		      :XMN_BOTTOM_POSITION	15
		      ))
	  (setq loadfile_button_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "load_file_button" controlpanel_w
		      :XMN_LABEL_STRING		"Load File\n< C-M-L >"
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	26
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	30
		      :XMN_TOP_ATTACHMENT	:attach_position
		      :XMN_TOP_POSITION		0
		      :XMN_BOTTOM_ATTACHMENT	:attach_position
		      :XMN_BOTTOM_POSITION	15
		      ))
	  (setq showfile_button_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "showfile_button_w" controlpanel_w
		      :XMN_LABEL_STRING		"Show File"
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	22
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	26
		      :XMN_TOP_ATTACHMENT	:attach_position
		      :XMN_TOP_POSITION		15
		      :XMN_BOTTOM_ATTACHMENT	:attach_position
		      :XMN_BOTTOM_POSITION	30
		      ))
	  (setq savefile_button_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "save_file_button" controlpanel_w
		      :XMN_LABEL_STRING		"Save File\n< C-M-S >"
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	26
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	30
		      :XMN_TOP_ATTACHMENT	:attach_position
		      :XMN_TOP_POSITION		15
		      :XMN_BOTTOM_ATTACHMENT	:attach_position
		      :XMN_BOTTOM_POSITION	30
		      ))
	  (let (fg bg)
	    ;; need to get color values from the manager in order to make the 
	    ;; XM_ROW_COLUMN_WIDGET_CLASS/:simple_radio_box have same colors
	    ;; as the gadgets in the controlpanel_w (puke).
	    (send controlpanel_w :get_values
		  :XMN_FOREGROUND 'fg
		  :XMN_BACKGROUND 'bg
		  )
	    (setq debug_options_w
		  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_radio_box
			"debug_options" controlpanel_w
			:XMN_PACKING		:pack_tight ;uses :pack_column by default, leaves too much spac
			:XMN_ORIENTATION	:horizontal
			:XMN_BUTTON_COUNT	4 ;create four buttons
			:XMN_BUTTON_SET		(if *breakenable* (if *tracenable* 3 2) (if *errhook* 1 0))
			:XMN_BUTTON_TYPE	#(:TOGGLEBUTTON :TOGGLEBUTTON :TOGGLEBUTTON :TOGGLEBUTTON)
			:XMN_BUTTONS 		(vector	;default converter doesn't use XM_STRING_CREATE_L_TO_R...
						 (xm_string_create_l_to_r "Debug Off\n(Terminal Out)")
						 (xm_string_create_l_to_r "Error Dialog\n(Window Output)")
						 (xm_string_create_l_to_r "Error Break-Loop\n(Terminal I/O)")
						 (xm_string_create_l_to_r "Error Trace-Back\n(Terminal I/O)")
						 )
			:XMN_FOREGROUND		fg ;make colors of radiobuttons same as other gadgets in controlpanel_w
			:XMN_BACKGROUND		bg ;make colors of radiobuttons same as other gadgets in controlpanel_w
			:XMN_LEFT_ATTACHMENT	:attach_position
			:XMN_LEFT_POSITION	0
			:XMN_RIGHT_ATTACHMENT	:attach_position
			:XMN_RIGHT_POSITION	22
			:XMN_TOP_ATTACHMENT	:attach_position
			:XMN_TOP_POSITION	15
			:XMN_BOTTOM_ATTACHMENT	:attach_position
			:XMN_BOTTOM_POSITION	30
			)))

	  (send-super :realize)

	  ;;
	  ;; set constraint resources on controlpanel so that paned window
	  ;; doesn't give it resize sashes.
	  ;;
	  (let (height)
	    (send controlpanel_w :get_values :xmn_height 'height)

	    ;; In the code below, the kludgery
	    ;; "(if *MOTIF-1.1-OR-LATER-P* ...)"
	    ;; is there to work around a name change between Motif 1.0 and 1.1:
	    ;; :XMN_MAXIMUM --> :XMN_PANE_MAXIMUM and :XMN_MINIMUM -->:XMN_PANE_MINIMUM
	    (send controlpanel_w :set_values
		  (if *MOTIF-1.1-OR-LATER-P* :XMN_PANE_MAXIMUM :XMN_MAXIMUM)
		  height
		  (if *MOTIF-1.1-OR-LATER-P* :XMN_PANE_MINIMUM :XMN_MINIMUM)
		  height
		  ))
	  (let (height)
	    (send editor_w :get_values :xmn_height 'height)
	    (send editor_w :set_values
		  (if *MOTIF-1.1-OR-LATER-P* :XMN_PANE_MAXIMUM :XMN_MAXIMUM)
		  height
		  (if *MOTIF-1.1-OR-LATER-P* :XMN_PANE_MINIMUM :XMN_MINIMUM)
		  height
		  ))

	  ;;
	  ;; Callbacks...
	  ;;
	  (send fsb_w :set-file-selected-callback-closure
		(lambda (selected_file_str)
		  (send self :local_edit_file selected_file_str)
		  ))
	  (send editfile_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		`(
		  (send ,self :edit_file)
		  ))
	  (send loadfile_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		`(			;Note: load_file is within "global" lexical scope
		  (send ,self :load_file)
		  ))
	  ;; Callback for "Show file"
	  (send showfile_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		`(
		  (send ,self :show_file)
		  ))
	  (send savefile_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		`(
		  (send ,self :save_file)
		  ))
	  (send eval_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		`(
		  (send ,editor_w :eval_defun)
		  ))
	  (send fmt_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		`(
		  (send ,editor_w :format_defun)
		  ))
	  (send prev_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		`(
		  (send ,editor_w :prev_paren)
		  ))
	  (send next_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		`(
		  (send ,editor_w :next_paren)
		  ))
	  (let ((errhook_backup NIL))
	    (send debug_options_w :add_callback :XMN_ENTRY_CALLBACK ;use this instead of XmNsimpleCallback
		  '(CALLBACK_ENTRY_WIDGET CALLBACK_ENTRY_SET)
		  '(
		    (if CALLBACK_ENTRY_SET
			(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
			      (0	;DEBUG OFF
			       (setq *breakenable* nil)
			       ;; (setq *tracenable* nil)
			       (if *errhook*
				   (setq errhook_backup *errhook*
					 *errhook*      nil))
			       )
			      (1	;ERROR DIALOG
			       (setq *breakenable* nil)
			       ;; (setq *tracenable* nil)
			       (if (null *errhook*)
				   ;; need to set *errhook* 
				   (if errhook_backup
				       ;; if it's in errhook_backup, use that
				       (setq *errhook* errhook_backup)
				     ;; else never loaded errhook, load it
				     (if (not (load "lib-utils/err-hook"))
					 (error "Couldn't load 'lib-utils/err-hook'"))
				     )
				 )
			       )
			      (2	;ERROR BREAK-LOOP
			       (if *errhook*
				   (setq errhook_backup *errhook*
					 *errhook*      nil))
			       (setq *breakenable* t)
			       (setq *tracenable*  nil)
			       )
			      (3	;ERROR TRACE-BACK
			       (if *errhook*
				   (setq errhook_backup *errhook*
					 *errhook*      nil))
			       (setq *breakenable* t)
			       (setq *tracenable*  t)
			       )
			      ))
		    ))
	    )
	  )
	))

;; Callback for "Edit File"
(send *w_ctrlpnl:class* :answer :LOCAL_EDIT_FILE '(selected_file_str)
      '(
	(winterp-show-busy-progn
	 (send editor_w :read_file selected_file_str)
	 )
	))

;; Callback for "Edit File"
(send *w_ctrlpnl:class* :answer :EDIT_FILE '()
      '(
	(system (concatenate 'string
			     (if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
			     " "
			     (send fsb_w :get-filepath-str)
			     " &"	;run it in the background so that winterp don't block...
			     ))
	))

;; Callback for "Load File"
(send *w_ctrlpnl:class* :answer :LOAD_FILE '()
      '(
	;;(progv '(*breakenable*) '(nil)
	(winterp_show_busy t)
	(unwind-protect
	    (progn
	      (load (send fsb_w :get-filepath-str)
		    :verbose t :print t)
	      (format T "; Done Loading\n\n")
	      )
	  ;; unwind always
	  (winterp_show_busy nil)
	  )
	;;)
	))

;; Callback for "Show File"
(send *w_ctrlpnl:class* :answer :SHOW_FILE '()
      '(
	(winterp-show-busy-progn
	 (send editor_w :read_file
	       (send fsb_w :get-filepath-str))
	 )
	))

;; Callback for "Save file"
(send *w_ctrlpnl:class* :answer :SAVE_FILE '()
      '(
	(let (dir-mask dir-spec save_fsb_w)
	  (send fsb_w :get_values
		:XMN_DIR_MASK	'dir-mask ;the "filter" name in fsb_w
		:XMN_DIR_SPEC	'dir-spec ;the "selection" file in fsb_w
		)
	  (setq save_fsb_w
		(send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :unmanaged :dialog
		      "save-in-file-dialog" self
		      :XMN_DIR_MASK		dir-mask ;set "filter" to whatever is in fsb_w
		      :XMN_AUTO_UNMANAGE	nil
		      :XMN_DIALOG_TITLE		"Save in file:"
		      :XMN_DIALOG_STYLE		:dialog_full_application_modal ;they've got to answer this dialog before doing anything else w/ WINTERP...
		      ))
	  ;; set "selection" to whatever is in fsb_w -- unfortunately, this doesn't take
	  ;; effect if set above during creation of save_fsb_w
	  (send save_fsb_w :set_values :XMN_DIR_SPEC dir-spec)
	  (send (send save_fsb_w :get_child :DIALOG_HELP_BUTTON) :unmanage)
		  
	  (send save_fsb_w :add_callback :XMN_OK_CALLBACK '(CALLBACK_VALUE)
		'(
		  (send editor_w :write_file (xm_string_get_l_to_r CALLBACK_VALUE))
		  (send save_fsb_w :destroy)
		  ))
	  (send save_fsb_w :add_callback :XMN_CANCEL_CALLBACK '()
		'(
		  (send save_fsb_w :destroy)
		  ))
	  (send save_fsb_w :manage)
	  )
	))

(send *w_ctrlpnl:class* :answer :SET-DEBUG-STATE '(state)
      '(
	(if state			;user wants "debug==breakloop" on
	    (if *tracenable*
		;; if tracenable set, then set "Error Trace-Back" toggle
		(send (aref (send debug_options_w :get_children) 3) :set_state t t)
	        ;; else tracenable not set, then set "Error Break-Loop" toggle
	        (send (aref (send debug_options_w :get_children) 2) :set_state t t)
		)
	    (if *errhook*
		;; if *errhook* was set, then set "Error Dialog" toggle
		(send (aref (send debug_options_w :get_children) 1) :set_state t t)
	        ;; else *errhook* not set, so set "Debug Off" toggle.
	        (send (aref (send debug_options_w :get_children) 0) :set_state t t)
		))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *WINTERP_CTRL_PNL*
      (send *w_ctrlpnl:class* :new
	    "winterpCtrlPnl"		;app-instance name
	    "WinterpCtrlPnl"		;app-class name
	    :XMN_TITLE			"WINTERP: Widget INTERPreter Control Panel"
	    :XMN_ICON_NAME		"W:w_ctrlpnl"
	    ))
