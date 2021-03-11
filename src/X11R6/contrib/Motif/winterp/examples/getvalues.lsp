;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         getvalues.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/getvalues.lsp,v 2.8 1994/06/06 14:43:16 npm Exp $
; Description:  This file should is a random test to see whether the code
;               in winterp/src-server/w_resources.c has any machine dependencies.
;               Load this file, and if your stdout beeps and you see messages
;               about "failed: ..." then please send the output to mayer@netcom.com
;		NOTE: the actual graphical result of loading this file is not
;		pretty. In fact, it's not supposed to be pretty....
; Author:       Niels Mayer
; Created:      Tue Jul 31 19:50:20 1990
; Modified:     Sun Jun  5 18:41:57 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*, *MOTIF-1.0-P*

(progn
  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "Winterp: Set/GetValues Test"
	      ))
  (setq scr_w
	(send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed top_w
	      :XMN_SCROLLING_POLICY :AUTOMATIC
	      ))
  (setq rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" scr_w
	      :XMN_ORIENTATION :VERTICAL
	      :XMN_PACKING :PACK_COLUMN
	      :XMN_NUM_COLUMNS 2
	      :XMN_ENTRY_ALIGNMENT :ALIGNMENT_CENTER
	      ))

  (setq te_w (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled rc_w
		   :XMN_EDIT_MODE :MULTI_LINE_EDIT
		   ))
  (setq pb_w (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "pb" rc_w))
  (setq to_w (send XM_TOGGLE_BUTTON_WIDGET_CLASS :new :managed "tog" rc_w))
  (setq pa_w (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed "paned" rc_w))
  (setq te1_w (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled pa_w
		    :XMN_EDIT_MODE :MULTI_LINE_EDIT
		    ))
  (setq te2_w (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled pa_w
		    :XMN_EDIT_MODE :MULTI_LINE_EDIT
		    ))
  (setq te3_w (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled pa_w
		    :XMN_EDIT_MODE :MULTI_LINE_EDIT
		    ))
  (setq te4_w (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled pa_w
		    :XMN_EDIT_MODE :MULTI_LINE_EDIT
		    ))
  (setq la_w (send XM_LABEL_WIDGET_CLASS :new :managed "label" rc_w))

  (setq li_w
	(send XM_LIST_WIDGET_CLASS :new :managed :scrolled rc_w
	      :xmn_selection_policy :browse_select
	      :xmn_items #("foo" "bar" "baz" "bof" "boof" "frob" "snob" "blog")
	      :xmn_item_count 8
	      :xmn_visible_item_count 20
	      ))

  (send top_w :realize)
  )

(defun test (w r v)
  (let (result)
    (send w :set_values r v)
    (send w :get_values r 'result)
    (cond
     ((equal v result)
      (format T "success: widget=~A resource=~A value=~A\n" w r v))
     (t
      (X_BELL)
      (format T "failed: widget=~A resource=~A value=~A result=~A\n" w r v result))
     )))


(test pb_w :XMN_BORDER_WIDTH 6)		;XmRDimension
(test pb_w :XMN_HIGHLIGHT_THICKNESS 10)	;XmRShort
(if *MOTIF-1.0-P*
    (test pb_w :XMN_SHOW_AS_DEFAULT 5)	;Motif 1.0 -- XmRShort
  (test pb_w :XMN_SHOW_AS_DEFAULT T))	;Motif >= 1.1 -- XmRBooleanDimension

(test pb_w :XMN_SHADOW_THICKNESS 6)	;XmRShort

(test pa_w :XMN_SASH_INDENT -20)	;XmRPosition
(test pa_w :XMN_SASH_INDENT 2)          ;XmRPosition
(test pa_w :XMN_SASH_INDENT 1)          ;XmRPosition
(test pa_w :XMN_SASH_INDENT 0)          ;XmRPosition
(test pa_w :XMN_SASH_INDENT -1)         ;XmRPosition
(test pa_w :XMN_SASH_INDENT -2)         ;XmRPosition
(test pa_w :XMN_SASH_SHADOW_THICKNESS 0) ;XmRint
(test pa_w :XMN_SASH_SHADOW_THICKNESS 1) ;XmRint
(test pa_w :XMN_SASH_SHADOW_THICKNESS 2) ;XmRint

(test rc_w :XMN_NUM_COLUMNS 1)		;XmRShort

(if *MOTIF-1.1-OR-LATER-P*		;don't do this test for Motif 1.0
    (test to_w :XMN_SPACING 100)	;buggy in 1.0 due to misdeclared resource  names in Motif 1.0 (see comments in w_resources.c)
  )
(test rc_w :XMN_SPACING 10)
(test rc_w :XMN_SPACING 10)
(test pa_w :XMN_SPACING 50)

(if *MOTIF-1.0-P*
    (test rc_w :xmn_rcmargin_width 21)	;Motif 1.0 workaround
  (test rc_w :xmn_margin_width 10)	;works in Motif >= 1.1, buggy in 1.0
  )

(test la_w :xmn_margin_width 22)

(if *MOTIF-1.0-P*
    (test rc_w :xmn_rcmargin_height 21)	;Motif 1.0 workaround
  (test rc_w :xmn_margin_height 20)	;works in Motif >= 1.1, buggy in 1.0
  )
(test la_w :xmn_margin_height 22)

(test rc_w :XMN_ORIENTATION :vertical)

(test rc_w :XMN_ENTRY_ALIGNMENT :alignment_beginning) ;unsigned char
(test rc_w :XMN_ENTRY_ALIGNMENT :alignment_center)
(test rc_w :XMN_ENTRY_ALIGNMENT :alignment_end)
(test rc_w :XMN_PACKING :pack_tight)
(test rc_w :XMN_PACKING :pack_column)
(test rc_w :XMN_PACKING :pack_none)

(test li_w :XMN_SELECTION_POLICY :multiple_select)
(test li_w :XMN_SELECTION_POLICY :single_select)
(test li_w :XMN_SELECTION_POLICY :browse_select)
(test li_w :XMN_SELECTION_POLICY :extended_select)

(test top_w :xmn_keyboard_focus_policy :explicit)
(test top_w :xmn_keyboard_focus_policy :pointer)

;; XmRBoolean
(test top_w :XMN_OVERRIDE_REDIRECT t)
(test top_w :XMN_OVERRIDE_REDIRECT nil)
(test top_w :XMN_WAIT_FOR_WM nil)
(test top_w :XMN_WAIT_FOR_WM t)
(test top_w :XMN_TRANSIENT t)
(test top_w :XMN_TRANSIENT nil)

;; XmRChar
(dotimes (i 255)
	 (if (/= i 7)			;\007==BEL, we don't want it to beep the terminal since errors are only supposed to do that.
	     (test la_w :XMN_MNEMONIC (int-char i))
	   ))

;;; (dotimes (i 32767)
;;; 	 (test te_w :XMN_MAX_LENGTH i)
;;; 	 )

(test te_w :XMN_MAX_LENGTH 32767)
(test te_w :XMN_MAX_LENGTH -32768)
(test te_w :XMN_MAX_LENGTH -1)
(test te_w :XMN_MAX_LENGTH 0)
(test te_w :XMN_MAX_LENGTH 1)
(test te_w :XMN_MAX_LENGTH 32767)


;; (test te_w :XMN_OVERRIDE_REDIRECT t)
;; (test te_w :XMN_OVERRIDE_REDIRECT nil)
;; (test te_w :XMN_WAIT_FOR_WM nil)
;; (test te_w :XMN_WAIT_FOR_WM t)
;; (test te_w :XMN_TRANSIENT t)
;; (test te_w :XMN_TRANSIENT nil)
