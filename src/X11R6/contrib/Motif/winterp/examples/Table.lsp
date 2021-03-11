;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         Table.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/Table.lsp,v 2.4 1994/06/06 14:43:24 npm Exp $
; Description:  demo of TABLE_WIDGET_CLASS
; Author:       Niels Mayer
; Created:      Mon Dec 23 22:51:37 1991
; Modified:     Sun Jun  5 18:14:02 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definite Widget Abuse -- create an array of 50 widgets and use table widget to
;; plot the widgets into a sine wave...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let (top_w tab_w w_array)
  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "tabshl"
	      :XMN_TITLE	"WINTERP: Table Demo"
	      :XMN_ICON_NAME	"W:Table"
	      ))

  (setq tab_w
	(send TABLE_WIDGET_CLASS :new :unmanaged "table" top_w
	      ))
	    
  (setq w_array (make-array 50))

  (dotimes
   (i (length w_array))
   (setf (aref w_array i)
	 (send XM_PUSH_BUTTON_WIDGET_CLASS :new :unmanaged 
	       (format nil "~A" i)
	       tab_w
	       ))
   )

  (let* ((2pi (* 3.1415 2))
	 (le (length w_array))
	 )

    ;; (XT_TBL_CONFIG <widget> <col> <row> <h_span> <v_span> <opt>)
    (dotimes (i le)
	     (XT_TBL_CONFIG (aref w_array i)
			    i
			    (truncate (* (+ (sin (/ (* 2pi i) le)) 1) le))
			    1 1 :TBL_SM_WIDTH :TBL_SM_HEIGHT
			    )
	     ))

  (xt_manage_children w_array)
  (send tab_w :manage)
  (send top_w :realize)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; widget weirdness...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn

(setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "tabshl"
	      :XMN_GEOMETRY	"300x300+1+1"
	      :XMN_TITLE	"WINTERP: Table Demo"
	      :XMN_ICON_NAME	"W:Table"
	      ))

(send top_w :realize)

(setq tab_w
      (send TABLE_WIDGET_CLASS :new :managed "table" top_w
	    ))

(setq w_array (make-array 20))

(dotimes
   (i (length w_array))
   (setf (aref w_array i)
	 (send XM_TEXT_WIDGET_CLASS :new :unmanaged 
	       (format nil "text-~A" i)
	       tab_w
;;	       :XMN_RESIZE_WIDTH t
;;	       :XMN_RESIZE_HEIGHT t
	       :xmn_rows 5
	       :xmn_columns 5
	       ))
   )

;; (XT_TBL_CONFIG <widget> <col> <row> <h_span> <v_span> <opt>)
(progn
(xt_unmanage_children w_array)
(XT_TBL_CONFIG (aref w_array 0)  0  0  1  1 :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 1)  1  1  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 2)  2  2  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 3)  0  3  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 4)  1  4  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 5)  2  5  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 6)  0  6  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 7)  1  7  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 8)  2  8  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 9)  0  9  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 10) 1 10  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 11) 2 11  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 12) 0 12  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 13) 1 13  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 14) 2 14  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 15) 0 15  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 16) 1 16  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 17) 2 17  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 18) 0 18  1  1  :TBL_LK_HEIGHT)
(XT_TBL_CONFIG (aref w_array 19) 1 19  1  1  :TBL_LK_HEIGHT)
(xt_manage_children w_array)
)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A "form" consisting of 10 labels and  10 edit widgets. Note that the
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
(setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "tabshl"
	      :XMN_GEOMETRY	"300x300+1+1"
	      :XMN_TITLE	"WINTERP: Table Demo"
	      :XMN_ICON_NAME	"W:Table"
	      ))

(setq tab_w
      (send TABLE_WIDGET_CLASS :new :unmanaged "table" top_w
	    :XMN_LAYOUT "label0 0 0 0 0; edit0 0 1 1 1; label1 0 2 1 1; edit1 1 3 1 1; label2 1 4 1 1; edit2 1 5 1 1; label3 1 6 1 1; edit3 1 7 1 1; label4 1 8 1 1; edit4 9 1 2 1; label5 10 1 2 1; edit5 1 11 1 1; label6 1 12 1 1; edit6 4 13 1 1; label7 4 14 1 1; edit7 5 15 1 1; label8 7 16 1 1; edit8 8 17 1 1;label9 1 18 1 1; edit9 2 18 1 1"
;;; BAD -- "HW" SEEMS TO CAUSE "oscillation" inf loops when typing into textw
;;;	    :XMN_DEFAULT_OPTIONS "HW"
;;; BAD -- "hw" SEEMS TO CAUSE "oscillation" inf loops when typing into textw
;;;	    :XMN_DEFAULT_OPTIONS "hw"
	    :XMN_DEFAULT_OPTIONS "lt"
	    ))

(setq label_array (make-array 10))
(setq textf_array (make-array 10))

(dotimes
   (i (length label_array))
   (setf (aref label_array i)
	 (send XM_LABEL_WIDGET_CLASS :new :unmanaged 
	       (format nil "label~A" i)
	       tab_w
	       :XMN_LABEL_STRING (format nil "this is label ~A" i)
	       ))
   )

(dotimes
 (i (length textf_array))
 (setf (aref textf_array i)
       (send XM_TEXT_WIDGET_CLASS :new :unmanaged 
	     (format nil "edit~A" i)
	     tab_w
	     :XMN_RESIZE_WIDTH nil
	     :XMN_RESIZE_HEIGHT nil
	     :XMN_EDIT_MODE :multi_line_edit
	     :XMN_CURSOR_POSITION_VISIBLE t
	     ))
 )

(defun set-label-at-idx (idx widget)
  (send (aref label_array idx) :set_values
	:xmn_label_string (send widget :get_string))
  )

(dotimes
 (i (length textf_array))

 (send (aref textf_array i) :override_translations
       (format nil "<Key>Return: Lisp(set-label-at-idx ~A ACTION_WIDGET)" i))

 ;;  (send (aref textf_array i) :set_callback :xmn_activate_callback '(callback_widget)
 ;;        `(
 ;; 	 (send (aref label_array ,i) :set_values
 ;; 	       :xmn_label_string (send callback_widget :get_string))
 ;; 	 )
 ;;        )
 )

(xt_manage_children label_array)
(xt_manage_children textf_array)
(send tab_w :manage)
(send top_w :realize)

)


(progn
(setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "tabshl"
	      :XMN_GEOMETRY	"300x300+1+1"
	      :XMN_TITLE	"WINTERP: Table Demo"
	      :XMN_ICON_NAME	"W:Table"
	      ))

(setq tab_w
      (send TABLE_WIDGET_CLASS :new :unmanaged "table" top_w
	    :XMN_DEFAULT_OPTIONS "lt"
	    ))

(setq label_array (make-array 10))
(setq textf_array (make-array 10))

(dotimes
   (i (length label_array))
   (setf (aref label_array i)
	 (send XM_LABEL_WIDGET_CLASS :new :unmanaged 
	       (format nil "label~A" i)
	       tab_w
	       :XMN_LABEL_STRING (format nil "this is label ~A" i)
	       :XMN_DEFAULT_OPTIONS "lt"
	       ))
   )

(dotimes
 (i (length textf_array))
 (setf (aref textf_array i)
       (send XM_TEXT_WIDGET_CLASS :new :unmanaged 
	     (format nil "edit~A" i)
	     tab_w
	     :XMN_RESIZE_WIDTH nil
	     :XMN_RESIZE_HEIGHT nil
	     :XMN_EDIT_MODE :multi_line_edit
	     :XMN_CURSOR_POSITION_VISIBLE t
	     ))
 )

(defun set-label-at-idx (idx widget)
  (send (aref label_array idx) :set_values
	:xmn_label_string (send widget :get_string))
  )

(dotimes
 (i (length textf_array))

 (send (aref textf_array i) :override_translations
       (format nil "<Key>Return: Lisp(set-label-at-idx ~A ACTION_WIDGET)" i))

 ;;  (send (aref textf_array i) :set_callback :xmn_activate_callback '(callback_widget)
 ;;        `(
 ;; 	 (send (aref label_array ,i) :set_values
 ;; 	       :xmn_label_string (send callback_widget :get_string))
 ;; 	 )
 ;;        )
 )

(xt_manage_children label_array)
(xt_manage_children textf_array)
(send tab_w :manage)
(send top_w :realize)

;; (XT_TBL_CONFIG <widget> <col> <row> <h_span> <v_span> <opt>)
(progn
(xt_unmanage_children label_array)
(xt_unmanage_children textf_array)
(XT_TBL_CONFIG (aref label_array 0)  0  0  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 1)  0  1  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 2)  0  2  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 3)  0  3  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 4)  0  4  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 5)  0  5  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 6)  0  6  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 7)  0  7  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 8)  0  8  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 9)  0  9  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 0)  1  0  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 1)  1  1  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 2)  1  2  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 3)  1  3  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 4)  1  4  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 5)  1  5  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 6)  1  6  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 7)  1  7  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 8)  1  8  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 9)  1  9  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(xt_manage_children label_array)
(xt_manage_children textf_array)
)

)

(progn
(xt_unmanage_children label_array)
(xt_unmanage_children textf_array)
(XT_TBL_CONFIG (aref label_array 0)  0  0  2  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 1)  0  1  2  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 2)  0  2  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 3)  0  3  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 4)  0  4  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 5)  0  5  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 6)  0  6  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 7)  0  7  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 8)  0  8  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref label_array 9)  0  9  1  1 :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 0)  1  0  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 1)  1  1  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 2)  1  2  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 3)  1  3  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 4)  1  4  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 5)  3  5  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 6)  12  6  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 7)  7  7  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 8)  8  8  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(XT_TBL_CONFIG (aref textf_array 9)  9  9  1  1 :TBL_SM_WIDTH :TBL_SM_HEIGHT)
(xt_manage_children label_array)
(xt_manage_children textf_array)
)

;;
;; :SET_VALUES of XMN_LAYOUT doesn't work -- obviously a "create time only"
;; resource...
;;
; (progn
;   (xt_unmanage_children label_array)
;   (xt_unmanage_children textf_array)
;   (send tab_w :set_values
; 	:XMN_LAYOUT
; 	"label0 0 0 0 0; edit0 0 1 1 1; label1 0 2 1 1; edit1 1 3 1 1; label2 1 4 1 1; edit2 1 5 1 1; label3 1 6 1 1; edit3 1 7 1 1; label4 1 8 1 1; edit4 9 1 2 1; label5 10 1 2 1; edit5 1 11 1 1; label6 1 12 1 1; edit6 4 13 1 1; label7 4 14 1 1; edit7 5 15 1 1; label8 7 16 1 1; edit8 8 17 1 1;label9 1 18 1 1; edit9 2 18 1 1"
; 	)
;   (xt_manage_children label_array)
;   (xt_manage_children textf_array)
;   )

