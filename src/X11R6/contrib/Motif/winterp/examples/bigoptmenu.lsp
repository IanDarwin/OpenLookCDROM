; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         bigoptmenu.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/bigoptmenu.lsp,v 2.1 1994/06/06 14:43:22 npm Exp $
; Description:  Example code to create an option menu with lots of items on it;
;		Normally, option menus would create a single-column layout of
;		the option-buttons. This one uses a 10x10 rectangular array...
;		Load this file to see the example.
; Author:       Niels P. Mayer
; Created:      Sun Jun  6 14:25:33 1993
; Modified:     Sun Jun  5 18:16:28 1994 (Niels Mayer) npm@indeed
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

;;; Big option menu hack described by David Smyth, JPL.
;;;
;;; Mri.wcPopups:           doMenu
;;; Mri.wcChildren:         doOption
;;; 
;;; *doMenu.wcCreate:       XmCreatePulldownMenu
;;; *doMenu.numColumns:     10
;;; *doMenu.packing:        XmPACK_COLUMN
;;; *doMenu.wcChildren:     0 1 2 3 4 5 6 7 8 9 \
;;;                         10 11 12 13 14 15 16 17 18 19 \
;;;                         20 21 22 23 24 25 26 27 28 29 \
;;;                         30 31 32 33 34 35 36 37 38 39 \
;;;                         40 41 42 43 44 45 46 47 48 49 \
;;;                         50 51 52 53 54 55 56 57 58 59 \
;;;                         60 61 62 63 64 65 66 67 68 69 \
;;;                         70 71 72 73 74 75 76 77 78 79 \
;;;                         80 81 82 83 84 85 86 87 88 89 \
;;;                         90 91 92 93 94 95 96 97 98 99
;;; 
;;; *doMenu*wcCreate:               XmPushButton
;;; 
;;; *doOption.wcCreate:             XmCreateOptionMenu
;;; *doOption.labelString:          Do It
;;; *doOption.subMenuId:            *doMenu

(let (top_w doMenu_w btn-array)
  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "somshl"
	      :XMN_TITLE	"WINTERP: Multi-Column Option Menu"
	      :XMN_ICON_NAME	"W:bigoptmenu"
	      ))

  ;; NOTE: create pulldown menu "unmanaged" to avoid warning:
  ;;	Name:  popup_doMenu
  ;;	Class: XmMenuShell
  ;;	Attempting to manage an incomplete menu.
  (setq doMenu_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :pulldown_menu
	      "doMenu" top_w
	      :XMN_PACKING	:pack_column
	      :XMN_NUM_COLUMNS	10
	      :XMN_ORIENTATION	:horizontal
	      ))
  (send doMenu_w :add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
	'(CALLBACK_ENTRY_WIDGET)
	'(
	  (format T "Selected opion ~A\n" (send CALLBACK_ENTRY_WIDGET :name))
	  ))

  ;; create 100 push-button gadgets inside the pulldown menu
  ;; NOTE: it is FAR more efficient to create unmanaged and manage
  ;; later w/ XT_MANAGE_CHILDREN
  (setq btn-array (make-array 100))
  (dotimes (i (length btn-array))
	   (setf (aref btn-array i)
		 (send XM_PUSH_BUTTON_GADGET_CLASS :new
		       (format nil "~2@A" i) doMenu_w)
		 ))

  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :option_menu
	"doOption" top_w
	:XMN_LABEL_STRING	"Do It"
	:XMN_SUB_MENU_ID	doMenu_w
	)

  (xt_manage_children btn-array)
  (send top_w :realize)
  )



