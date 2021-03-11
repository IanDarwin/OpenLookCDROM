; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xmu-menu.lsp
; RCS:          $Header: $
; Description:  Xmu WINTERP-based menu server package
; Author:       Richard Hess, Consilium
; Created:      Sun Oct  6 00:06:05 1991
; Modified:     Sun Oct  6 00:07:07 1991 (Niels Mayer) mayer@hplnpm
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
; documentation, and that the name of Hewlett-Packard and David Betz not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and David Betz
; make no representations about the suitability of this software for any
; purpose. It is provided "as is" without express or implied warranty.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; +---------------------------------------------------------------------------
;  WHO:    Richard Hess                    CORP:   Consilium
;  TITLE:  Staff Engineer                  VOICE:  [415] 691-6342
;      [ X-SWAT Team:  Special Projects ]  USNAIL: 640 Clyde Court
;  UUCP:   ...!uunet!cimshop!rhess                 Mountain View, CA 94043
; +---------------------------------------------------------------------------

(defvar *xmu_timer*     nil )		;; the active <timeout_obj>...
(defvar *xmu_assoc*     nil )		;; the menu-cache...
(defvar *xmu_pick*      nil )		;; the last menu pick... [ string ]
(defvar *xmu_xtra*      32  )		;; the max size of a menu... ****
(defvar *xmu_lock*      nil )		;; the menu server "LOCK"...
(defvar *xmu_callback*  nil )		;; the active menu callback...
(defvar *xmu_color*    "red")		;; the "null" menu background color...

(defun Xmu_Popup (key &optional xloc yloc cbk)
  "[ Xmu ]:  popup a menu from the menu-cache..."
  (setq *xmu_callback*  cbk)
  (if *xmu_lock*
      (progn (format T "ERROR:  menu server currently active...~%")
	     (Xmu_Error ":NoLock"))
    (progn (setq *xmu_lock* t)
	   (let* ((lookup (assoc key *xmu_assoc*))
		  (menu (if lookup
			    (cadr lookup)
			  nil))
		  )
	     (if menu
		 (progn (if (and xloc yloc)
			    (send menu :SET_VALUES
				  :XMN_X    xloc
				  :XMN_Y    yloc)
			  (let ((xypos (get_mouse_location)))
			    (send menu :SET_VALUES
				  :XMN_X    (car xypos)
				  :XMN_Y    (cdr xypos))))
			(send menu :manage))
	       (progn (format T "ERROR:  invalid menu... [~S] ~%" key)
		      (Xmu_Error ":NoMenu")))
	     ))
    ))

(defun Xmu_Menu (key heading entries &optional note)
  "[ Xmu ]:  create a menu entry in the menu-cache..."
  (let* ((xmu (send XM_ROW_COLUMN_WIDGET_CLASS :new :popup_menu
		    *TOPLEVEL_WIDGET*
		    ))
	 mlist
	 )
    (send xmu
	  :add_callback :XMN_UNMAP_CALLBACK
	  '()
	  '((setq *xmu_timer*
		  (XT_ADD_TIMEOUT 200 '((Xmu_Error ":NoPick")))))
	  )
    (if heading
	(progn
	  (setq sbtn (send XM_LABEL_GADGET_CLASS :new :managed 
			   "menuLabel" xmu
			   :XMN_LABEL_STRING heading
			   ))
	  (send XM_SEPARATOR_GADGET_CLASS :new :managed "menuLine" xmu))
	  )
    (Xmu_Menu_Items xmu entries)
    (setq mlist (assoc key *xmu_assoc*))
    (if mlist
	(progn (format T "WARNING:  deleting existing menu... [~S] ~%" key)
	       (send (cadr mlist) :destroy)
	       (setq *xmu_assoc* (delete mlist *xmu_assoc*))
	       ))
    (setq *xmu_assoc* (append (cons (cons key
					 (cons xmu
					       (cons note nil)))
				    nil)
			      *xmu_assoc*))
    ))

(defun Xmu_Submenu (menu label entries)
  "[ Xmu ]:  create a submenu entry for this menu..."
  (let* ((xmu  (send XM_ROW_COLUMN_WIDGET_CLASS :new :pulldown_menu menu
		     ))
	 (xbtn (send XM_CASCADE_BUTTON_GADGET_CLASS :new :managed
		     "menuSub" menu
		     :XMN_LABEL_STRING label
		     :XMN_SUB_MENU_ID  xmu
		     ))
	 )
    (Xmu_menu_items xmu entries)
    ))

(defun Xmu_Menu_Items (menu entries)
  "[ Xmu ]:  process the menu's entry list..."
  (let ((nn 0)
	(xmu menu)
	xtra
	xbtn
	)
    (dolist (elt entries)
	    (if (>= nn *xmu_xtra*)
		(progn (setq xtra (send XM_ROW_COLUMN_WIDGET_CLASS :new 
					:pulldown_menu xmu
					))
		       (setq xbtn (send XM_CASCADE_BUTTON_WIDGET_CLASS
					:new :managed "menuXtra" xmu
					:XMN_SUB_MENU_ID  xtra
					))
		       (setq xmu xtra)
		       (setq nn 1)
		       )
	      (setq nn (+ nn 1))
	      )
	    (if (and elt
		     (listp elt))
		(let ((label (car elt))
		      (tag   (cadr elt))
		      )
		  (if (and label
			   (listp tag))
		      (Xmu_Submenu xmu label tag)
		    (Xmu_String_Items xmu label tag)))
	      (Xmu_String_Items xmu elt nil)
	      ))
    (if (and *xmu_color*
	     (eq nn 0))
	(send xmu :SET_VALUES :XMN_BACKGROUND *xmu_color*))
    ))

(defun Xmu_String_Items (menu label tag)
  "[ Xmu ]:  create menu entries... [ button ][ seperator ]"
  (if label
      (let ((xbtn (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
			"menuButton" menu
			:XMN_LABEL_STRING label
			))
	    (xtag label)
	    )
	(if tag (setq xtag tag))
	(send xbtn
	      :add_callback :XMN_ACTIVATE_CALLBACK
	      '() '((Xmu_Pick xtag)))
	)
    (send XM_SEPARATOR_GADGET_CLASS :new :managed "menuLine" menu)
    ))

(defun Xmu_Pick (tag)
  "[ Xmu ]:  return the menu pick..."
  (XT_REMOVE_TIMEOUT *xmu_timer*)
  (if *xmu_callback*
      (eval (list *xmu_callback* tag)))
  (setq *xmu_timer*     nil)
  (setq *xmu_pick*     (format nil "~A" tag))
  (setq *xmu_callback*  nil)
  (setq *xmu_lock*      nil)
  )

(defun Xmu_Error (tag)
  "[ Xmu ]:  return the menu error..."
  (if *xmu_callback*
      (eval (list *xmu_callback* tag)))
  (setq *xmu_timer*     nil)
  (setq *xmu_pick*      "")
  (setq *xmu_callback*  nil)
  (setq *xmu_lock*      nil)
  )

; -----------------------------------------------------------------------<eof>
