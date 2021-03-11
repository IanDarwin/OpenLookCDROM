;; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         droppocket.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/SGI/RCS/droppocket.lsp,v 2.1 1994/06/06 14:49:27 npm Exp $
; Description:  For SGI Irix 5.X only -- a demo of SG_DROP_POCKET_WIDGET_CLASS
;		showing the kinds of data transferred to the drop pocket widget
;		when icons are dragged from the IndigoMagic desktop into the
;		drop pocket widget.
; Author:       Niels P. Mayer
; Created:      Tue Dec 14 18:09:57 1993
; Modified:     Sun Jun  5 18:08:50 1994 (Niels Mayer) npm@indeed
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

(defconstant *category-string* "Category:")
(defconstant *name-string*     "Name:")
(defconstant *type-string*     "Type:")
(defconstant *host-string*     "Host:")
(defconstant *viewx-string*    "ViewX:")
(defconstant *viewy-string*    "ViewY:")
(defconstant *viewgallery-string* "ViewGallery:")

(let (toplevel_w form_w name_w category_w type_w host_w viewx_w viewy_w viewg_w
		 dp_w)

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "form1shl"
	    :XMN_GEOMETRY	"300x400"
	    :XMN_TITLE		"WINTERP: Drop Pocket Test"
	    :XMN_ICON_NAME	"W:DropPock"
	    ))

(setq form_w
      (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
	    "form" toplevel_w
	    ))

(let ((frm_w (send XM_FRAME_WIDGET_CLASS :new :managed "frm" form_w)))
  (send XM_LABEL_GADGET_CLASS :new :managed
	"label" frm_w
	:XMN_LABEL_TYPE			:string
	:XMN_LABEL_STRING		*name-string*
	:XMN_CHILD_TYPE			:frame_title_child ;XmFrame constraint resource
	:XMN_CHILD_VERTICAL_ALIGNMENT	:alignment_widget_bottom ;XmFrame constraint resource
	:XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	)
  (setq name_w
	(send XM_LABEL_GADGET_CLASS :new :managed
	      "field" frm_w
	      :XMN_LABEL_TYPE			:string
	      :XMN_LABEL_STRING			"< ?????? >"
	      :XMN_CHILD_TYPE			:frame_workarea_child ;XmFrame constraint resource
	      :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	      ))
  )
(let ((frm_w (send XM_FRAME_WIDGET_CLASS :new :managed "frm" form_w)))
  (send XM_LABEL_GADGET_CLASS :new :managed
	"label" frm_w
	:XMN_LABEL_TYPE			:string
	:XMN_LABEL_STRING		*category-string*
	:XMN_CHILD_TYPE			:frame_title_child ;XmFrame constraint resource
	:XMN_CHILD_VERTICAL_ALIGNMENT	:alignment_widget_bottom ;XmFrame constraint resource
	:XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	)
  (setq category_w
	(send XM_LABEL_GADGET_CLASS :new :managed
	      "field" frm_w
	      :XMN_LABEL_TYPE			:string
	      :XMN_LABEL_STRING			"< ?????? >"
	      :XMN_CHILD_TYPE			:frame_workarea_child ;XmFrame constraint resource
	      :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	      ))
  )
(let ((frm_w (send XM_FRAME_WIDGET_CLASS :new :managed "frm" form_w)))
  (send XM_LABEL_GADGET_CLASS :new :managed
	"label" frm_w
	:XMN_LABEL_TYPE			:string
	:XMN_LABEL_STRING		*type-string*
	:XMN_CHILD_TYPE			:frame_title_child ;XmFrame constraint resource
	:XMN_CHILD_VERTICAL_ALIGNMENT	:alignment_widget_bottom ;XmFrame constraint resource
	:XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	)
  (setq type_w
	(send XM_LABEL_GADGET_CLASS :new :managed
	      "field" frm_w
	      :XMN_LABEL_TYPE			:string
	      :XMN_LABEL_STRING			"< ?????? >"
	      :XMN_CHILD_TYPE			:frame_workarea_child ;XmFrame constraint resource
	      :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	      ))
  )
(let ((frm_w (send XM_FRAME_WIDGET_CLASS :new :managed "frm" form_w)))
  (send XM_LABEL_GADGET_CLASS :new :managed
	"label" frm_w
	:XMN_LABEL_TYPE			:string
	:XMN_LABEL_STRING		*host-string*
	:XMN_CHILD_TYPE			:frame_title_child ;XmFrame constraint resource
	:XMN_CHILD_VERTICAL_ALIGNMENT	:alignment_widget_bottom ;XmFrame constraint resource
	:XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	)
  (setq host_w
	(send XM_LABEL_GADGET_CLASS :new :managed
	      "field" frm_w
	      :XMN_LABEL_TYPE			:string
	      :XMN_LABEL_STRING			"< ?????? >"
	      :XMN_CHILD_TYPE			:frame_workarea_child ;XmFrame constraint resource
	      :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	      ))
  )
(let ((frm_w (send XM_FRAME_WIDGET_CLASS :new :managed "frm" form_w)))
  (send XM_LABEL_GADGET_CLASS :new :managed
	"label" frm_w
	:XMN_LABEL_TYPE			:string
	:XMN_LABEL_STRING		*viewx-string*
	:XMN_CHILD_TYPE			:frame_title_child ;XmFrame constraint resource
	:XMN_CHILD_VERTICAL_ALIGNMENT	:alignment_widget_bottom ;XmFrame constraint resource
	:XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	)
  (setq viewx_w
	(send XM_LABEL_GADGET_CLASS :new :managed
	      "field" frm_w
	      :XMN_LABEL_TYPE			:string
	      :XMN_LABEL_STRING			"< ?????? >"
	      :XMN_CHILD_TYPE			:frame_workarea_child ;XmFrame constraint resource
	      :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	      ))
  )
(let ((frm_w (send XM_FRAME_WIDGET_CLASS :new :managed "frm" form_w)))
  (send XM_LABEL_GADGET_CLASS :new :managed
	"label" frm_w
	:XMN_LABEL_TYPE			:string
	:XMN_LABEL_STRING		*viewy-string*
	:XMN_CHILD_TYPE			:frame_title_child ;XmFrame constraint resource
	:XMN_CHILD_VERTICAL_ALIGNMENT	:alignment_widget_bottom ;XmFrame constraint resource
	:XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	)
  (setq viewy_w
	(send XM_LABEL_GADGET_CLASS :new :managed
	      "field" frm_w
	      :XMN_LABEL_TYPE			:string
	      :XMN_LABEL_STRING			"< ?????? >"
	      :XMN_CHILD_TYPE			:frame_workarea_child ;XmFrame constraint resource
	      :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	      ))
  )
(let ((frm_w (send XM_FRAME_WIDGET_CLASS :new :managed "frm" form_w)))
  (send XM_LABEL_GADGET_CLASS :new :managed
	"label" frm_w
	:XMN_LABEL_TYPE			:string
	:XMN_LABEL_STRING		*viewgallery-string*
	:XMN_CHILD_TYPE			:frame_title_child ;XmFrame constraint resource
	:XMN_CHILD_VERTICAL_ALIGNMENT	:alignment_widget_bottom ;XmFrame constraint resource
	:XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	)
  (setq viewg_w
	(send XM_LABEL_GADGET_CLASS :new :managed
	      "field" frm_w
	      :XMN_LABEL_TYPE			:string
	      :XMN_LABEL_STRING			"< ?????? >"
	      :XMN_CHILD_TYPE			:frame_workarea_child ;XmFrame constraint resource
	      :XMN_CHILD_HORIZONTAL_ALIGNMENT	:alignment_center ;XmFrame constraint resource
	      ))
  )


;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

(setq dp_w
      (send SG_DROP_POCKET_WIDGET_CLASS :new :managed "droppocket" form_w
	    :XMN_HEIGHT 80
	    :XMN_WIDTH  80
	    ))

(send dp_w :set_callback :SGN_ICON_UPDATE_CALLBACK 
      '(CALLBACK_ICON_NAME CALLBACK_ICON_DATA)
      '(
	(format T "CALLBACK_ICON_NAME=~A\n"
		(xm_string_get_l_to_r CALLBACK_ICON_NAME)
		)

	;; category
	(send category_w :set_values :xmn_label_string
	      (subseq CALLBACK_ICON_DATA (+ (search *category-string* CALLBACK_ICON_DATA) (length *category-string*))
		      (1- (search *name-string* CALLBACK_ICON_DATA)))
	      )

	;; file name		  
	(send name_w :set_values :xmn_label_string
	      (subseq CALLBACK_ICON_DATA (+ (search *name-string* CALLBACK_ICON_DATA) (length *name-string*))
		      (1- (search *type-string* CALLBACK_ICON_DATA)))
	      )

	;; type
	(send type_w :set_values :xmn_label_string
	      (subseq CALLBACK_ICON_DATA (+ (search *type-string* CALLBACK_ICON_DATA) (length *type-string*))
		      (1- (search *host-string* CALLBACK_ICON_DATA)))
	      )

	;; host
	(send host_w :set_values :xmn_label_string
	      (subseq CALLBACK_ICON_DATA (+ (search *host-string* CALLBACK_ICON_DATA) (length *host-string*))
		      (1- (search *viewx-string* CALLBACK_ICON_DATA)))
	      )

	;; view-x
	(send viewx_w :set_values :xmn_label_string
	      (subseq CALLBACK_ICON_DATA (+ (search *viewx-string* CALLBACK_ICON_DATA) (length *viewx-string*))
		      (1- (search *viewy-string* CALLBACK_ICON_DATA)))
	      )

	;; view-y
	(send viewy_w :set_values :xmn_label_string
	      (subseq CALLBACK_ICON_DATA (+ (search *viewy-string* CALLBACK_ICON_DATA) (length *viewy-string*))
		      (1- (search *viewgallery-string* CALLBACK_ICON_DATA)))
	      )

	;; viewgallery
	(send viewg_w :set_values :xmn_label_string
	      (subseq CALLBACK_ICON_DATA (+ (search *viewgallery-string* CALLBACK_ICON_DATA) (length *viewgallery-string*)))
	      )
	))

(send toplevel_w :realize)
)
