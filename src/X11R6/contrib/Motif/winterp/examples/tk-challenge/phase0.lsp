; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         phase0.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/tk-challenge/RCS/phase0.lsp,v 2.4 1994/06/06 14:58:04 npm Exp $
; Description:  Phase 0 of toolkit challenge
; Author:       Niels P. Mayer
; Created:      Thu Apr  2 1992
; Modified:     Mon Jun  6 01:20:59 1994 (Niels Mayer) npm@indeed
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 35 mins. (basic layout took about 15 minutes. The rest was
;; needed to make David Harrison's table widget layout work correctly
;; when the window was resized dynamically.) 

(require "lib-utils/initialize")	;define STRCAT, etc.
(require "lib-utils/motif-vers")	;define MOTIF-1.1-OR-LATER-P, MOTIF-1.1.3-OR-LATER-P
(require "lib-utils/unixstuf")		;define WINTERP-STANDALONE-P

(let                                    ;declare some local variables
    (top-w frame+help-form-w frame-w table+bbox-form-w table-w button-box-w)
           
  (setq top-w
        (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "app-0"
              :XMN_TITLE		"WINTERP: Toolkit Challenge Application (phase 0)"
              :XMN_ICON_NAME		"W:phase0"
              ))
  (setq frame+help-form-w
        (send XM_FORM_WIDGET_CLASS :new :managed
              "frame+help-form" top-w
              ))
  (setq frame-w
        (send XM_FRAME_WIDGET_CLASS :new :managed
              "frame" frame+help-form-w
              :XMN_TOP_ATTACHMENT       :attach_form
              :XMN_LEFT_ATTACHMENT      :attach_form
              :XMN_RIGHT_ATTACHMENT     :attach_form
              ))
  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
        "help-btn" frame+help-form-w
        :XMN_LABEL_STRING "Help"
        :XMN_TOP_ATTACHMENT             :attach_widget
        :XMN_TOP_WIDGET                 frame-w
        :XMN_RIGHT_ATTACHMENT           :attach_form
        :XMN_BOTTOM_ATTACHMENT          :attach_form
        )
  (setq table+bbox-form-w
        (send XM_FORM_WIDGET_CLASS :new :managed
              "table+bbox-form" frame-w
              ))
  (setq table-w
        (send TABLE_WIDGET_CLASS :new :managed
              "table" table+bbox-form-w
              :XMN_TOP_ATTACHMENT       :attach_form
              :XMN_LEFT_ATTACHMENT      :attach_form
              :XMN_RIGHT_ATTACHMENT     :attach_form
              :XMN_LAYOUT "name-label    0 0 1 1 rWH;\
                           addrs-0-label 0 1 1 1 rWH;\
                           addrs-1-label 0 2 1 1 rWH;\
                           addrs-2-label 0 3 1 1 rWH;\
                           hophone-label 0 4 1 1 rWH;\
                           wophone-label 0 5 1 1 rWH;\
                           fax-label     0 6 1 1 rWH;\
                           name-field    1 0 1 1 h;\
                           addrs-0-field 1 1 1 1 h;\
                           addrs-1-field 1 2 1 1 h;\
                           addrs-2-field 1 3 1 1 h;\
                           hophone-field 1 4 1 1 h;\
                           wophone-field 1 5 1 1 h;\
                           fax-field     1 6 1 1 h;"
              ))
  (setq button-box-w 
        (send TABLE_WIDGET_CLASS :new :managed
              "button-box" table+bbox-form-w
              :XMN_TOP_ATTACHMENT       :attach_widget
              :XMN_TOP_WIDGET           table-w
              :XMN_LEFT_ATTACHMENT      :attach_form
              :XMN_RIGHT_ATTACHMENT     :attach_form
              :XMN_LAYOUT "clear-btn    0 0 1 1 WH;\
                           add-btn      1 0 1 1 WH;\
                           search-btn   2 0 1 1 WH;\
                           delete-btn   3 0 1 1 WH;"
              ))
  (send XM_LABEL_GADGET_CLASS :new :managed
        "name-label" table-w
        :XMN_ALIGNMENT          :alignment_end
        :XMN_LABEL_STRING       "Name:"
        )
  (send XM_LABEL_GADGET_CLASS :new :managed
        "addrs-0-label" table-w
        :XMN_ALIGNMENT          :alignment_end
        :XMN_LABEL_STRING       "Address:"
        )
  (send XM_LABEL_GADGET_CLASS :new :managed
        "addrs-1-label" table-w
        :XMN_ALIGNMENT          :alignment_end
        :XMN_LABEL_STRING       ""
        )
  (send XM_LABEL_GADGET_CLASS :new :managed
        "addrs-2-label" table-w
        :XMN_ALIGNMENT          :alignment_end
        :XMN_LABEL_STRING       ""
        )
  (send XM_LABEL_GADGET_CLASS :new :managed
        "hophone-label" table-w
        :XMN_ALIGNMENT          :alignment_end
        :XMN_LABEL_STRING       "Home Phone:"
        )
  (send XM_LABEL_GADGET_CLASS :new :managed
        "wophone-label" table-w
        :XMN_ALIGNMENT          :alignment_end
        :XMN_LABEL_STRING       "Work Phone:"
        )
  (send XM_LABEL_GADGET_CLASS :new :managed
        "fax-label" table-w
        :XMN_ALIGNMENT          :alignment_end
        :XMN_LABEL_STRING       "Fax:"
        )
  (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
        "name-field" table-w
        )
  (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
        "addrs-0-field" table-w
        )
  (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
        "addrs-1-field" table-w
        )
  (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
        "addrs-2-field" table-w
        )
  (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
        "hophone-field" table-w
        )
  (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
        "wophone-field" table-w
        )
  (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
        "fax-field" table-w
        )
  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed 
        "clear-btn" button-box-w
        :XMN_LABEL_STRING       "Clear"
        )
  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed 
        "add-btn" button-box-w
        :XMN_LABEL_STRING       "Add"
        )
  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed 
        "search-btn" button-box-w
        :XMN_LABEL_STRING       "Search"
        )
  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed 
        "delete-btn" button-box-w
        :XMN_LABEL_STRING       "Delete"
        )

  (send top-w :realize)
  )
