; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         List1.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/List1.lsp,v 2.4 1994/06/06 14:43:27 npm Exp $
; Description:  Demo of accessing/using Scrolled XmList widget's Scrolled
;		Window callbacks. This was used to prototype an incremental
;		list browser that would only read/generate data (object lists)
;		when the XmList was scrolled to the first or last list item.
;		this is useful for accessing persistent data that is potentially
;		unbounded in size... This demo, however, is boring...
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Sun Jun  5 17:35:11 1994 (Niels Mayer) npm@indeed
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

(setq to_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "listsh"
	    :XMN_TITLE		"WINTERP: XmList Test 1"
	    :XMN_ICON_NAME	"W:List1"
	    ))

(setq items-list 
      (do*
       (
	(p (popen "ls -ld $HOME/*"))
	(l (read-line p) (read-line p))
	(r nil)
	)
       ((null l)
	(reverse r)
	)
       (setq r (cons l r))
       )
      )


(setq list_w
      (send XM_LIST_WIDGET_CLASS :new :managed :scrolled
	    "list" to_w
	    :XMN_SELECTION_POLICY	:browse_select
	    :XMN_ITEMS			items-list
	    :XMN_ITEM_COUNT		(length items-list)
	    :XMN_VISIBLE_ITEM_COUNT	20
	    ))

(send to_w :realize)

(setq sw_w (send list_w :parent))
(setq sb_w (aref (send sw_w :get_children) 0))

(eq (send sb_w :class) XM_SCROLL_BAR_WIDGET_CLASS)

(+
 (car (send sb_w :get_values
	    :XMN_VALUE nil
	    ))
 (car (send sb_w :get_values
	    :XMN_SLIDER_SIZE nil
	    ))
 )

(send sb_w :set_callback :XMN_TO_TOP_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_TO_TOP_CALLBACK: callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_TO_BOTTOM_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_TO_BOTTOM_CALLBACK: callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_DECREMENT_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_DECREMENT_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_INCREMENT_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_INCREMENT_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))


(send sb_w :set_callback :XMN_PAGE_DECREMENT_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_PAGE_DECREMENT_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_PAGE_INCREMENT_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_PAGE_INCREMENT_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_VALUE_CHANGED_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_VALUE_CHANGED_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(+
 (car (send sb_w :get_values
	    :XMN_VALUE nil
	    ))
 (car (send sb_w :get_values
	    :XMN_SLIDER_SIZE nil
	    ))
 )

(send sb_w :set_callback :XMN_TO_TOP_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_TO_TOP_CALLBACK: callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_TO_BOTTOM_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_TO_BOTTOM_CALLBACK: callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_DECREMENT_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_DECREMENT_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_INCREMENT_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_INCREMENT_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))


(send sb_w :set_callback :XMN_PAGE_DECREMENT_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_PAGE_DECREMENT_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_PAGE_INCREMENT_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_PAGE_INCREMENT_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))

(send sb_w :set_callback :XMN_DRAG_CALLBACK
      '(callback_value callback_pixel)
      '(
	(format t ":XMN_DRAG_CALLBACK callback_value=~A callback_pixel=~A\n"
		callback_value callback_pixel)
	))


(let (max min)
  (send sb_w :get_values
	:XMN_MINIMUM 'min
	:XMN_MAXIMUM 'max
	)
  (send sb_w :set_event_handler BUTTON_RELEASE_MASK
	'(EVHANDLER_WIDGET)
	'(
	  (let (val delta)
	    (send sb_w :get_values
		  :XMN_VALUE 'val
		  :XMN_SLIDER_SIZE 'delta
		  )
	    (cond
	     ((= (+ val delta) max)
	      (format T "incr-scan-append\n")
	      )
	     ((= val min)
	      (format T "incr-scan-prepend\n")
	      )
	     )
	    )
	  )
	)
  )

