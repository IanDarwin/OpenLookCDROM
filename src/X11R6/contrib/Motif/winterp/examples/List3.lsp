;; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         List3.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/List3.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  Demo of XmList using multiple font-lists and multi-line list
;		entries. I think multi-line list entries only work as of
;		Motif 1.2. This assumes your system has the following fonts
;		roman: -*-courier-medium-r-normal-*-12-*-*-*-m-*-iso8859-1
;		bold:  -*-courier-bold-r-normal-*-12-*-*-*-m-*-iso8859-1
; Author:       Niels P. Mayer
; Created:      1994
; Modified:     Sun Jun  5 17:43:46 1994 (Niels Mayer) npm@indeed
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an example of using multiple character sets with XM_LIST_WIDGET_CLASS
;; hopefully, your system has these fonts! if not, set this as appropriate...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *LIST2-NORMAL-CHARSET*	"NORMAL_CS")
(defvar *LIST2-BOLD-CHARSET*	"BOLD_CS")
(defvar *LIST2-FONTLIST*
  ;; N.B.: Motif 1.2.3 (and Irix 5.2) will puke if there are spaces/newlines
  ;; in the font-list definition, so don't allow space, Motif will either
  ;; generate an error message -- Warning: Unmatched quotation marks in string "", any remaining fonts in list unparsed
  ;; or it will core dump WINTERP. Therefore, don't put spaces/newlines here!
  (format nil				
	  "fixed,-*-courier-medium-r-normal-*-12-*-*-*-m-*-iso8859-1=~A,-*-courier-bold-r-normal-*-12-*-*-*-m-*-iso8859-1=~A"
	  *LIST2-NORMAL-CHARSET* *LIST2-BOLD-CHARSET*
	  ))


;; remove the last word from <str> where words are delimited by the
;; space character
(defun remove-last-word (str)
  (do
   ((i (1- (length str)) (1- i))
    )
   ((or (< i 0) (char= (char str i) #\ ))
    (if (< i 0)
        str
      (subseq str 0 i))
    )
   )
  )

;; retrieve the last word from <str> where words are delimited by the
;; space character
(defun list3-get-last-word (str)
  (do
   ((i (1- (length str)) (1- i))
    )
   ((or (< i 0) (char= (char str i) #\ ))
    (if (< i 0)
        NIL
      (subseq str (1+ i) nil))
    )
   )
  )

(setq to_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "listsh"
	    :XMN_TITLE		"WINTERP: XmList Test 2"
	    :XMN_ICON_NAME	"W:List2"
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

(setq xm-str-items-list
      (map 'list #'(lambda (s)
		     (xm_string_concat
		      (xm_string_segment_create
		       (list3-get-last-word s)
		       *LIST2-BOLD-CHARSET*
		       :STRING_DIRECTION_L_TO_R
		       T)		;T-->create a separator
		      (xm_string_segment_create
		       (concatenate 'string "   " (remove-last-word s))
		       *LIST2-NORMAL-CHARSET*
		       :STRING_DIRECTION_L_TO_R
		       NIL))		;NIL-->don't create separator
		     )
	   items-list
	   ))

(setq list_w
      (send XM_LIST_WIDGET_CLASS :new :managed :scrolled
	    "list" to_w
	    :XMN_SELECTION_POLICY	:browse_select
	    :XMN_FONT_LIST		*LIST2-FONTLIST*
	    :XMN_ITEMS			xm-str-items-list
	    :XMN_ITEM_COUNT		(length xm-str-items-list)
	    :XMN_VISIBLE_ITEM_COUNT	20
	    ))

(send to_w :realize)
