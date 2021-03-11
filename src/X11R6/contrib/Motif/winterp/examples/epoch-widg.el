; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         epoch-widg.el
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/epoch-widg.el,v 2.2 1994/06/06 14:43:18 npm Exp $
; Description:  Epoch 4.X Emacs-Lisp functions used by epoch-widg.lsp in
;		this directory. You must load these functions into Epoch
;		before you load epoch-widg.lsp into WINTERP.
; Author:       Niels P. Mayer
; Created:      Tue Mar  2 07:00:50 1993
; Modified:     Sun Jun  5 18:38:51 1994 (Niels Mayer) npm@indeed
; Language:     Emacs-Lisp
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

;(setq debug-on-error t)
;(setq stack-trace-on-error t)


(setq *window-type-atom* (intern-atom "WINDOW"))
(setq *winterp-window-id-to-epoch-screen-alist* '())

(defun win-create-scrn (scrn-win-id-str)
  (let ((scrn
	 (epoch::create-screen
	  nil				;default to  *scratch* buffer
	  (list 
	   (cons 'parent
		 (string-to-resource scrn-win-id-str *window-type-atom*))
	   )))
	)
    (if scrn
	(progn (message "creating winterp screen ...")
	       (sit-for 0 nil))
      (error
       "Error in win-create-scrn -- 'create-screen' failed parent windowID=%s."
       scrn-win-id-str)
      )
    (setq *winterp-window-id-to-epoch-screen-alist*
	  (cons (cons scrn-win-id-str scrn)
		*winterp-window-id-to-epoch-screen-alist*))
    (mapraised-screen scrn)
;;; (select-screen scrn)
    )
  )

;;; NOT NEEDED -- ONCE CREATED, EPOCH REFRESHES ITSELF...
;;; (defun win-refresh-scrn (scrn-win-id-str)
;;; )

(defun win-destroy-scrn (scrn-win-id-str)
  (let ((scrn-tuple
	 (assoc scrn-win-id-str *winterp-window-id-to-epoch-screen-alist*))
	)
    (if scrn-tuple
	(progn
	  (message "destroying winterp screen ...")
	  (sit-for 0 nil)
	  (delete-screen (cdr scrn-tuple)) ;get rid of the epoch screen...
	  (setq *winterp-window-id-to-epoch-screen-alist*
		(delete-if-eq scrn-tuple *winterp-window-id-to-epoch-screen-alist*))
	  )
      (error
       "Error in 'win-destroy-scrn' -- couldn't find epoch screen w/ parent windowID=%s"
       scrn-win-id-str)
      )
    ))

(defun win-id-to-scrn (scrn-win-id-str)
  (let ((scrn-tuple
	 (assoc scrn-win-id-str *winterp-window-id-to-epoch-screen-alist*))
	)
    (if scrn-tuple
	(cdr scrn-tuple)
      (error
       "Error in 'win-id-to-scrn' -- couldn't find epoch screen w/ parent windowID=%s"
       scrn-win-id-str)
      )
    ))

;; returns list (char-width char-height x y width height border-w border-h map-state)
(defun win-get-epoch-screen-info (scrn-win-id-str)
  (let ((sc (win-id-to-scrn scrn-win-id-str))
	)
    (cons (epoch::screen-width sc)
	  (cons (epoch::screen-height sc) (epoch::screen-information sc)))
    )
  )

;;
(defun win-find-file-in-screen (scrn-win-id-str filename)
  (let ((cscr (current-screen))
	ptmax)
    (select-screen (win-id-to-scrn scrn-win-id-str))
    (switch-to-buffer (find-file-noselect filename))
    (setq ptmax (point-max))
    (select-screen cscr)
    (1- ptmax)				;return length of file == (1- (point-max))
    )
  )

;;
(defun win-goto-line-in-screen (scrn-win-id-str linenum highlight_p)
  (let ((cscr (current-screen))
	pos)
    (select-screen (win-id-to-scrn scrn-win-id-str))
    (goto-line linenum)
    (recenter)
    (setq pos (point))
    (select-screen cscr)
    (1- pos)				;return position of selected line == (1- (point))
    )
  )


