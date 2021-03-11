;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         scooter.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/scooter.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  a silly example that scoots (moves) windows around the
;		screen while changing their colors. This can really tie up your
;		X server and window manager, so be careful...
; Author:       Niels Mayer
; Created:      Sat Oct  5 18:49:55 1991
; Modified:     Mon Jun  6 00:29:00 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define *X11-BITMAPS-DIRECTORY*

(setq top_w
      (send OVERRIDE_SHELL_WIDGET_CLASS :new 
	    :xmn_geometry "+1+1"
	    ))
(setq pb_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed top_w
	    :XMN_LABEL_TYPE   :pixmap
	    :XMN_LABEL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo64")
	    ))
(send top_w :realize)

(defun randomove()
  (do
   ((i 0 (1+ i)))
   ((eq 100 i)
    )
   (send top_w :set_values
	 :xmn_x (random 1000)
	 :xmn_y (random 1000)
	 )
   )
  )
(defun scoot(d)
  (do
   ((i 0 (1+ i))
    (x 0 (+ d x))
    (y 0 (+ d y)))
   ((eq 100 i)
    )
   (send top_w :set_values
	 :xmn_x x
	 :xmn_y y
	 )
   )
  )


(scoot 1)
(scoot 5)
(scoot 10)


(defun colorize ()
  (do* 
   ((fp (open "/usr/lib/X11/rgb.txt" :direction :input))
    (color
     (fscanf-string fp "%*d %*d %*d %[^\n]\n")
     (fscanf-string fp "%*d %*d %*d %[^\n]\n"))
    (x 1 (1+ x))
    (y 1 (1+ y))
    )
   ((null color)
    (close fp)
    )

   (send top_w :set_values 
	 :xmn_x x
	 :xmn_y y
	 )
   (send pb_w :set_values
	 :xmn_background color)
;   (send pb_w :update_display)
   ))

(colorize)

