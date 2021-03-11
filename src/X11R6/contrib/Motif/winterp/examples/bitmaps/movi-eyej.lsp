; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         movi-eyej.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/bitmaps/RCS/movi-eyej.lsp,v 2.1 1994/06/06 14:51:07 npm Exp $
; Description:  sets global *eyej-movie* to 3d array containing a sequence of
;		bitmaps of an eye-man jumping.. Use this with
;		TANGO:BITMAP_IMAGE_CLASS ...
; Author:       Niels P. Mayer
; Created:      Fri May  7 20:46:14 1993
; Modified:     Mon Jun  6 00:14:39 1994 (Niels Mayer) npm@indeed
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
 
;;; (progn
;;;   (require "xtango/xbm-to-arr")
;;;   (progv
;;;    '(*bitmap-pixel-off-value*	*bitmap-pixel-on-value*)
;;;    '(0				7)
;;;    (setq *eyej-movie*
;;; 	 (vector 
;;; 	  (bitmap-file-to-array "/usr/local/include/X11/AIcons/movies/eyejump_1.xbm" TANGO_COLOR_BLACK TANGO_COLOR_WHITE)
;;; 	  (bitmap-file-to-array "/usr/local/include/X11/AIcons/movies/eyejump_2.xbm" TANGO_COLOR_BLACK TANGO_COLOR_WHITE)
;;; 	  (bitmap-file-to-array "/usr/local/include/X11/AIcons/movies/eyejump_3.xbm" TANGO_COLOR_BLACK TANGO_COLOR_WHITE)
;;; 	  (bitmap-file-to-array "/usr/local/include/X11/AIcons/movies/eyejump_4.xbm" TANGO_COLOR_BLACK TANGO_COLOR_WHITE)
;;; 	  ))
;;;    )
;;;   (setq fi (open "/tmp/movie.lsp" :direction :output))
;;;   (format fi "~A" *eyej-movie*)
;;;   (close fi)
;;;   )

(setq *eyej-movie*
      #(
	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 7 7 7 7 7 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 7 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 7 0 0 0 7 0 7 7 7 0 0 0 7 0 0 0 7 0 0 0 0)
	  #(0 0 0 7 0 0 0 0 7 7 7 7 0 0 7 0 0 0 0 7 0 0 0 0)
	  #(0 0 7 0 0 0 0 7 0 7 7 7 7 7 0 7 0 0 0 0 7 0 0 0)
	  #(0 0 7 0 7 0 0 0 7 7 7 7 7 7 7 0 0 0 7 0 7 0 0 0)
	  #(0 7 0 0 7 0 0 7 0 7 7 7 7 7 0 7 0 0 7 0 0 7 0 0)
	  #(0 7 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 7 0 7 0 0)
	  #(7 0 0 0 7 0 7 0 0 0 0 0 0 0 0 0 7 0 7 0 0 0 7 0)
	  #(7 0 0 0 7 0 0 7 7 0 0 7 0 0 7 7 0 0 7 0 0 0 7 0)
	  #(0 7 7 7 0 0 0 0 7 0 7 0 7 0 7 0 0 0 0 7 7 7 0 0)
	  #(0 0 0 0 0 0 7 7 0 0 7 0 7 0 0 7 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 7 7 0 0 0 0 0 7 0 0 0 0 0 7 7 0 0 0 0 0)
	  #(0 0 0 7 7 7 7 7 7 7 7 0 7 7 7 7 7 7 7 7 0 0 0 0)
	  )
	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 7 7 7 7 7 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 7 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 7 0 0 0 7 0 7 7 7 0 0 0 7 0 0 0 7 0 0 0 0)
	  #(0 0 7 0 0 0 0 0 7 7 7 7 0 0 7 0 0 0 0 0 7 0 0 0)
	  #(0 7 0 0 0 0 0 7 0 7 7 7 7 7 0 7 0 0 0 0 0 7 0 0)
	  #(0 7 0 0 7 0 0 0 7 7 7 7 7 7 7 0 0 0 7 0 0 7 0 0)
	  #(0 7 0 7 7 0 0 7 0 7 7 7 7 7 0 7 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 7 0 7 0 0)
	  #(7 0 0 0 7 7 0 0 0 0 0 0 0 0 0 0 0 7 7 0 0 0 7 0)
	  #(7 0 0 0 7 0 7 0 0 0 0 0 0 0 0 0 7 0 7 0 0 0 7 0)
	  #(0 7 7 7 0 0 0 7 7 0 0 7 0 0 7 7 0 0 0 7 7 7 0 0)
	  #(0 0 0 0 0 0 0 0 7 0 7 0 7 0 7 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 7 0 7 0 7 0 7 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 7 0 0 7 0 7 0 0 7 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 7 7 0 0 0 0 0 7 0 0 0 0 0 7 7 0 0 0 0 0)
	  #(0 0 0 7 7 7 7 7 7 7 7 0 7 7 7 7 7 7 7 7 0 0 0 0)
	  )
	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 7 7 7 7 7 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 7 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 0 0 0)
	  #(0 0 7 7 0 0 0 7 0 7 7 7 0 0 0 7 0 0 0 7 7 0 0 0)
	  #(0 7 0 0 0 0 0 0 7 7 7 7 0 0 7 0 0 0 0 0 0 7 0 0)
	  #(0 7 0 0 0 0 0 7 0 7 7 7 7 7 0 7 0 0 0 0 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 7 7 7 7 7 7 7 0 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 7 0 0 7 0 7 7 7 7 7 0 7 0 0 7 7 0 7 0 0)
	  #(0 7 0 7 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 7 0 7 0 0)
	  #(7 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 7 0)
	  #(7 0 0 0 7 7 0 0 0 0 0 0 0 0 0 0 0 7 7 0 0 0 7 0)
	  #(0 7 7 7 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 7 7 7 0 0)
	  #(0 0 0 0 0 0 0 7 0 0 7 7 7 0 0 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 7 0 0 0 7 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 0 7 0 0 0 7 0 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 7 0 0 7 0 7 0 0 7 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 7 7 0 0 0 0 0 7 0 0 0 0 0 7 7 0 0 0 0 0)
	  #(0 0 0 7 7 7 7 7 7 7 7 0 7 7 7 7 7 7 7 7 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  )
	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 7 7 7 7 7 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 7 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0)
	  #(0 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 0 0)
	  #(0 0 7 7 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 7 7 0 0 0)
	  #(0 7 0 0 0 0 0 7 0 7 7 7 0 0 0 7 0 0 0 0 0 7 0 0)
	  #(0 7 0 7 7 7 0 0 7 7 7 7 0 0 7 0 0 7 7 0 0 7 0 0)
	  #(0 7 0 0 7 0 0 7 0 7 7 7 7 7 0 7 0 0 7 0 0 7 0 0)
	  #(0 7 0 0 7 0 0 0 7 7 7 7 7 7 7 0 0 0 7 0 0 7 0 0)
	  #(0 7 7 7 0 0 0 7 0 7 7 7 7 7 0 7 0 0 0 7 7 7 0 0)
	  #(7 0 0 0 7 0 0 0 7 0 7 0 7 0 7 0 0 0 7 0 0 0 7 0)
	  #(7 0 0 0 7 0 0 0 0 7 0 7 0 7 0 0 0 0 7 0 0 0 7 0)
	  #(0 7 7 7 0 7 0 0 0 0 0 0 0 0 0 0 0 7 0 7 7 7 0 0)
	  #(0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 7 0 0 7 7 7 0 0 7 0 0 0 0 0 0 0 0)
	  #(0 7 7 7 0 0 7 0 0 7 0 0 0 7 0 0 7 0 0 7 7 7 0 0)
	  #(0 0 7 0 7 7 0 0 7 0 0 0 0 0 7 0 0 7 7 0 7 0 0 0)
	  #(0 0 0 7 0 0 0 7 0 0 0 0 0 0 0 7 0 0 0 7 0 0 0 0)
	  #(0 0 0 0 7 0 7 0 0 0 0 0 0 0 0 0 7 0 7 0 0 0 0 0)
	  #(0 0 0 0 0 7 7 0 0 0 0 0 0 0 0 0 7 7 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  )
	)
      )
