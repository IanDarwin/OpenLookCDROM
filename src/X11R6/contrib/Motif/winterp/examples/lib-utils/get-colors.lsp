;; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         get-colors.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-utils/RCS/get-colors.lsp,v 2.1 1994/06/06 14:54:46 npm Exp $
; Description:  Sets up *X11-COLORS-LIST*, which is used by other applications
;		that need a list of the system colors from /usr/lib/X11/rgb.txt
; Author:       Niels P. Mayer
; Created:      Mon Jun  6 00:36:08 1994
; Modified:     Mon Jun  6 00:38:49 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define unix:get-uname and other unixisms etc.

;;;
;;; This variable holds the pathname for the X11 RGB file, which is used by
;;; xtango/imag-build.lsp (actually used by lib-utils/get-colors.lsp which
;;; is used by xtango/wcls-fgcol.lsp xtango/wcls-bgcol.lsp ...).
;;;

(defvar *X11-RGB-TXT-FILEPATH*
  (cond
   ((equal (unix:get-uname) "SunOS")
    "/usr/openwin/lib/X11/rgb.txt")
   (t
    "/usr/lib/X11/rgb.txt")		;default location
   ))


(defvar *X11-COLORS-LIST*
  (let ((fp (open *X11-RGB-TXT-FILEPATH*
		  :direction :input :if-does-not-exist nil)))
    (if (null fp)
	(list (format nil "<<Error: Can't find '~A'>>" ;ERROR-RETURN
		      *X11-RGB-TXT-FILEPATH*))
      (do* 
       ((result NIL)
	(color
	 (fscanf-string fp "%*d %*d %*d %[^\n]\n")
	 (fscanf-string fp "%*d %*d %*d %[^\n]\n"))
	)
       ((null color)
	(close fp)
	(reverse result)		;RETURN
	)
       (setq result (cons color result))
       )
      ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-utils/get-colors")
