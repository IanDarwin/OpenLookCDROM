; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xbm-to-arr.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/xbm-to-arr.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  (bitmap-file-to-array <fname>) converts *.xbm bitmap file
;		to a xlisp 2-D array, for use by TANGO:BITMAP_IMAGE_CLASS
; Author:       Niels P. Mayer
; Created:      Fri May  7 20:23:26 1993
; Modified:     Mon Jun  6 04:33:50 1994 (Niels Mayer) npm@indeed
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

(defun bitmap-file-to-array (filename tango-fg tango-bg)
  (let* ((f
	  (open filename))
	 (width
	  (get-bitmap-file-width f))
	 (height
	  (get-bitmap-file-height f))
	 (bitmap_array (make-array height))
	 )

    (return-name-and-skip-to-beginning-of-bitmap-data f)

    (do*
	((row (read-bitmap-row-ret-array f width tango-fg tango-bg)
	      (read-bitmap-row-ret-array f width tango-fg tango-bg))
	 (i   0
	      (1+ i))
	 )
	((null row)
	 (close f)
	 bitmap_array			;RETURN VALUE
	 )

;;;   (format T "i=~A:~A\n" i row)
      (setf (aref bitmap_array i) row)
      )
    ))

(defun read-bitmap-row-ret-array (f width tango-fg tango-bg)
  (do*
   (
    (arr (make-array width))
    (base-idx 0 (+ 8 base-idx))		;increment by 8 since each fscanf-fixun reads one byte...
    (n 0)
    )
   ((>= base-idx width)			;loop until successfully read <width> # of bits		
    arr)				;RETURN array representing a row of the bitmap 

   (setq n (fscanf-fixnum f " %i,"))	;read 0x00-0xff, e.g. one byte return as fixnum
					;(note %x breaks on suns, %i works on suns and HP,
					;so hopefully it's somewhat standard in functionality).

   (if (null n)				;fscanf-fixnum will return NIL on EOF or conversion error
       (return nil)			;RETURN NIL
     )

   ;; convert byte FIXNUM <n> into 8 bits and store the bits in <arr>
   ;; beginning at <base-idx>...
   (do ((i 7 (1- i)))
       ((< i 0))
       (let ((y (expt 2 i))
	     (idx (+ base-idx i))
	     )

;;;	  (format T "i=~A, n=~A, rem=~A, div=~A\n" 
;;;		  i n (rem n y) (truncate n y))

	 (if (< idx width)
	     (if (eq 0 (truncate n y))
		 (setf (aref arr idx) tango-fg)
	       (setf (aref arr idx) tango-bg)
	       ))
	 (setq n (rem n y))
	 )
       )

;;; (format t "n=~A, base-idx=~A, b7=~A, b6=~A, b5=~A, b4=~A, b3=~A, b2=~A, b1=~A, b0=~A\n"
;;; 	    n
;;; 	    base-idx
;;; 	    (aref arr base-idx)
;;; 	    (aref arr (+ base-idx 1))
;;; 	    (aref arr (+ base-idx 2))
;;; 	    (aref arr (+ base-idx 3))
;;; 	    (aref arr (+ base-idx 4))
;;; 	    (aref arr (+ base-idx 5))
;;; 	    (aref arr (+ base-idx 6))
;;; 	    (aref arr (+ base-idx 7))
;;; 	    )
   ))

(defun get-bitmap-file-width (f)
  ;; read until hit "#define"
  (do () ((string= "#define" (fscanf-string f " %s "))) ) 

  ;; read until hit "_width"
  (do* ((wstr "_width")
	(wlen (length wstr))
	(str (fscanf-string f " %s ")
	     (fscanf-string f " %s "))
	)
      ((string= wstr str :start2 (- (length str) wlen) :end2 nil))
    )

  (fscanf-fixnum f " %d")		;RETURN the integer after "_width"
  )

(defun get-bitmap-file-height (f)
  ;; read until hit "#define"
  (do () ((string= "#define" (fscanf-string f " %s "))) ) ;read until hit "#define"

  ;; read until hit "_height"
  (do* ((wstr "_height")
	(wlen (length wstr))
	(str (fscanf-string f " %s ")
	     (fscanf-string f " %s "))
	)
      ((string= wstr str :start2 (- (length str) wlen) :end2 nil))
    )

  (fscanf-fixnum f " %d")		;RETURN the integer after "_height"
  )

(defun return-name-and-skip-to-beginning-of-bitmap-data (f)
  (let (name)
    (do () ((string= "static" (fscanf-string f " %s "))) ) ;read until hit "static"
    (do () ((string= "char" (fscanf-string f " %s "))) ) ;read until hit "char"
    (setq name (fscanf-string f " %[^_]_bits "))
    (do () ((string= "{" (fscanf-string f " %s "))) ) ;read until hit "{"
    name
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/xbm-to-arr")
