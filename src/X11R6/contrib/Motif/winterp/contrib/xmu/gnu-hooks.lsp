; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         gnu-hooks.lsp
; RCS:          $Header: $
; Description:  GNU Elisp Handler for WINTERP menu server
; Author:       Richard Hess, Consilium.
; Created:      Sat Oct  5 23:59:10 1991
; Modified:     Sun Oct  6 00:03:23 1991 (Niels Mayer) mayer@hplnpm
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

(defvar *gnu_pick*  "/tmp/.xmu_output")  	;; the output file... [ HACK ]

(defun Gnu_cbk (tag)
  "[ GNU ]:  handle the menu selection for GNU Elisp interface..."
  (let* ((opt (open *gnu_pick* :direction :output))
	 )
    (if tag
	(format opt "~A~%" tag)
      (format opt "~A~%" ""))
    (close opt)
    ))

(defun Gnu_Touch ()
  "[ GNU ]:  touch the pick file... [ HACK ]"
  (close (open *gnu_pick* :direction :output))
  )

(Gnu_Touch)			;; [ HACK ]:  initialize the output file...

; -----------------------------------------------------------------------<eof>
