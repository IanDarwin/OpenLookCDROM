; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         reg-globals.lsp
; RCS:          $Header: $
; Description:  defines various globals for register displays...
; Author:       Niels Mayer, HPLabs
; Created:      Fri Aug 28 07:14:18 1992
; Modified:     Sun Jun  5 16:31:59 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       Experimental (Do Not Distribute)
;
; WINTERP Copyright 1989-1992 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and Niels Mayer not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and Niels Mayer
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *reg-num-rows* 16)
(defvar *num-registers* 128)

(defvar *reg-colorlist*
  (vector
   (x_alloc_color "red")
   (x_alloc_color "orange")
   (x_alloc_color "#d29400")
   (x_alloc_color "ForestGreen")
   (x_alloc_color "#5d759a")
   (x_alloc_color "brown")
   ))

(defvar *reg-colorlist-maxidx*
  (1- (length *reg-colorlist*))
  )
