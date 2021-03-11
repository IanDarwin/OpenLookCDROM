; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xmenu-demo.el
; RCS:          $Header: $
; Description:  Example of xmenu Elisp interface...
; Author:       Richard Hess, Consilium.
;		Updated for gnuvo by Niels Mayer.
; Created:      Sat Oct  5 23:24:49 1991
; Modified:     Sat Oct  5 23:43:05 1991 (Niels Mayer) mayer@hplnpm
; Language:     Emacs-Lisp
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; +---------------------------------------------------------------------------
;  WHO:    Richard Hess                    CORP:   Consilium
;  TITLE:  Staff Engineer                  VOICE:  [415] 691-6342
;      [ X-SWAT Team:  Special Projects ]  USNAIL: 640 Clyde Court
;  UUCP:   ...!uunet!cimshop!rhess                 Mountain View, CA 94043
; +---------------------------------------------------------------------------

(require 'xmenu-lib)

(cond
 ;; special version of xmenu-buffer-select for gnuvo (based on 18.44.2)
 ((string= emacs-version "18.44.2")	
(defun xmenu-buffer-select (&optional arg)
  "switch-to-buffer using xmenu..."
  (interactive)
  (let* ((buflist (mapcar (function buffer-name) (buffer-list)))
	 (xpick   (xmenu "Select Buffer" (xmenu-buffer-filter buflist)))
	 )
    (if xpick
	(set-buffer (find-buffer xpick)))
      ))
)
  ;; normal gnuemacs version of xmenu-buffer-select
  (t
(defun xmenu-buffer-select (&optional arg)
  "switch-to-buffer using xmenu..."
  (interactive)
  (let* ((buflist (mapcar (function buffer-name) (buffer-list)))
	 (xpick   (xmenu "Select Buffer" (xmenu-buffer-filter buflist)))
	 )
    (if xpick
	(switch-to-buffer xpick))
    ))
)
)

(defun xmenu-buffer-filter (l)
  "remove private buffers who's name begins with a SPACE..."
  (if l
    (if (string-match "^ " (car l))    ;; REF:  (string-match "\*$" (car l))
	(xmenu-buffer-filter (cdr l))
      (cons (car l) (xmenu-buffer-filter (cdr l))))))


; (define-key global-map "\^x\^b" 'xmenu-buffer-select)

;; ----<eof>
