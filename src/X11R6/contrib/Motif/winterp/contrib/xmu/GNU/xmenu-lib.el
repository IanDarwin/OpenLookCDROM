; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xmenu-lib.el
; RCS:          $Header: $
; Description:  Simple interface to xmu-menu.el WINTERP-based menu server.
; Author:       Richard Hess, Consilium.
; Created:      Sat Oct  5 23:24:49 1991
; Modified:     Sat Oct  5 23:42:54 1991 (Niels Mayer) mayer@hplnpm
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

;; ---------------------------------------------------------------------------
;; REF:   Derived from work by Mark A. Kolb & Gill Derge... [ gnu.epoch.misc ]
; +---------------------------------------------------------------------------
;  WHO:    Richard Hess                    CORP:   Consilium
;  TITLE:  Staff Engineer                  VOICE:  [415] 691-6342
;      [ X-SWAT Team:  Special Projects ]  USNAIL: 640 Clyde Court
;  UUCP:   ...!uunet!cimshop!rhess                 Mountain View, CA 94043
; +---------------------------------------------------------------------------

(require 'xmu-menu)
(provide 'xmenu-lib)

(defvar xmenu-max-width 20)

;; ------------------------------------------------------------------[ XMENU ]

(defun xmenu (heading choices &optional choice-list)
  "Pops up an XMENU menu to select a string from among CHOICES.
Returns the selected string or NIL if the abort option is selected.
If CHOICE-LIST is provided, then instead of returning a string from
CHOICES, the corresponding item from CHOICE-LIST is returned."
  (let* ()
    (let ((filtered-choices (xmenu-string-items choices))
	  (choice-count (length choices))
	  (key (user-real-uid))
	  )
      (xmu_menu key heading filtered-choices "::GNU [ xmenu ]")
      (let* ((xpick (xmu_popup key t))
	     (result-int (string-to-int xpick))
	     (result (if (or (equal xpick "")
			     (= result-int choice-count))
			 nil
		       (nth result-int (or choice-list choices)))))
	result))))

;;;  Auxiliary functions for filtering XMENU choice strings.
;;;  This munging is necessary because we're passing out arbitrary
;;;  strings to the shell.  Much of this code is also courtesy Gill Derge.

(defun xmenu-string-items (strings)
  "Transform STRINGS into a list of menu choices suitable for XMENU,
tacking on an equals sign and a count onto the end of each transformed string."
  (%xmenu-string-items strings 0))

(defun %xmenu-string-items (strings count)
  (if (null strings) nil
    (cons (xmenu-string-item (car strings) count)
	  (%xmenu-string-items (cdr strings) (1+ count)))))

(defun xmenu-string-item (string count)
  "Transforms STRING into a choice-item for XMENU.
This function will also tack on the the number of the item.
Thus, (xmenu-string-item \"This string\") will yield \"This string=0\"
if COUNT is zero.  Truncation is also performed here--menu items are of
finite width."
  (let ((shortened-string (if (< (length string) xmenu-max-width)
			      string
			    (concat (substring string 0 (- xmenu-max-width 3))
				    "..."))))
    (cons (format "%s"
		  (xmenu-quote-regexp
		   (xmenu-quote-regexp shortened-string "\\\\") "="))
	  (cons (format "%d" count)
		nil))
    ))

(defun xmenu-quote-regexp (string regexp)
  "Quote anything in STRING that matches REGEXP with '\'."
  (let ((str-len (length string))
	(work-str string)
	last-match)
    (setq last-match 0)
    (while (< last-match str-len)
      (if (string-match regexp (substring work-str last-match nil))
	  (progn (setq last-match (+ last-match (match-end 0)))
		 (setq work-str
		       (concat
			(substring work-str 0 (- last-match 1)) "\\"
			(substring work-str (- last-match 1) nil)))
		 (setq last-match (+ last-match 2))
		 (setq str-len (+ str-len 2)))
	(setq last-match str-len)))
    work-str))

;; ----<eof>
