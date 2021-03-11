;; Copyright (C) 1991 Bob Weiner
;; WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
;; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;;
;; Permission to use, copy, modify, distribute, and sell this software and
;; its documentation for any purpose is hereby granted without fee,
;; provided that the above copyright notice appear in all copies and that
;; both that copyright notice and this permission notice appear in
;; supporting documentation, and that the name of Hewlett-Packard, Niels
;; Mayer, Brown University and Bob Weiner not be used in advertising or
;; publicity pertaining to distribution of the software without specific,
;; written prior permission.  Hewlett-Packard, Niels Mayer, Brown University
;; and Bob Weiner makes no representations about the suitability of this
;; software for any purpose.  It is provided "as is" without express or
;; implied warranty.
;;
;; This file is not part of GNU Emacs.
;;

;;; Date: Wed, 19 Dec 90 04:43:24 -0500
;;; From: rsw@cs.brown.edu (Bob Weiner)
;;; Message-Id: <9012190943.AA07371@reverb.cs.brown.edu>
;;; To: mayer@hplnpm.hpl.hp.com
;;; Subject: Function to extract outline from structured text documents.
;;; 
;;; I noticed in your Winterp doc that you must use Emacs outlining mode a good
;;; deal, otherwise, why would you nest so much?
;;; 
;;; If you do, you'll probably find this function indispensible.  Try it.
;;; 
;;; Here's a sample output so you get the idea:
;;; * WINTERP <--> Motif Widget Classes
;;; ** WIDGET_CLASS -- the WINTERP widget metaclass.
;;; *** equivalent Xt 'WidgetClass':
;;; *** equivalent creation convenience function:
;;; *** XtCreateWidget():
;;; *** XtSetValues():
;;; *** XtGetValues():
;;; *** XtAddCallback():

(global-set-key "\C-x4o" 'outline-extract)
(defun outline-extract ()
  "[weiner] Copy outline structure from current buffer to \"/tmp/otl\".
Assume bodies in current buffer are already hidden.  This allows the user to
select the levels of the outline that are copied.  Otherwise, use {M-x
hide-body} when in outline-mode, to hide everything but headings."
  (interactive)
  (let ((start) (end)
	(obuf (current-buffer))
	(buf (get-buffer-create "/tmp/otl")))
    (save-excursion 
      (goto-char (point-min))
      (while (progn (setq start (point))
		    (re-search-forward "[\n]" nil t))
	(backward-char 1)
	(setq end (point))
	(set-buffer buf)
	(insert-buffer-substring obuf start end)
	(insert "\n")
	(set-buffer obuf)
	(forward-line 1)))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (delete-matching-lines "")))
