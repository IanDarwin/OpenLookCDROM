;;!emacs
;;
;; FILE:         rolo-simple.el
;; SUMMARY:      Very simple routines to display entries matching a string
;;                 from a rolodex file.  For those who find "rolo.el" too
;;                 intimidating.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., Communications Sector, Applied Research
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;;
;; ORIG-DATE:     7-Jun-89 at 22:08:29
;; LAST-MOD:     16-Jun-89 at 00:27:20 by Bob Weiner
;;
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
;; DESCRIPTION:  
;; DESCRIP-END.

(defconst rolo-file "~/.rolodex.otl"
  "User-specific file in which rolodex entries are stored.")

(defconst rolo-entry-regexp "^\*+"
  "Regular expression to match the beginning of a rolodex entry.")

(defconst rolo-hdr-regexp "^==="
  "Regular expression to match the last line of the rolodex file header.
This header is inserted into rolo-display-buffer before any entries are
added.")

(defconst rolo-display-buffer "*Rolodex*"
  "Buffer used to display set of last matching rolodex entries.")

(defun rolo-fgrep (string)
  "Find entries in rolo-file matching STRING.
The rolo-file consists of a header terminated by a line which matches
rolo-hdr-regexp.  And rolodex entries beginning with rolo-entry-regexp."
  (interactive "sRolodex string to match: ")
  (let ((obuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create rolo-display-buffer))
      (erase-buffer)
      (find-file rolo-file)
      (goto-char (point-min))
      (save-excursion
	(if (re-search-forward rolo-hdr-regexp nil t)
	    (progn (forward-line)
		   (append-to-buffer rolo-display-buffer
				     (point-min) (point)))))
      (re-search-forward rolo-entry-regexp nil t)
      (beginning-of-line)
      (while (search-forward string nil t)
	(re-search-backward rolo-entry-regexp nil t)
	(let ((start (point)))
	  (if (re-search-forward rolo-entry-regexp nil t 2)
	      (beginning-of-line)
	    (goto-char (point-max)))
	  (append-to-buffer rolo-display-buffer start (point)))))
    (pop-to-buffer rolo-display-buffer)
    (set-buffer-modified-p nil)
    (select-window (get-buffer-window obuf))))

(defun rolo-edit ()
  "Display user-specific rolodex file for editing."
  (interactive)
  (find-file rolo-file))

(provide 'rolo)
