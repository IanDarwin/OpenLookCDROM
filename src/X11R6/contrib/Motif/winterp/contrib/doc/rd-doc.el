;;!emacs
;;
;; FILE:         rd-doc.el
;; SUMMARY:      Provide completion and documentation lookup facilities.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Brown University
;; ORIG-DATE:    19-Dec-90
;; LAST-MOD:     17-Sep-91 at 22:21:55 by Bob Weiner
;;
;; Copyright (C) 1990, 1991 Bob Weiner
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
;;
;;   'rd-doc' is invoked by env-specific '*-doc' packages.
;;   Each such package must override the values of all variables within the
;;   'Private variables' section below.
;;
;; DESCRIP-END.
;;

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

;;  Require 'wrolo' or 'rolo'.
;;  My rolodex package has been renamed 'wrolo' but many sites still call
;;  it rolo, so account for both possible names when trying to load it.
;;
(cond ((or (featurep 'wrolo) (load "wrolo" 'try)))
      ((or (featurep 'rolo)  (load "rolo"))))

;; ************************************************************************
;; Public functions
;; ************************************************************************

(defun rd-doc (str)
  "Display doc entries matching STR from 'rd-doc-files'."
  (interactive (list (rd-complete-name)))
  (let ((rolo-file-list rd-doc-files)
	(rolo-display-buffer rd-doc-buf))
    (rolo-fgrep str)))

(defun rd-complete-name (&optional must-match prompt)
  "Interactively completes symbol name from 'rd-doc-list' if possible, and returns class name.
Optional MUST-MATCH means name must match a completion table entry.
Optional PROMPT is intial prompt string for user."
  (interactive)
  (let ((default (rd-find-name))
	(completion-ignore-case t)
	completions
	name)
    ;; Prompt with possible completions of class-name.
    (setq prompt (or prompt "Doc name:")
	  completions rd-doc-list
	  name
	  (if completions
	      (completing-read
		(format "%s (default %s) " prompt default)
		completions nil must-match)
	    (read-string
	      (format "%s (default %s) " prompt default))))
    (if (equal name "") default name)))


(defun rd-find-name ()
  "Return symbol name that point is within, else nil."
  (if (= (point) (point-max)) (skip-chars-backward " \t\n"))
  (save-excursion
    (skip-chars-forward " \t")
    (skip-chars-backward rd-identifier-chars)
    (if (looking-at rd-identifier)
	(buffer-substring (point)
			  (match-end 0)))))

;; ************************************************************************
;; Public variables  - Overridden by environment-specific *-doc packages.
;; ************************************************************************

(defvar rd-doc-files nil
  "List of documentation files to search.
Overridden by environment-specific *-doc packages.
Each section within a file should begin with 'outline-regexp'.")

(defvar	rd-doc-buf nil
  "Name of buffer in which to display matching doc sections.
Overridden by environment-specific *-doc packages.")

(defvar	rd-doc-list nil
  "Completion list of keywords to use when prompting for doc search key.
Overridden by environment-specific *-doc packages.")

(defvar	rd-identifier-chars nil
  "Characters used to match doc key at point.
Overridden by environment-specific *-doc packages.")

(defvar rd-identifier nil
  "Regular expression matching a doc key at point.
Overridden by environment-specific *-doc packages.")


;; ************************************************************************
;; Function calls
;; ************************************************************************

(rd-doc-init)

(provide 'rd-doc)
