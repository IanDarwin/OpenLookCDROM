;; WINTERP Copyright 1989-1992 Hewlett-Packard Company (by Niels Mayer).
;; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;; 
;; Permission to use, copy, modify, distribute, and sell this software and its
;; documentation for any purpose is hereby granted without fee, provided that
;; the above copyright notice appear in all copies and that both that
;; copyright notice and this permission notice appear in supporting
;; documentation, and that the name of Hewlett-Packard and Niels Mayer not be
;; used in advertising or publicity pertaining to distribution of the software
;; without specific, written prior permission.  Hewlett-Packard and Niels Mayer
;; makes no representations about the suitability of this software for any
;; purpose.  It is provided "as is" without express or implied warranty.


;;; Date: Mon, 17 Dec 90 16:46:39 -0500
;;; From: rsw@cs.brown.edu (Bob Weiner)
;;; Message-Id: <9012172146.AA06790@fiddle.cs.brown.edu>
;;; To: mayer@hplnpm.hpl.hp.com
;;; Subject: Generalization of winterp.el code.
;;; 
;;; The generalization I include here substitutes an eval-last-sexp type function
;;; for your send-defun.  The send-defun function may be left around but really is
;;; not needed since all one need do is move to the end of the function and
;;; evaluate it.  This makes the interface so much simpler when one wants to do
;;; bottom up testing via the interpreter.
;;; 
;;; I've included only the one function and the key binding, so you should
;;; substitute it into your winterp.el file.

(defun winterp-eval-last-sexp ()
  "A version of eval-last-sexp that talks to WINTERP's lisp server via the
winterp client program 'wl' (which must be on your search path).  Evaluates
sexp immediately preceding point."
  (interactive)
  (if (not (get-process "winterp-client-shell"))
      (make-shell "winterp-client-shell" winterp-client-shell)
    )
  (let ((loadfile (format "/tmp/wl%d.lsp"
			  (process-id (get-process "winterp-client-shell"))))
	(stab (syntax-table)))
    (write-region
     (unwind-protect
	 (save-excursion
	   (set-syntax-table emacs-lisp-mode-syntax-table)
	   (forward-sexp -1)
	   (point))
       (set-syntax-table stab))
     (point)
     loadfile nil 'nomessage)
    (process-send-string "winterp-client-shell"
			 (format "%s %s '(load \"%s\" :verbose nil
                                  :print t)'\n" 
				  winterp-client-program
				  winterp-client-program-args
				  loadfile))
    )
  )

(define-key lisp-mode-map "\e\C-x" 'winterp-eval-last-sexp)

;;; 
;;; Date: Mon, 17 Dec 90 16:56:32 -0500
;;; From: rsw@cs.brown.edu (Bob Weiner)
;;; Message-Id: <9012172156.AA06799@fiddle.cs.brown.edu>
;;; To: mayer@hplnpm.hpl.hp.com
;;; Subject: Further generalization of winterp.el code.
;;; 
;;; We can further simplify the interface by combinding the expression evaluation
;;; command with the buffer evaluation command.  The latter is called when a prefix
;;; argument is sent to the 'winterp-eval' command.  Thus, only one key is needed,
;;; but both functions are accessible.  Try it.
;;;

(defun winterp-eval (&optional arg)
  "Evaluate last sexpression.  With prefix ARG, evaluate visible portion of buffer."
  (interactive "P")
  (if arg
      (winterp-send-buffer)
    (winterp-eval-last-sexp)))

(define-key lisp-mode-map "\M-\C-x" 'winterp-eval)


;;; To: mayer@hplabs.hpl.hp.com
;;; Cc: gildea@alexander.bbn.com
;;; Subject: XLisp comments
;;; Date: Tue, 02 Oct 90 11:45:16 EDT
;;; From: Stephen Gildea <gildea@alexander.bbn.com>
;;; 
;;; I found I needed the function winterp-send-region, so I added it to
;;; winterp.el.  Here it is:

(defun winterp-send-region (start end)
  "Send the current region off to WINTERP for evaluation.
This routine talks to WINTERP's lisp server via the winterp client
program 'wl' (which must be on your search path)."
  (interactive "r")
  (if (not (get-process "winterp-client-shell"))
      (make-shell "winterp-client-shell" winterp-client-shell))
  (let ((loadfile (format "/tmp/wl%d.lsp"
			  (process-id (get-process "winterp-client-shell")))))
    (save-excursion (write-region start end loadfile nil 'nomessage))
    (process-send-string "winterp-client-shell"
			 (format
			  "%s %s '(load \"%s\" :verbose nil :print t)'\n"
			  winterp-client-program winterp-client-program-args
			  loadfile))))
