;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         prov-req.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-utils/RCS/prov-req.lsp,v 2.1 1994/06/06 14:54:46 npm Exp $
; Description:  WARNING -- WINTERP 2.0 HAS BUILT-IN 'PROVIDE' and 'REQUIRE'
;		SO YOU PROBABLY SHOULD NOT USE THIS FILE FOR NEW PROGRAMS.
;		Pseudo version of common lisp's provide/require functionality
;		Note that this uses the X11r4 routine XT_RESOLVE_PATHNAME
;		XtResolvePathname(), therefore you can only use this w/
;		Motif >= 1.1.
; Author:       Eric Blossom, HP Response Center Lab; Niels Mayer
; Created:      Mon Feb 12 19:05:25 1990
; Modified:     Mon Jun  6 00:49:43 1994 (Niels Mayer) npm@indeed
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

;;;
;;; I use prov-req.lsp to set up a load-path for WINTERP.
;;;
;;; Put 
;;; (PROVIDE <module-name>) 
;;; at the end of each file/module/class/<program-unit>
;;; PROVIDE indicates that a file <module-name> has been
;;; loaded. Once a file is LOADed and PROVIDE'd,
;;; (REQUIRE <module-name>) will not re-LOAD
;;; <module-name> again. <module-name> can be a string
;;; or a symbol.
;;;
;;; (REQUIRE <module-name> [<path>])
;;; will load <module-name> if it hasn't been PROVIDE'd
;;; already. <module-name> is a string or a symbol. If
;;; <path> isn't provided, *default-load-path*=="%N%S:%N"
;;; is used in it's place. See documentation for
;;; XtResolvePathname() for an explanation of load-paths.
;;; If <module-name>=="file", then 
;;; by default, the default load path will load
;;; "file.lsp" out of the directory specified by resource
;;; "Winterp.lispLibDir" or by the command line arg -lisp_lib_dir.
;;; ... 
;;;
;;; I do the following in my winterp startup file ~/.winterp,
;;; which I specify thru Xdefault "Winterp.lispInitFile: /users/mayer/.winterp"
;;;
;;; (setq *default-load-path*
;;;       (concatenate 'string
;;;        "/users/mayer/%N%S:"
;;;        "/users/mayer/%N:"
;;;        "/users/mayer/src/widgit/examples/%N%S:"
;;;        "/users/mayer/src/widgit/examples/%N:"
;;;        "/tmp/%N%S:"
;;;        "/tmp/%N"
;;;        ))
;;;
;;; (require "lib-utils/prov-req")

(defvar *default-load-path* "%N%S:%N")	;load-path, see XtResolvePathname()
(defvar *modules-size* 20)		;size of hashtable.
(setq *modules* (make-array *modules-size*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun provide (module-name)
  (let ((file-str (cond ((symbolp module-name)
			 (symbol-name module-name))
			((stringp module-name)
			 module-name)
			(t
			 (error "Wrong type argument to PROVIDE" module-name)))
		  ))
    
    (let ((hashbucket (aref *modules* (hash file-str *modules-size*))))
      (if (member file-str (aref *modules* (hash file-str *modules-size*))
		  :test #'equal)
	  t
	(setf (aref *modules* (hash file-str *modules-size*))
	      (cons file-str hashbucket))
	))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun require (module-name &optional pathname)
  (let ((file-str (cond ((symbolp module-name)
			 (symbol-name module-name))
			((stringp module-name)
			 module-name)
			(t
			 (error "Wrong type argument to REQUIRE" module-name)))))

    (if (member file-str (aref *modules* (hash file-str *modules-size*))
		:test #'equal)
	t		
      (let (
	    (filepath-str (XT_RESOLVE_PATHNAME nil ;ignore all %T substitutions
					       file-str	;substitute for %N
					       ".lsp" ;substitute for %S
					       (if pathname pathname *default-load-path*))))
	(if (null filepath-str)
	    (error (format nil "Can't find module ~A in path ~A" file-str (if pathname pathname *default-load-path*))))

	(if (not (load filepath-str :verbose T :print NIL))
	    (error "Can't load required module" filepath-str)
	  t
	  ))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "prov-req")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (provide "lib-utils/prov-req")
