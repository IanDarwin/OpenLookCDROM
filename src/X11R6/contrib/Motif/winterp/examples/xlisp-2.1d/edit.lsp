; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         edit.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/edit.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Edit a file...
; Author:       Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 03:02:41 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       X11r6 contrib release
; 
; Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
; WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
; WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
; 
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Enterprise Integration Technologies,
; Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Enterprise Integration
; Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
; Betz make no representations about the suitability of this software for any
; purpose. It is provided "as is" without express or implied warranty.
; 
; ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
; LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
; SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
; IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
; COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
; INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
; OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; This variable is the default file to edit
;;;

(defvar *edit-file* "")

(defvar *editor* "eps")

;;;
;;; edit a file using the specified editor
;;; if the file editted was a lisp file (.lsp) load it
;;;

;; Two versions, the first works when position-if exists and does a better
;; job   

(if (fboundp 'position-if)
(defmacro edit (&optional file &aux rfile)
  (read-char)
  (when file (setq *edit-file* (string file)))
  (setq rfile (reverse *edit-file*))
  (when (null (position-if #'(lambda (x) (eq x #\.))
			   rfile
			   :end 
			   (position-if #'(lambda (x) 
						  (or (eq x #\\) (eq x #\/)))
					rfile)))
	(setq *edit-file* (strcat *edit-file* ".lsp")))
  (unless (system (strcat *editor* " " *edit-file*))
	  (error (strcat "Unable to execute: " *editor* " " *edit-file*)))
  (let ((len (length *edit-file*)))
       (when (and (> len 4)
		  (string= (string-downcase (subseq *edit-file* (- len 4)))
			   ".lsp"))
	     (list 'load *edit-file*))))

(defmacro edit (&optional file)
    (read-char)
    (when file (setq *edit-file* (string file)))
    (when (not (member #\.
		       (get-output-stream-list
			  (make-string-input-stream *edit-file*))))
	  (setq *edit-file* (strcat *edit-file* ".lsp")))
    (unless (system (strcat *editor* " " *edit-file*))
          (error (strcat "Unable to execute: " *editor* " " *edit-file*)))
    (let ((len (length *edit-file*)))
      (when (and (> len 4)
		 (string= (string-downcase (subseq *edit-file* (- len 4)))
			  ".lsp"))
	    (list 'load *edit-file*))))

)
