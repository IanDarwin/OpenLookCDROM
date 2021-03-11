; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         hashtable.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-utils/RCS/hashtable.lsp,v 2.1 1994/06/06 14:54:46 npm Exp $
; Description:  Hash_Table_Class -- initialize a hash table, add entries,
;		or find entries in the hash table.
; Author:       Niels P. Mayer
; Created:      Mon Jan 17 01:46:31 1994
; Modified:     Mon Jun  6 00:39:07 1994 (Niels Mayer) npm@indeed
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash_Table_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Hash_Table_Class 
      (send Class :new 
	    '(
	      hashtab_size
	      hashtab
	      equality_test_fn
	      )
	    '()
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;; (send Hash_Table_Class :new <size> [:test <test-fn>])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Hash_Table_Class :answer :ISNEW
      '(size_fixnum
	&key ((:test k_test) NIL)
	)
      '(
	(setq hashtab_size size_fixnum)
	(setq hashtab (make-array size_fixnum))
	(setq equality_test_fn (if k_test k_test #'equal))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (send hashtab :ADD-ASSOC <hash_obj> <assoc>)
;; 
;; this may be called a few times, s.t. :FIND-ASSOC will return
;; (<assoc1> <assoc2> ... <assocN>), where <assoc-i> are set by 
;; successive calls to (send <hashtab> :add
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Hash_Table_Class :answer :ADD-ASSOC
      '(hash_obj assoc)
      '(
	(let* ((hashidx      (hash hash_obj hashtab_size))
	       (hbucket_list (aref hashtab hashidx))
	       (hashmatch    (assoc hash_obj hbucket_list :test equality_test_fn))
	       )
	  (if hashmatch
	      (nconc hashmatch (list assoc)) ;add on new assoc to existing assoc-list entry...
	    (setf (aref hashtab hashidx)
		  (cons (list hash_obj assoc) hbucket_list))
	    )
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (send <hashtab> :find-assoc <hash_obj>)
;; --> returns (<assoc1> <assoc2> ... <assocN>)
;; where <assoc-i> are set through successive calls to :ADD-ASSOC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Hash_Table_Class :answer :FIND-ASSOC
      '(hash_obj)
      '(
	(let* ((hbucket_list (aref hashtab (hash hash_obj hashtab_size)))
	       (hashmatch    (assoc hash_obj hbucket_list :test equality_test_fn))
	       )
	  (if hashmatch
	      (cdr hashmatch)		;RETURN (<assoc1> <assoc2> ... <assocN>)
	    NIL				;RETURN NIL if no match....
	    )
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-utils/hashtable")
