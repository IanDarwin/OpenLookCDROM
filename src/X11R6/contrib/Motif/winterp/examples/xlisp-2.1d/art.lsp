; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         art.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/art.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  OOP example
; Author:       Tom Almy and/or David Betz
; Created:      Mon Jun  6 01:31:03 1994
; Modified:     Mon Jun  6 03:03:04 1994 (Niels Mayer) npm@indeed
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

; This is an example using the object-oriented programming support in
; XLISP.  The example involves defining a class of objects representing
; dictionaries.  Each instance of this class will be a dictionary in
; which names and values can be stored.  There will also be a facility
; for finding the values associated with names after they have been
; stored.

; Create the 'Dictionary' class and establish its instance variable list.
; The variable 'entries' will point to an association list representing the
; entries in the dictionary instance.

(setq Dictionary (send Class :new '(entries)))

; Setup the method for the ':isnew' initialization message.
; This message will be send whenever a new instance of the 'Dictionary'
; class is created.  Its purpose is to allow the new instance to be
; initialized before any other messages are sent to it.  It sets the value
; of 'entries' to nil to indicate that the dictionary is empty.

(send Dictionary :answer :isnew '()
	    '((setq entries nil)
	      self))

; Define the message ':add' to make a new entry in the dictionary.  This
; message takes two arguments.  The argument 'name' specifies the name
; of the new entry; the argument 'value' specifies the value to be
; associated with that name.

(send Dictionary :answer :add '(name value)
	    '((setq entries
	            (cons (cons name value) entries))
	      value))

; Create an instance of the 'Dictionary' class.  This instance is an empty
; dictionary to which words may be added.

(setq d (send Dictionary :new))

; Add some entries to the new dictionary.

(send d :add 'mozart 'composer)
(send d :add 'winston 'computer-scientist)

; Define a message to find entries in a dictionary.  This message takes
; one argument 'name' which specifies the name of the entry for which to
; search.  It returns the value associated with the entry if one is
; present in the dictionary.  Otherwise, it returns nil.

(send Dictionary :answer :find '(name &aux entry)
	    '((cond ((setq entry (assoc name entries))
	      (cdr entry))
	     (t
	      nil))))

; Try to find some entries in the dictionary we created.

(send d :find 'mozart)
(send d :find 'winston)
(send d :find 'bozo)

; The names 'mozart' and 'winston' are found in the dictionary so their
; values 'composer' and 'computer-scientist' are returned.  The name 'bozo'
; is not found so nil is returned in this case.
 
