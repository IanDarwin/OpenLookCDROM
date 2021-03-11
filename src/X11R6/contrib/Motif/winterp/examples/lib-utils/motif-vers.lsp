; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         motif-vers.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-utils/RCS/motif-vers.lsp,v 2.1 1994/06/06 14:54:46 npm Exp $
; Description:  Define *MOTIF-1.0-P*, *MOTIF-1.1-OR-LATER-P*,
;		*MOTIF-1.1.3-OR-LATER-P*, *MOTIF-1.2-OR-LATER-P*
; Author:       Niels P. Mayer
; Created:      Sun Nov  7 16:27:17 1993
; Modified:     Mon Jun  6 00:43:11 1994 (Niels Mayer) npm@indeed
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

(defconstant *MOTIF-1.0-P*		;for conditionalizing against 1.0 braindamage....
  (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 0))
  )

(defconstant *MOTIF-1.1-OR-LATER-P*
  (not (and (eq *MOTIF_VERSION* 1) (eq *MOTIF_REVISION* 0)))
  )

(defconstant *MOTIF-1.1.3-OR-LATER-P*
  (or (> *MOTIF_VERSION* 1)		;Motif 2.X, 3.X, etc (???)
      (and (= *MOTIF_VERSION* 1) (> *MOTIF_REVISION* 1)) ;Motif >= 1.2
      (and (= *MOTIF_VERSION* 1) (= *MOTIF_REVISION* 1) (>= *MOTIF_SUBREVISION* 3))) ;Motif  >= 1.1.3
  )

(defconstant *MOTIF-1.2-OR-LATER-P*
  (and (>= *MOTIF_VERSION* 1) (>= *MOTIF_REVISION* 2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-utils/motif-vers")
