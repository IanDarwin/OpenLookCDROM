; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xmu-cache.lsp
; RCS:          $Header: $
; Description:  initialize the WINTERP menu server cache
; Author:       Richard Hess, Consilium
; Created:      Sun Oct  6 00:00:11 1991
; Modified:     Sun Oct  6 00:03:32 1991 (Niels Mayer) mayer@hplnpm
; Language:     Lisp
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; +---------------------------------------------------------------------------
;  WHO:    Richard Hess                    CORP:   Consilium
;  TITLE:  Staff Engineer                  VOICE:  [415] 691-6342
;      [ X-SWAT Team:  Special Projects ]  USNAIL: 640 Clyde Court
;  UUCP:   ...!uunet!cimshop!rhess                 Mountain View, CA 94043
; +---------------------------------------------------------------------------

(setq *xmu_def1* '("BSD"
		   "OSF-1"
		   "SVr4"
		   nil
		   ("MIPS"	("DS-3100"
				 "DS-5000"
				 ("SGI 4D/35" "Cool stuff...")
				 nil
				 ("Stuff" ())  
				 ))
		   ("NeXT"	("NeXTstation"
				 "NeXTstation Color"
				 "NeXTcube"
				 nil
				 "NeXTdimension"
				 ))
		   ("PA-RISC"	("Series 720"
				 "Series 730"
				 "Series 750"
				 ))
		   ("Sparc"	("SLC"
				 "IPC"
				 "Station"
				 "Server"
				 ))
		   nil
		   ("Xterms"	("NCD"
				 "TekXpress"
				 ("VT1200" "turtle...")
				 ))
		   ))

(setq *xmu_def2* '("BSD"
		   "OSF-1"
		   "SVr4"
		   nil
		   ("Sparc"	("SLC"
				 "IPC"
				 "Station"
				 "Server"
				 ))
		   ("MIPS"	("DS-3100"
                                 ("DS-5000" "Hot")
                                 ("SGI 4D/35" "Cool stuff...")
                                 nil
                                 ("Stuff" ())
                                 ))
		   "NeXT"
		   nil
		   ("Xterms"	("NCD"
				 "TekXpress"
				 ("VT1200" "turtle...")
				 ))
		   ))

(Xmu_Menu 888 "UNIX" *xmu_def1* "::Xmu [ demo1 ]")
(Xmu_Menu 890 "UNIX" *xmu_def2* "::Xmu [ demo2 ]")

; -----------------------------------------------------------------------<eof>
