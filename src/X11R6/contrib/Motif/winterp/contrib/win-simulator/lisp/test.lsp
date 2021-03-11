;
; WINTERP Copyright 1989-1992 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and Niels Mayer not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and Niels Mayer
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;

(send *sim* :get-integer-registers)

(let ((li0 nil)
      (li1 nil))
  (send *sim* :SET-INTEGER-REGISTERS
	99
	(apply #'vector 
	       (dotimes 
		(i *num-registers* li0)
		(setq li0 (cons (random 100) li0))
		))
	(apply #'vector 
	       (dotimes 
		(i *num-registers* li1)
		(setq li1 (cons (random 100) li1))
		))))

(send *sim* :redisplay-integer-registers)

(send *sim* :visit-file "/etc/passwd")

(send *sim* :set-file-label "fooooo")

(send *sim* :set-curtime-label "fooooo")

(send *sim* :set-pc-label "fooooo")

(send *sim* :set-sim-status-display :run)

(send *sim* :get-sim-status-display)


(send *sim* :set-mem-trace
      (let ((li ()))
	(dotimes 
	 (i 1024 li)
	 (setq li
	       (cons
		(send Mem_Trace_Item_Class :new
		      i			;cycle
		      (if (= 0 (rem i 2)) #\R #\W) ;r/w
		      (random 1000000000) ;addrs
		      (random 65536)	;value
		      )
		li))
	 )))
