; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         hdwr.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/hdwr.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  A simple description of hardware objects using xlisp; Mix and
;		match instances of the objects to create your organization.
; Author:       Jwahar R. Bammi, Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 03:02:12 1994 (Niels Mayer) npm@indeed
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

; CONVERTED FOR 2.0, but untested.
; -*-Lisp-*-
;
; Jwahar R. Bammi
; A simple description of hardware objects using xlisp
; Mix and match instances of the objects to create your
; organization.
; Needs:
; - busses and connection and the Design
;   Class that will have the connections as instance vars.
; - Print method for each object, that will display
;   the instance variables in an human readable form.
; Some day I will complete it.
;
;
;
; utility functions


; function to calculate 2^n

(defun pow2 (n)
	(pow2x n 1))


(defun pow2x (n sum)
       (cond((equal n 0) sum)
	    (t (pow2x (- n 1) (* sum 2)))))


; hardware objects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The class areg

(setq areg (send Class :new '(value nbits max_val min_val)))

; methods

; initialization method
; when a new instance is called for the user supplies
; the parameter nbits, from which the max_val & min_val are derived

(send areg :answer :isnew '(n)
	  '((send self :init n)
	    	self))

(send areg :answer :init '(n)
	  '((setq value ())
	    (setq nbits n)
	    (setq max_val (- (pow2 (- n 1)) 1))
	    (setq min_val (- (- 0 max_val) 1))))

; load areg

(send areg :answer :load '(val)
    '((cond ((> val max_val) (princ (list "The max value a "nbits" bit register can hold is "max_val"\n")))
	    ((< val min_val) (princ (list "The min value a "nbits" bit register can hold is "min_val"\n")))
	    (t (setq value val)))))

; see areg

(send areg :answer :see '()
      '((cond ((null value) (princ "Register does not contain a value\n"))
	      (t value))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The class creg ( a register that can be cleared and incremented)
; subclass of a reg

(setq creg (send Class :new '() '() areg))

; it inherites all the instance vars & methods of a reg
; in addition to them it has the following methods

(send creg :answer :isnew '(n)
      '((send self :init n)
	self))

(send creg :answer :init '(n)
      '((setq value ())
	(setq nbits n)
	(setq max_val (- (pow2 n) 1))
	(setq min_val 0)))

(send creg :answer :clr '()
      '((setq value 0)))

(send creg :answer :inc '()
      '((cond ((null value) (princ "Register does not contain a value\n"))
	      (t (setq value (rem (+ value 1) (+ max_val 1)))))))

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Register bank
; contains n areg's n_bits each

(setq reg_bank (send Class :new '(regs n_regs curr_reg)))

;methods

(send reg_bank :answer :isnew '(n n_bits)
	  '((send self :init n n_bits)
	    self))

(send reg_bank :answer :init '(n n_bits)
	  '((setq regs ())
	    (setq n_regs (- n 1))
	    (send self :initx n n_bits)))

(send reg_bank :answer :initx '(n n_bits)
	  '((cond ((equal n 0) t)
	          (t (list (setq regs (cons (send areg :new n_bits) regs))
		  (send self :initx (setq n (- n 1)) n_bits))))))

(send reg_bank :answer :load '(reg val)
	  '((cond((> reg n_regs) (princ (list "Only "(+ 1 n_regs)" registers instantiated\n")))
		 (t (setq curr_reg (nth (+ reg 1) regs))
		    (curr_reg :load val)))))

(send reg_bank :answer :see '(reg)
	  '((cond((> reg n_regs) (princ (list "Only "(+ 1 n_regs)" registers instantiated\n")))
		 (t (setq curr_reg (nth (+ reg 1) regs))
		    (curr_reg :see)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Class alu

;alu - an n bit alu

(setq alu (send Class :new '(n_bits maxs_val mins_val maxu_val minu_val nf zf vf cf)))

; methods

(send alu :answer :isnew '(n)
     '((send self :init n)
       self))

(send alu :answer :init '(n)
     '((setq n_bits n)
       (setq maxu_val (- (pow2 n) 1))
       (setq maxs_val (- (pow2 (- n 1)) 1))
       (setq mins_val (- (- 0 maxs_val) 1))
       (setq minu_val 0)
       (setq nf 0)
       (setq zf 0)
       (setq vf 0)
       (setq cf 0)))

(send alu :answer :check_arith '(a b)
     '((cond ((and (send self :arith_range a) (send self :arith_range b)) t)
	     (t ()))))

(send alu :answer :check_logic '(a b)
     '((cond ((and (send self :logic_range a) (send self :logic_range b)) t)
	     (t ()))))

(send alu :answer :arith_range '(a)
     '((cond ((< a mins_val) (princ (list "Operand out of Range "a"\n")))
	     ((> a maxs_val) (princ (list "Operand out of range "a"\n")))
             (t t))))

(send alu :answer :logic_range '(a)
     '((cond ((< (abs a) minu_val) (princ (list "Operand out of Range "a"\n")))
             (t t))))

(send alu :answer :set_flags '(a b r)
     '((if (equal 0 r) ((setq zf 1)))
       (if (< r 0) ((setq nf 1)))
       (if (or (and (and (< a 0) (< 0 b)) (>= r 0))
		  (and (and (>= a 0) (>= b 0)) (< r 0))) ((setq vf 1)))
       (if (or (or (and (< a 0) (< b 0)) (and (< a 0) (>= r 0)))
		  (and (>= r 0) (< b 0))) ((setq cf 1)))))
       
(send alu :answer :add '(a b &aux result)
     '((cond ((null (send self :check_arith a b)) ())
	    (t (send self :clear_flags)
	       (setq result (+ a b))
	       (if (> result maxs_val) ((setq result (+ (- (rem result maxs_val) 1) mins_val))))
		   (if (< result mins_val) ((setq result (+ (rem result mins_val) (+ maxs_val 1)))))
	       (send self :set_flags a b result)
	       result))))

(send alu :answer :or '(a b &aux result)
     '((cond ((null (send self :check_logic a b)) ())
	    (t (send self :clear_flags)
	       (setq result (bit-ior a b))
	       (send self :set_flags a b result)
	       result))))

(send alu :answer :and '(a b &aux result)
     '((cond ((null (send self :check_logic a b)) ())
	    (t (send self :clear_flags)
	       (setq result (bit-and a b))
	       (send self :set_flags a b result)
	       result))))

(send alu :answer :not '(a  &aux result)
     '((cond ((null (send self :check_logic a 0)) ())
	    (t (send self :clear_flags)
	       (setq result (bit-not a))
	       (send self :set_flags a 0 result)
	       result))))	       

(send alu :answer :subtract '(a b)
     '((send self '+ a (- 0 b))))

(send alu :answer :passa '(a)
     '(a))

(send alu :answer :zero '()
     '(0))

(send alu :answer :com '(a)
     '((send self :- 0 a)))

(send alu :answer :status '()
     '((princ (list "NF "nf"\n"))
       (princ (list "ZF "zf"\n"))
       (princ (list "CF "cf"\n"))
       (princ (list "VF "vf"\n"))))

(send alu :answer :clear_flags '()
     '((setq nf 0)
       (setq zf 0)
       (setq cf 0)
       (setq vf 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The class Memory
;

(setq memory (send Class :new '(nabits ndbits maxu_val maxs_val mins_val max_addr undef memry)))

; methods

(send memory :answer :isnew '(addr_bits data_bits)
     '((send self :init addr_bits data_bits)
       self))

(send memory :answer :init '(addr_bits data_bits)
     '((setq nabits addr_bits)
       (setq ndbits data_bits)
       (setq maxu_val (- (pow2 data_bits) 1))
       (setq max_addr (- (pow2 addr_bits) 1))
       (setq maxs_val (- (pow2 (- data_bits 1)) 1))
       (setq mins_val (- 0 (pow2 (- data_bits 1))))
       (setq undef (+ maxu_val 1))
       (setq memry (array :new max_addr undef))))


(send memory :answer :load '(loc val)
     '((cond ((> (abs loc) max_addr) (princ (list "Address "loc" out of range\n")))
	     ((< val 0) (princ (list "Cant store "val" in "ndbits" bits\n")))
	     ((> val maxu_val) (princ (list "Cant store "val" in "ndbits" bits\n")))
	     (t (memry :load loc val)))))

(send memory :answer :write '(loc val)
     '((cond ((> (abs loc) max_addr) (princ (list "Address "loc" out of range\n")))
	     ((> val maxs_val) (princ (list "Cant store "val" in "ndbits" bits\n")))
	     ((< val mins_val) (princ (list "Cant store "val" in "ndbits" bits\n")))
	     (t (memry :load loc val)))))


(send memory :answer :read '(loc &aux val)
     '((cond ((> (abs loc) max_addr) (princ (list "Address "loc" out of range\n")))
	     (t (setq val (memry :see loc))
		(cond ((equal undef val) (princ (list "Address "loc" read before write\n")))
		      (t val))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The class array

(setq array (send Class :new '(arry)))

; methods

(send array :answer :isnew '(n val)
       '((send self :init n val)
	 self))

(send array :answer :init '(n val)
	'((cond ((< n 0) t)
	      (t (setq arry (cons val arry))
		 (send self :init (- n 1) val)))))

(send array :answer :see '(n)
	       '((nth (+ n 1) arry)))


(send array :answer :load '(n val &aux left right temp)
       '((setq left (send self :left_part n arry temp))
	 (setq right (send self :right_part n arry))
	 (setq arry (append left (list val)))
	 (setq arry (append arry right))
	 val))

(send array :answer :left_part '(n ary left)
       '((cond ((equal n 0) (reverse left))
	       (t (setq left (cons (car ary) left))
		  (send self :left_part (- n 1) (cdr ary) left)))))

(send array :answer :right_part '(n ary &aux right)
       '((cond ((equal n 0) (cdr ary))
	       (t (send self :right_part (- n 1) (cdr ary))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
