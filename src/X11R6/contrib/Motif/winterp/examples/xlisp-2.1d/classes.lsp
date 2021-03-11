; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         classes.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/classes.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Useful stuff for object programming, including more standard
;		Lisp/OOP calls, such as DEFCLASS, DEFMETHOD, etc.
; Author:       Tom Almy, David Betz
; Created:      
; Modified:     Mon Jun  6 03:02:50 1994 (Niels Mayer) npm@indeed
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

; useful stuff for object programming

; filter certain keyword arguments for passing argument list to superclass

(defun remove-keys (keys list)
    (cond ((null keys) list)
	  ((null list) 'nil)
	  ((member (car list) keys)
	   (remove-keys (remove (car list) keys) (cddr list)))
	  (t (cons (car list) (remove-keys keys (cdr list))))))


; fix so that classes can be named (requires PNAME ivar in class Class)
;  The source files have been modified for PNAME instance variable,
;  and printing of class PNAME if it exists.

(send class :answer :set-pname
      '(name)
      '((setf pname (string name))))


; *setf* property of SEND is set to allow setting instance variables

(setf (get 'send '*setf*) 
      #'(lambda (obj ivar value) 
		(send obj :set-ivar (get ivar 'ivarname) value)))

; (defclass <classname> [(<instvars>) [(<classvars>) [<superclass>]]])
; defclass sets up access methods for all instance and class variables!
; an instance variable can be of form <ivar>  or (<ivar> <init>)
; :ISNEW is automatically defined to accept keyword arguments to overide
; default initialization.

(defmacro defclass (name &optional ivars cvars super 
			 &aux (sym (gensym)) (sym2 (gensym)))
; CIVAR is instance variable list with init values removed
    (let ((civars (mapcar #'(lambda (x) (if (consp x) (car x) x))
			  ivars)))

      `(progn ; Create class and assign to global variable
              (setf ,name
		    (send class :new
			  ',civars
			  ',cvars
			  ,@(if super (list super) nil)))

	      ; Set the name ivar of the class
	      (send ,name :set-pname ',name)

	      ; Generate the :<ivar> and :<cvar> methods
	      ,@(mapcar #'(lambda (arg)
			    `(send ,name
				   :answer
				   ,(intern (strcat ":" (string arg)))
				   'nil
				   '(,arg)))
		        (append civars cvars))

	      ; The method needed to set the instance variables
	      (send ,name :answer :set-ivar
		    '(,sym ,sym2)
		    '((case ,sym
			    ,@(mapcar #'(lambda (arg)
					       `(,arg (setq ,arg ,sym2)))
				      (append civars cvars))
			    (t (send-super :set-ivar ,sym ,sym2)))))

	      ; Set the ivarname property of the :<ivar> symbols
	      ,@(mapcar #'(lambda (arg)
	      		    `(setf (get ',(intern (strcat ":" (string arg)))
			    	        'ivarname)
				   ',arg))
		        civars)

	      ; Generate the :ISNEW method
	      (send ,name
		    :answer :isnew
		    '(&rest ,sym &key ,@ivars &allow-other-keys)

		    ; first :ISNEW setfs 
		    ;  for all its declared instance variables
		    '(,@(mapcar #'(lambda (arg)
				    `(setf (send self
					   	 ,(intern (strcat ":" 
						 	     (string arg))))
					   ,arg))
			        civars)

		      ; then the remaining initialization arguments are
		      ;  passed to the superclass.
		      (apply #'send-super
			     (cons ':isnew
				   (remove-keys
				      ',(mapcar #'(lambda (arg)
						    (intern (strcat ":"
							       (string arg))))
					        civars)
				      ,sym)))
		      self)))))


; (defmethod <class> <message> (<arglist>) <body>)

(defmacro defmethod (cls message arglist &rest body)
    `(send ,cls
	   :answer
	   ,message
	   ',arglist
	   ',body))

; (definst <class> <instname> [<args>...])

(defmacro definst (cls name &rest args)
    `(setf ,name
           (send ,cls
	         :new
		 ,@args)))

; (extensions suggested by Jim Ferrans)

(defun classp (name)
       (when (objectp name)
	     (eq (send name :class) class)))

(defmethod class :superclass () superclass)
(defmethod class :messages () messages)

(defmethod object :superclass () nil)

(defmethod object :ismemberof (cls)
	   (eq (send self :class) cls))

(defmethod object :iskindof (cls)
	   (do ((this (send self :class) (send this :superclass)))
	       ((or (null this)(eq this cls))
		(eq this cls))))

(defmethod object :respondsto (selector &aux temp)
	   (do ((this (send self :class) (send this :superclass)))
	       ((or (null this)
		    (setq temp 
			  (not (null (assoc selector 
				       (send this :messages))))))
		temp)
	       (setf temp nil)))


(defmethod class :ivars () ivars)

(defmethod class :pname () pname)

; :Storeon returns a list that can be executed to re-generate the object.
; It relies on the object's class being created using DEFCLASS,   so the
; instance variables can be generated.


(defmethod object :storeon (&aux cls ivlist res)
	   (setq cls
		 (send self :class)
		 ivlist
		 (do ((ivars (send cls :ivars)
			     (append (send super :ivars) ivars))
		      (super (send cls :superclass)
			     (send super :superclass)))
		     ((eq super object) ivars))
		 res
		 (mapcan #'(lambda (x) 
				   (let ((temp
					  (intern (strcat ":" (string x)))))
					(list temp
					      (let ((y (send self temp)))
						   (if (and y 
							    (or (symbolp y)
								(consp y)))
						       (list 'quote y)
						       y)))))
				   ivlist))
	   (append (list 'send (make-symbol (send cls :pname)) ':new)
		   res))

; For classes we must use a different tact.
; We will return a PROGN that uses SENDs to create the class and any methods.
; It also assumes the global environment. None of the DEFxxx functions
; are needed to do this.

; because of the subrs used in messages, :storeon cannot be  used to
; generate a reconstructable copy of classes Object and Class.

; Class variables are not set, because there are no class methods in XLISP
; to do this (one would have to create an instance, and send messages to
; the instance, and I feel that is going too far).


(defmethod class :storeon (&aux (classname (intern pname)))
   (nconc (list 'progn)
	  (list (list 'setq classname
		      (list 'send 'class :new ivars cvars 
			    (if superclass 
				(intern (send superclass :pname))
				nil))))
	  (list (list 'send classname :set-pname pname))
	  (mapcar #'(lambda (mess &aux 
				  (val (if (typep (cdr mess) 'closure)
					   (get-lambda-expression (cdr mess))
					   (list nil nil mess))))
			    (list 'send classname :answer
				  (first mess)
				  (list 'quote (cdadr val))
				  (list 'quote (cddr val))))
		  messages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; special hack for WINTERP so that :storeon method doesn't puke...
(defmethod object (intern ": _bogus_ivar_ ") (&rest args)
  )

(provide "xlisp-2.1d/classes")		;NPM: for WINTERP
