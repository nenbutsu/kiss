;;; -*- mode: lisp; coding: utf-8 -*- 
;;; control_l.lisp --- defines the control mechanism of ISLisp processor KISS.

;; Copyright (C) 2017, 2018 Yuji Minejima <yuji@minejima.jp>

;; This file is part of ISLisp processor KISS.

;; KISS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; KISS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; special operator: 
;; (for (iteration-spec*) (end-test result*) form*) -> <object> 
;;  iteration-spec ::= (var init [step])
(defmacro for (iteration-specs test-result-spec &rest body)
  (let ((tmp-var-inits nil))
    (flet ((make-inits (iteration-specs)
		       ;; in:  ((var1 init1 [step1]) (var2 init2 [step2]) ...)
		       ;; out: ((var1 init1) (var2 init2) ...)
		       (let ((inits nil)
			     (spec nil))
			 (while (consp iteration-specs)
			   (setq spec (car iteration-specs))
			   (setq inits `((,(car spec) ,(cadr spec)) ,@inits))
			   (setq iteration-specs (cdr iteration-specs)))
			 (nreverse inits)))
	   (make-steps (iteration-specs)
		       ;; in:  ((var1 init1) (var2 init2 step2) ...)
		       ;; note var1 doesn't have a step in this example
		       ;; out: ((setq var2 step2) ...) 
		       (let ((tmps nil)
			     (steps nil))
			 (while (consp iteration-specs)
			   (let ((spec (car iteration-specs))
				 (g (gensym)))
			     (if (consp (cddr spec)) ; if step exists
				 (progn
				   (setq tmp-var-inits `((,g nil) ,@tmp-var-inits))
				   (setq tmps `((setq ,g ,(caddr spec)) ,@tmps))
				   (setq steps `((setq ,(car spec) ,g) ,@steps)))))
			   (setq iteration-specs (cdr iteration-specs)))
			 `(,@(nreverse tmps) ,@(nreverse steps)))))
      (let ((inits (make-inits iteration-specs))
	    (steps (make-steps iteration-specs))
	    (test `(not ,(car test-result-spec)))
	    (results (cdr test-result-spec)))
	`(let (,@tmp-var-inits ,@inits)
	   (while ,test
	     ,@body
	     ,@steps)
	   ,@results)))))
