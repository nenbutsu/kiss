;;; -*- mode: lisp; coding: utf-8 -*- 
;;; control.lisp --- defines the control mechanism of ISLisp processor KISS.

;; Copyright (C) 2017 Yuji Minejima.

;; This file is part of ISLisp processor KISS.

;; KISS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; KISS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; function: (not obj) -> boolean
;; This predicate is the logical “not” (or “¬”). It returns t
;; if obj is nil and nil otherwise. obj may be any ISLISP object.
(defun not (obj) (if (eq obj nil) t nil))

;; special operator: (and form*) -> <object>
;; and is the sequential logical “and” (or “∧”). forms are evaluated
;; from left to right until either one of them evaluates to nil or else
;; none are left. If one of them evaluates to nil, then nil is returned
;; from the and; otherwise, the value of the last evaluated form is returned.
(defmacro and (&rest args)
  (if (eq args nil)                     ; (and) ≡ ’t
      'T
    (if (eq (cdr args) nil)             ; (and form) ≡ form
        (car args)
      ;; (and form1 form2 . . . formn)≡(if form1 (and form2 . . . formn) ’nil)
      `(if ,(car args) (and ,@(cdr args)) 'nil))))


;; special operator: (or form*) -> <object>
;; or is the sequential logical “or” (or “∨”). forms are evaluated from
;; left to right until either one of them evaluates to a non-nil value or
;; else none are left. If one of them evaluates to a non-nil value,
;; then this non-nil value is returned, otherwise nil is returned.
(defmacro or (&rest args)
  (cond
   ((eq args nil)                       ; (or) ≡ ’nil
    'nil)
   ((eq (cdr args) nil)                 ; (or form) ≡ form
    (car args))
   (t (let ((var (gensym)))
        ;; (or form1 form2 . . . formn) ≡ 
        ;; ((lambda (var) (if var var (or form2 . . . formn))) form1)
        ;; where var does not occur in form2 . . . formn
        `((lambda (,var) (if ,var ,var (or ,@(cdr args)))) ,(car args))))))


;; special operator: (cond (test form*)*) -> <object>
;; Executing the prepared cond, the clauses (test form*) are scanned
;; sequentially and in each case the test is evaluated; when a test delivers
;; a non-nil value the scanning process stops and all forms associated
;; with the corresponding clause are sequentially evaluated and the value
;; of the last one is returned. If no test is true, then nil is returned.
;; If no form exists for the successful test then the value of this test
;; is returned.
(defmacro cond (&rest args)
  (if (eq args nil)                     
      ;; (cond) ≡ nil
      nil
    (let ((clause1 (car args))
          (rest (cdr args)))
      (if (eq (cdr clause1) nil)
          ;; (cond (test1)         ≡ (or test1
          ;;       (test2 form2*)        (cond (test2 form2*)
          ;;          ...)                   ...))
        `(or ,(car clause1)
             (cond ,@rest))
        (let ((test1 (car clause1))
              (forms1 (cdr clause1)))
          ;; (cond (test1 form+1)  ≡ (if test1
          ;;       (test2 form2*)        (progn form+1)
          ;;          ...)                  (cond (test2 form2*)
          ;;                                    ...))
          `(if ,test1
               (progn ,@forms1)
             (cond ,@rest)))))))

;; special operator: (while test-form body-form*) -> <null>
;; Iterates while the test-form returns a true value. Specifically:
;;   1. test-form is evaluated, producing a value Vt.
;;   2. If Vt is nil, then the while form immediately returns nil.
;;   3. Otherwise, if Vt is non-nil, the forms body-form* are evaluated 
;;      sequentially (from left to right).
;;   4. Upon successful completion of the body-forms*, the while form begins
;;       again with step 1.
(defmacro while (test-form &rest body)
  (let ((begin (gensym))
        (end (gensym)))
    `(tagbody
      ,begin
      (if (eq ,test-form nil) (go ,end))
      ,@body
      (go ,begin)
      ,end)))

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


;; special operator: (case keyform ((key*) form*)* [(t form*)]) -> <object>
(defmacro case (keyform &rest clauses)
  `(case-using #'eql ,keyform
     ,@clauses))

;; special operator: (case-using predform keyform ((key*) form*)* [(t form*)])
;;  -> <object>
(defmacro case-using (predform keyform &rest clauses)
  ;; clauses := (((key*) form*)* [(t form*)])
  (cond
   ((eq clauses nil)
    ;; (case-using predform keyform)
    `(progn ,predform ,keyform nil))
   ((eq (car (car clauses)) t)
    ;; (case-using predform keyform (t form*))
    `(progn ,predform ,keyform ,@(cdr (car clauses))))
   (t
    ;; (case-using predform keyform ((key*) form*) ((key*) form*)* [(t form*)])
    ;;                              ^^^^^^^^^^^^^^
    ;;                                clause1
    (flet ((make-test (pred var keys)
             (let ((test nil))
               (while (consp keys)
                 (setq test `((funcall ,pred ,var (quote ,(car keys))) ,@test))
                 (setq keys (cdr keys)))
               `(or ,@(nreverse test)))))
      (let* ((pred (gensym))
             (var (gensym))
             (clause1 (car clauses))
             (test (make-test pred var (car clause1))))
        `(let ((,pred ,predform)
               (,var ,keyform))
           (if ,test
               (progn ,@(cdr clause1))
             (case-using ,pred ,var
                ,@(cdr clauses)))))))))

;; Emacs lisp
;; macro: when condition then-forms...
;; This is a variant of `if' where there are no ELSE-FORMS, and
;; possibly several THEN-FORMS.  In particular,
;;     (when CONDITION A B C)
;; is entirely equivalent to
;;     (if CONDITION (progn A B C) nil)
;;(defmacro when (condition &rest then-forms)
;;  `(if ,condition
;;       (progn ,@then-forms)
;;     nil))


;; function: (equal obj1 obj2) -> boolean
(defun equal (obj1 obj2)
  (cond
   ((and (null obj1) (null obj2)) t)
   ((and (consp obj1) (consp obj2))
    (if (and (equal (car obj1) (car obj2))
             (equal (cdr obj1) (cdr obj2)))
        t
      nil))
   ((and (stringp obj1) (stringp obj2))
    (if (string= obj1 obj2)
	t
      nil))
   ((and (general-vector-p obj1) (general-vector-p obj2))
    (if (= (length obj1) (length obj2))
	(let ((i 0))
	  (while (< i (length obj1))
	    (if (not (eql (elt obj1 i) (elt obj2 i)))
		(return-from equal nil))
	    (setq i (+ i 1)))
	  t)
      nil))
   ((and (general-array*-p obj1) (general-array*-p obj2)
	 (equal (array-dimensions obj1) (array-dimensions obj2)))
    (if (equal (kiss::general-array*-to-list obj1) (kiss::general-array*-to-list obj2))
	t
      nil))
   (t (eql obj1 obj2))))

(defmacro prog1 (first-form &rest forms)
  (let ((result-var (gensym)))
    `(let ((,result-var ,first-form))
       ,@forms
       ,result-var)))
