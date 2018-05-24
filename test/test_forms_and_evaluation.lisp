;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_forms_and_evaluation.lisp --- a bunch of forms with which ISLisp processor must return true.

;; Copyright (C) 2017, 2018 Yuji Minejima <yuji@minejima.jp>

;; This file is part of ISLisp processor KISS.

;; KISS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; KISS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; functionp
(functionp (function car))
(functionp (function create))
(functionp (lambda () t))
(not (functionp 'car))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                    (signal-condition condition nil)))
                (functionp))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                    (signal-condition condition nil)))
                (functionp 'car 'cdr))
  nil)


;;; function
(= (funcall (function -) 3) -3)
(= (apply #'- '(4 3)) 1)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                    (signal-condition condition nil)))
                (function))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                    (signal-condition condition nil)))
                (function a b))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <undefined-function>))
		      (return-from a t)
                    (signal-condition condition nil)))
                (function no-such-function))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
                    (signal-condition condition nil)))
                (function (love me do)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
                    (signal-condition condition nil)))
                (function "car"))
  nil)


;;; lambda
(= ((lambda (x y) (+ (* x x) (* y y))) 3 4) 25)
(equal ((lambda (x y &rest z) z) 3 4 5 6) '(5 6))
(equal ((lambda (x y :rest z) z) 3 4 5 6) '(5 6))
(= (funcall (lambda (x y) (- y (* x y))) 7 3) -18)


;;; labels
(eq (labels  ((evenp (n)
                     (if (= n 0)
                         t
                       (oddp (- n 1))))
              (oddp (n)
                    (if (= n 0)
                        nil
                      (evenp (- n 1)))))
      (evenp 88))
    t)


;;; flet
(= (flet ((f (x) (+ x 3)))
     (flet ((f (x) (+ x (f x))))
       (f 7)))
   17)


;;; apply
(= (apply (if (< 1 2) (function max) (function min)) 1 2 (list 3 4)) 4)
(defun compose (f g)
  (lambda (:rest args)
    (funcall f (apply g args))))
(= (funcall (compose (function sqrt) (function *)) 12 75) 30)


;;; funcall
(= (let ((x '(1 2 3)))
     (funcall (cond ((listp x) (function car))
                    (t (lambda (x) (cons x 1))))
              x))
   1)

