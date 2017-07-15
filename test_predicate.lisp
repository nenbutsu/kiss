;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_predicate.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; t
(eq t 't)
(symbolp t)

;; nil
(eq nil 'nil)
(eq nil '())
(eq (class-of 'nil) (class <null>))

;; eq eql
(eql () ())
(eq  () ())
(eql '() '())
(eq  '() '())
(eql 'a 'a)
(eq  'a 'a)
(eql 'a 'A)
(eq  'a 'A)
(not (eql 'a 'b))
(not (eq  'a 'b))
(not (eql 'f 'nil))
(not (eq  'f 'nil))
(eql 2 2)
(not (eql 2 2.0))
(not (eq  2 2.0))
(eql 100000000 100000000)
(eql 10.00000 10.0)
(not (eql (cons 1 2) (cons 1 2)))
(not (eq  (cons 1 2) (cons 1 2)))
(let ((x '(a))) (eql x x))
(let ((x '(a))) (eq x x))
(let ((p (lambda (x) x)))
  (eql p p))
(let ((p (lambda (x) x)))
  (eq p p))
(let ((x "a")) (eql x x))
(let ((x "a")) (eq x x))
(let ((x "")) (eql x x))
(let ((x "")) (eq x x))
(not (eql #\a #\A))
(not (eq #\a #\A))
(eql #\a #\a)
(eql #\space #\space)

;; equal
(equal 'a 'a)
(equal 2 2)
(not (equal 2 2.0))
(equal '(a) '(a))
(equal '(a (b) c) '(a (b) c))
(equal (cons 1 2) (cons 1 2))
(equal '(a) (list 'a))
(equal "abc" "abc")
;;(equal (vector 'a) (vector 'a))
(equal #(a b) #(a b))
(not (equal #(a b) #(a c)))
(not (equal "a" "A"))

;; not
(eq (not t) nil)
(eq (not '()) t)
(eq (not 'nil) t)
(eq (not nil) t)
(eq (not 3) nil)
(eq (not (list)) t)
(eq (not (list 3)) 'nil)

;; and
(eq (and) t)
(eq (and (= 2 2) (> 2 1)) t)
(eq (and (= 2 2) (< 2 1)) nil)
(eq (and (eql 'a 'a) (not (> 1 2))) t)
(eq (let ((x 'a)) (and x (setq x 'b))) 'b)
(eq (let ((x nil)) (and x (setq x 'b))) nil)
(eql (let ((time 10))
      (if (and (< time 24) (> time 12))
	  (- time 12) time))
     10)
(eql (let ((time 18))
       (if (and (< time 24) (> time 12))
	   (- time 12) time))
     6)

;; or
(eq (or (= 2 2) (> 2 1)) t)
(eq (or (= 2 2) (< 2 1)) t)
(eq (let ((x 'a)) (or x (setq x 'b))) 'a)
(eq (let ((x nil)) (or x (setq x 'b))) 'b)

