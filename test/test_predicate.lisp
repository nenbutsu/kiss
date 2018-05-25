;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_predicate.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;;; https://nenbutsu.github.io/ISLispHyperDraft/islisp-v23.html#predicates

;;; t
t
(eq t 't)
(symbolp t)


;; nil
(not nil)
(eq nil 'nil)
(eq nil '())
(symbolp 'nil)
(eq (class-of 'nil) (class <null>))


;;; eq eql
(eq (eql () ()) t)
(eq (eq  () ()) t)
(eq (eql '() '()) t)
(eq (eq  '() '()) t)
(eq (eql 'a 'a) t)
(eq (eq  'a 'a) t)
(eq (eql 'a 'A) t)
(eq (eq  'a 'A) t)
(not (eql 'a 'b))
(not (eq  'a 'b))
(not (eql 'f 'nil))
(not (eq  'f 'nil))
(eq (eql 2 2) t)
(not (eql 2 2.0))
(not (eq  2 2.0))
(eq (eql 10000 10000) t)
(eq (eql 10.00000 10.0) t)
(not (eql (cons 1 2) (cons 1 2)))
(not (eq  (cons 1 2) (cons 1 2)))
(let ((x '(a))) (eq (eql x x) t))
(let ((x '(a))) (eq (eq x x) t))
(let ((p (lambda (x) x)))
  (eq (eql p p) t))
(let ((p (lambda (x) x)))
  (eq (eq p p) t))
(let ((x "a")) (eq (eql x x) t))
(let ((x "a")) (eq (eq x x) t))
(let ((x "")) (eq (eql x x) t))
(let ((x "")) (eq (eq x x) t))
(not (eql #\a #\A))
(not (eq #\a #\A))
(eq (eql #\a #\a) t)
(eq (eql #\space #\space) t)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (eq))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (eq 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (eq 'a 'b 'c))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (eql))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (eql 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (eql 'a 'b 'c))
  nil)


;;; equal
(eq (equal 'a 'a) t)
(eq (equal 2 2) t)
(not (equal 2 2.0))
(eq (equal '(a) '(a)) t)
(eq (equal '(a (b) c) '(a (b) c)) t)
(eq (equal (cons 1 2) (cons 1 2)) t)
(eq (equal '(a) (list 'a)) t)
(eq (equal "abc" "abc") t)
(eq (equal (vector 'a) (vector 'a)) t)
(eq (equal #(a b) #(a b)) t)
(eq (equal #2a((1 2 3) (4 5 6)) #2a((1 2 3) (4 5 6))) t)
(not (equal #2a((1 2 3) (4 5 6)) #2a((1 2 3) (7 8 9))))
(not (equal #(a b) #(a c)))
(not (equal "a" "A"))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (equal))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (equal 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (equal 'a 'b 'c))
  nil)


;;; not
(eq (not t) nil)
(eq (not '()) t)
(eq (not 'nil) t)
(eq (not nil) t)
(eq (not 3) nil)
(eq (not (list)) t)
(eq (not (list 3)) 'nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (not))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (not 'a 'b 'c))
  nil)


;;; and
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


;;; or
(eq (or) 'nil)
(eq (or 'a) 'a)
(eq (or (= 2 2) (> 2 1)) t)
(eq (or (= 2 2) (< 2 1)) t)
(eq (let ((x 'a)) (or x (setq x 'b))) 'a)
(eq (let ((x nil)) (or x (setq x 'b))) 'b)

