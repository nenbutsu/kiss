;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_cons.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; cons
(consp '(a . b))
(consp '(a b c))
(not (consp '()))
(not (consp #(a b)))

(equal (cons 'a '()) '(a))
(equal (cons '(a) '(b c d)) '((a) b c d))
(let ((l (cons "a" '(b c))))
  (and (string= (car l) "a")
       (equal (cdr l) '(b c))))
(equal (cons 'a 3) '(a . 3))
(equal (cons '(a b) 'c) '((a b) . c))

;; car
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(car '())))
(eq (car '(a b c)) 'a)
(equal (car '((a) b c d)) '(a))
(= (car '(1 . 2)) 1)
(eq (car '(a b c)) 'a)
(equal (car '((a) b c d)) '(a))

;; cdr
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(cdr '())))
(equal (cdr '((a) b c d)) '(b c d))
(eql (cdr '(1 . 2)) 2)

;; set-car
(let ((l (list 'a 'b)))
  (set-car 'x l)
  (equal l '(x b)))

(let* ((l (list 1 2))
       (c (set-car 2 l)))
  (eql c 2))

;; set-cdr
(let ((l (list 'a 'b)))
  (set-cdr (cons 'z nil) l)
  (equal  l '(a z)))

(let ((l (list 1 2))
      (l2 (list 3 4 5)))
  (eq (set-cdr l2 l) l2))

;; null
(not (null '(a b c)))
(null '())
(null 'nil)
(null (list))

;; listp
(listp '(a b c))
(listp '())
(listp '(a . b))
(not (listp "abc"))
(not (listp 9))
(not (listp 'list))

;; create-list
(equal (create-list 3 17) '(17 17 17))
(equal (create-list 2 #\a) '(#\a #\a))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(create-list 'a nil)))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(create-list -1 nil)))

;; list
(equal (list 'a (+ 3 4) 'c) '(a 7 c))
(eq (list) nil)

;; reverse
(equal (reverse '(a b c d e)) '(e d c b a))
(equal (reverse '(a)) '(a))
(eq (reverse '()) '())

;; nreverse
(equal (nreverse (list 'a 'b 'c 'd 'e)) '(e d c b a))
(equal (nreverse (list 'a)) '(a))
(eq (nreverse '()) '())
