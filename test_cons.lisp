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

;; consp
(eq (consp '(a . b)) t)
(eq (consp '(a b c)) t)
(eq (consp '()) nil)
(eq (consp #(a b)) nil)
(eq (consp "love") nil)
(eq (consp #\x) nil)

;; cons
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
		(car '()))
  nil)
(eq (car '(a b c)) 'a)
(equal (car '((a) b c d)) '(a))
(= (car '(1 . 2)) 1)
(eq (car '(a b c)) 'a)
(equal (car '((a) b c d)) '(a))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(car "string"))
  nil)

;; cdr
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(cdr '()))
  nil)
(equal (cdr '((a) b c d)) '(b c d))
(eql (cdr '(1 . 2)) 2)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(cdr "string"))
  nil)

;; set-car
(let ((l (list 'a 'b)))
  (set-car 'x l)
  (equal l '(x b)))

(let* ((l (list 1 2))
       (c (set-car 2 l)))
  (eql c 2))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-car 0 "string"))
  nil)

;; set-cdr
(let ((l (list 'a 'b)))
  (set-cdr (cons 'z nil) l)
  (equal  l '(a z)))

(let ((l (list 1 2))
      (l2 (list 3 4 5)))
  (eq (set-cdr l2 l) l2))

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-cdr 0 "string"))
  nil)


;; null
(eq (null '(a b c)) nil)
(eq (null '()) t)
(eq (null 'nil) t)
(eq (null (list)) t)
(eq (null "") nil)
(eq (null #\z) nil)
(eq (null #()) nil)


;; listp
(eq (listp '(a b c)) t)
(eq (listp '()) t)
(eq (listp '(a . b)) t)
(eq (listp "abc") nil)
(eq (listp 9) nil)
(eq (listp 'list) nil)
(eq (listp #\z) nil)
(eq (listp #()) nil)


;; create-list
(equal (create-list 3 17) '(17 17 17))
(equal (create-list 2 #\a) '(#\a #\a))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(create-list 'a nil))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(create-list -1 nil))
  nil)
(eq (create-list 0) nil)
(eq (create-list 0 t) nil)

;; list
(equal (list 'a (+ 3 4) 'c) '(a 7 c))
(eq (list) nil)

;; reverse
(equal (reverse '(a b c d e)) '(e d c b a))
(equal (reverse '(a)) '(a))
(eq (reverse '()) '())
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(reverse "string"))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(reverse #(x y z)))
  nil)

;; nreverse
(equal (nreverse (list 'a 'b 'c 'd 'e)) '(e d c b a))
(equal (nreverse (list 'a)) '(a))
(eq (nreverse '()) '())
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(nreverse "string"))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(nreverse #(x y z)))
  nil)

;; append
(equal (append '(a b c) '(d e f)) '(a b c d e f))
(eq (append) '())
(eq (append '()) '())
(equal (append '(a b c) '() '(d e f)) '(a b c d e f))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(append '(a b) 'x))
  nil)

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(append 'z))
  nil)

;; member
(equal (member 'c '(a b c d e f)) '(c d e f))
(not (member 'g '(a b c d e f)))
(equal (member 'c '(a b c a b c)) '(c a b c))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(member 'a 'x))
  nil)


;; mapcar
(equal (mapcar #'car '((1 a) (2 b) (3 c))) '(1 2 3))
(equal (mapcar #'abs '(3 -4 2 -5 -6)) '(3 4 2 5 6))
(equal (mapcar #'cons '(a b c) '(1 2 3)) '((a . 1)(b . 2)(c . 3)))
(eq (mapcar (lambda (x y) (cons x y)) '() '(a b c)) nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcar 'not-a-function '(a b c) '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcar #'cons 'not-a-list '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcar #'cons))
  nil)

;; mapc
(eql (let ((x 0))
       (mapc (lambda (v) (setq x (+ x v))) '(3 5))
       x)
     8)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapc 'not-a-function '(a b c) '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapc #'cons 'not-a-list '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapc #'cons))
  nil)


;; maplist
(equal (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
       '((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))
(equal (maplist (lambda (x) (cons 'foo x)) '(a b c d))
       '((foo a b c d) (foo b c d) (foo c d) (foo d)))

(equal (maplist (lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c))
       '(0 0 1 0 1 1 1))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(maplist 'not-a-function '(a b c) '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(maplist #'cons 'not-a-list '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(maplist #'cons))
  nil)


;; mapl
(= (let ((k 0))
     (mapl (lambda (x)
	     (setq k (+ k (if (member (car x) (cdr x)) 0 1))))
	   '(a b a c d b c))
     k)
   4)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapl 'not-a-function '(a b c) '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapl #'cons 'not-a-list '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapl #'cons))
  nil)

;; mapcan
(equal (mapcan (lambda (x) (if (> x 0) (list x))) '(-3 4 0 5 -2 7))
       '(4 5 7))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcan 'not-a-function '(a b c) '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcan #'list 'not-a-list '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcan #'list))
  nil)

;; mapcon
(equal (mapcon (lambda (x) (if (member (car x) (cdr x)) (list (car x))))
	       '(a b a c d b c b c))
       '(a b c b c))
(equal (mapcon #'list '(1 2 3 4))
       '((1 2 3 4) (2 3 4) (3 4) (4)))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcon 'not-a-function '(a b c) '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcon #'list 'not-a-list '(x y z)))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(mapcon #'list))
  nil)

;; assoc
(equal (assoc 'a '((a . 1) (b . 2))) '(a . 1))
(equal (assoc 'a '((a . 1) (a . 2))) '(a . 1))
(eq (assoc 'c '((a . 1) (b . 2))) nil)

(equal (assoc #\a '((#\a . 1) (#\b . 2))) '(#\a . 1))
(equal (assoc #\a '((#\a . 1) (#\a . 2))) '(#\a . 1))
(eq (assoc #\c '((#\a . 1) (#\b . 2))) nil)

(equal (assoc 1 '((1 . 1) (2 . 2))) '(1 . 1))
(equal (assoc 1 '((1 . 1) (1 . 2))) '(1 . 1))


(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(assoc 'a 'not-a-list))
  nil)
