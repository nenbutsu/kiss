;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_vector.lisp --- a bunch of forms with which ISLisp processor must return true.

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


;;; basic-vector-p
(eq (basic-vector-p "abc") 't)
(eq (basic-vector-p "") 't)
(eq (basic-vector-p #(a b c)) 't)
(eq (basic-vector-p #()) 't)
(eq (basic-vector-p #1A(a b c)) 't)
(eq (basic-vector-p #2a((a) (b) (c))) 'nil)
(eq (basic-vector-p '(a b c)) 'nil)
(eq (basic-vector-p 'foo) 'nil)
(equal (mapcar (lambda (x)
                 (list (basic-vector-p x)
                       (general-vector-p x)))
               '((a b c)
                 "abc"
                 #(a b c)
                 #1a(a b c)
                 #2a((a) (b) (c))))
       '((nil nil) (t nil) (t t) (t t) (nil nil)))
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (basic-vector-p))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (basic-vector-p "love" "me"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (basic-vector-p "love" "me" "tender"))
  nil)


;;; general-vector-p
(eq (general-vector-p "abc") 'nil)
(eq (general-vector-p "") 'nil)
(eq (general-vector-p #(a b c)) 't)
(eq (general-vector-p #()) 't)
(eq (general-vector-p #1A(a b c)) 't)
(eq (general-vector-p #2a((a) (b) (c))) 'nil)
(eq (general-vector-p '(a b c)) 'nil)
(eq (general-vector-p 'foo) 'nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (general-vector-p))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (general-vector-p "love" "me"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (general-vector-p "love" "me" "tender"))
  nil)


;;; create-vector
(equal (create-vector 3 17) #(17 17 17))
(equal (create-vector 2 #\a) #(#\a #\a))
(equal (create-vector 0 #\a) #())
(= (length (create-vector 3)) 3)
(= (length (create-vector 0)) 0)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (create-vector))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (create-vector 3 'a 'b))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (create-vector 3 'a 'b 'c))
  nil)


;;; vector
(equal (vector) #())
(equal (vector 'a) #(a))
(equal (vector 'a 'b 10) #(a b 10))
