;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_array.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; basic-array-p, ...
(equal (mapcar (lambda (x)
		 (list (basic-array-p x)
		       (basic-array*-p x)
		       (general-array*-p x)))
	       '(#\z
		 (a b c)
		 "abc"
		 #(a b c)
		 #1a(a b c)
		 #2a((a) (b) (c))))
       '((nil nil nil) (nil nil nil) (t nil nil) (t nil nil) (t nil nil) (t t t)))

;; create-array
(equal (create-array '(2 3) 0.0)
       #2a((0.0 0.0 0.0) (0.0 0.0 0.0)))

(equal (create-array '(2) 0.0) #(0.0 0.0))

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(create-array '(-1)))
  nil)

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(create-array '(3 -1)))
  nil)

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(create-array 'a))
  nil)

;; aref
(defglobal array1 (create-array '(3 3 3) 0))
(equal array1 #3a(((0 0 0) (0 0 0) (0 0 0))
		  ((0 0 0) (0 0 0) (0 0 0))
		  ((0 0 0) (0 0 0) (0 0 0))))
(eql (aref array1 0 1 2) 0)
(eql (set-aref 3.14 array1 0 1 2) 3.14)
(eql (aref array1 0 1 2) 3.14)
(eql (set-aref 51.3 array1 0 1 2) 51.3)
(eql (aref array1 0 1 2) 51.3)
(eql (aref (create-array '(8 8) 6) 1 1) 6)
(eql (aref (create-array '() 19)) 19)
(eql (aref "abc" 1) #\b)
(eql (aref #(a b c) 1) 'b)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(aref 'a 0 0))
  nil)


;; garef
(defglobal array1 (create-array '(3 3 3) 0))
(equal array1 #3a(((0 0 0) (0 0 0) (0 0 0))
		  ((0 0 0) (0 0 0) (0 0 0))
		  ((0 0 0) (0 0 0) (0 0 0))))
(eql (garef array1 0 1 2) 0)
(eql (set-garef 3.14 array1 0 1 2) 3.14)
(eql (garef array1 0 1 2) 3.14)
(eql (set-garef 51.3 array1 0 1 2) 51.3)
(eql (garef array1 0 1 2) 51.3)
(eql (garef (create-array '(8 8) 6) 1 1) 6)
(eql (garef (create-array '() 19)) 19)
(eql (garef #(a b c) 1) 'b)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(garef "abc" 0))
  nil)


;; array-dimensions
(equal (array-dimensions (create-array '(2 2) 0)) '(2 2))
(equal (array-dimensions (vector 'a 'b)) '(2))
(equal (array-dimensions "foo") '(3))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(array-dimensions 'a))
  nil)
