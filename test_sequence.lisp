;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_sequence.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; length
(eql (length '(a b c)) 3)
(eql (length '(a (b) (c d e))) 3)
(eql (length '()) 0)
(eql (length #()) 0)
(eql (length "") 0)
(eql (length (vector 'a 'b 'c)) 3)
(eql (length '(a b . c)) 2)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(length 'not-a-sequence))
  nil)




;; elt
(eq (elt '(a b c) 2) 'c)
(eq (elt (vector 'a 'b 'c) 1) 'b)
(eql (elt "abc" 0) #\a)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(elt "abc" -1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(elt "abc" 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(elt "" 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(elt #() 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(elt '() 0))
  nil)

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(elt 'not-a-sequence 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(elt "abc" 'x))
  nil)


;; set-elt
(string= (let ((string (create-string 5 #\x)))
	   (set-elt #\O string 2)
	   string)
	 "xxOxx")

(equal (let ((x (create-list 3 'x)))
	 (set-elt 'y x 1)
	 x)
       '(x y x))

(equal (let ((x (vector 'a 'b 'c)))
	 (set-elt 'z x 1)
	 x)
       '#(a z c))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-elt 'x 'not-a-sequence 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-elt 'x (list 'a 'b 'c) 'z))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-elt 'z (list 'a 'b 'c) -1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-elt 'z (list 'a 'b 'c) 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-elt 'z (list) 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-elt 'z (vector) 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(set-elt 'z (create-string 0) 0))
  nil)


;; subseq
(equal (subseq "abcdef" 1 4) "bcd")
(equal (subseq '(a b c d e f) 1 4) '(b c d))
(equal (subseq (vector 'a 'b 'c 'd 'e 'f) 1 4) #(b c d))

(equal (subseq "" 0 0) "")
(equal (subseq #() 0 0) #())
(equal (subseq '() 0 0) nil)

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(subseq 'not-a-sequence 0 0))
  nil)

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(subseq #(a b c) 'z 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(subseq #(a b c) 0 'z))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(subseq #(a b c) 1 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(subseq #(a b c) 0 100))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(subseq #(a b c) -1 1))
  nil)

;; map-into
(equal (let ((a (list 1 2 3 4))
	     (b (list 10 10 10 10)))
	 (map-into a #'+ a b))
       '(11 12 13 14))

(equal (let ((a (list 11 12 13 14))
	     (k (list 'one 'two 'three)))
	 (map-into a #'cons k a))
       '((one . 11) (two . 12) (three . 13) 14))
(equal (let ((a (list '(one . 11) '(two . 12) '(three . 13) 14))
	     (x 0))
	 (map-into a (lambda () (setq x (+ x 2)))))
       '(2 4 6 8))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(map-into 'not-a-sequence (lambda (x) (+ x 1)) (list 0 1 2 3))))
