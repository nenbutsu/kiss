;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_control.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;;; constants
(equal #2A((a b c) (d e f)) '#2A((a b c) (d e f)))
(eql #\a '#\a)
(= 145932 '145932)
(string= "abc" '"abc")
(equal #(a b c) '#(a b c))
(char= #\space '#\space)
(char= #\newline '#\newline)
(= 0.12 '0.12)
(= 5.0 '5.0)
(= 123.0e10 '123.0E10)


;;; quote
(eq (quote a) 'a)
(equal (quote #(a b c)) '#(a b c))
(equal (quote (+ 1 2)) '(+ 1 2))
(eq '() nil)
(equal ''a '(quote a))
(equal '(car l) (quote (car l)))
(equal '(quote a) (quote (quote a)))
(eq (car ''a) 'quote)


;;; setq
(eq (defglobal x 2) 'x)
(= (+ x 1) 3)
(= (setq x 4) 4)
(= (+ x 1) 5)
(= (let ((x 1))
     (setq x 2)
     x)
   2)
(= (+ x 1) 5)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (setq))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (setq x))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (setq x 1 2 3))
  nil)


;;; let
(= (let () 1) 1)
(null (let ()))
(= (let ((x 2) (y 3)) (* x y)) 6)

(= (let ((x 2) (y 3))
     (let ((x 7)
           (z (+ x y)))
       (* z x)))
   35)

(equal (let ((x 1) (y 2))
         (let ((x y) (y x))
           (list x y)))
       '(2 1))

;;; let*
(= (let* () 1) 1)
(null (let* ()))
(eql (let ((x 2)
	   (y 3))
       (let* ((x 7)
	      (z (+ x y)))
	 (* z x)))
     70)

(equal (let ((x 1)
	     (y 2))
	 (let* ((x y)
		(y x))
	   (list x y)))
       '(2 2))


;;; dynamic
(defdynamic dynamic-foo 10)
(= (dynamic dynamic-foo) 10)
(dynamic-let ((dynamic-foo (- (dynamic dynamic-foo) 5)))
   (= (dynamic dynamic-foo) 5))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (dynamic))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (dynamic foo bar))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <unbound-variable>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (dynamic foo-bar-baz))
  nil)


;;; dynamic-let
(eq (defun foo (x)
      (dynamic-let ((y x))
                   (bar 1)))
    'foo)
(eq (defun bar (x)
      (+ x (dynamic y)))
    'bar)
(eql (foo 2) 3)


;;; if
(eq (if (> 3 2) 'yes 'no) 'yes)
(eq (if (> 2 3) 'yes 'no) 'no)
(eq (if (> 2 3) 'yes) nil)
(eql (if(> 3 2)
	 (- 3 2)
       (+ 3 2))
     1)
(eql (let ((x 7))
       (if (< x 0) x (- x)))
     -7)

;;; cond
(eq (cond ((> 3 2) 'greater)
	  ((< 3 2) 'less))
    'greater)
(eq (cond ((> 3 3) 'greater)
	  ((< 3 3) 'less))
    'nil)
(eq (cond ((> 3 3) 'greater)
	  ((< 3 3) 'less)
	  (t       'equal))
    'equal)
(eql (cond (10)) 10)
(eq (cond) 'nil)

;;; case case-using
(eq (case (* 2 3)
      ((2 3 5 7)
       'prime)
      ((4 6 8 9)
       'composite))
    'composite)
(eq (case (car '(c d))
      ((a) 'a)
      ((b) 'b))
    'nil)
(eq (case (car '(c d))
      ((a e i o u)
       'vowel)
      ((y) 'semivowel)
      (t 'consonant))
    'consonant)
(eq (let ((char #\u))
      (case char
	((#\a #\e #\o #\u #\i)
	 'vowels)
	(t 'consonants)))
    'vowels)

(eq (case-using #'= (+ 1.0 1.0)
		((1) 'one)
		((2) 'two)
		(t 'more))
    'two)
(eql (case-using #'string= "bar"
		 (("foo") 1)
		 (("bar") 2))
     2)


;;; progn
(and (eq (defglobal x 0) 'x)
     (eql (progn
	    (setq x 5)
	    (+ x 1))
	  6))

(eq (let ((out (create-string-output-stream)))
      (and (eq (progn
                 (format out "4 plus 1 equals ")
                 (format out "~D" (+ 4 1)))
               nil)
           (string= (get-output-stream-string out) "4 plus 1 equals 5")))
    t)

;;; while
(equal (let ((x '())
	     (i 5))
	 (while (> i 0) (setq x (cons i x)) (setq i (- i 1)))
	 x)
       '(1 2 3 4 5))

;;; for
(eql (let ((x '(1 3 5 7 9)))
       (for ((x x (cdr x))
	     (sum 0 (+ sum (car x))))
	    ((null x) sum)))
     25)

;;(equal (for ((vec (vector 0 0 0 0 0))
;;	     (i 0 (+ i 1)))
;;	    ((= i 5) vec)
;;	    (setf (elt vec i) i))
;;       #(0 1 2 3 4))

;;; block
(eql 6 (block x 
	 (+ 10 (return-from x 6) 22)))

(eq 'exit (labels ((f2 (g)
		       (funcall g))
		   (f1 ()
		       (block b
			 (let ((f (lambda () (return-from b 'exit))))
			   (f2 f)))))
	    (f1)))

(progn
  (defun f1 ()
    (block b
      (let ((f (lambda () (return-from b 'exit))))
	;; big computation
	(f2 f))))

  (defun f2 (g)
    ;; big computation
    (funcall g))

  (eq (f1) 'exit))

(eql (block sum-block
       (for ((x '(1 a 2 3) (cdr x))
	     (sum 0 (+ sum (car x))))
	    ((null x) sum)
	    (cond ((not (numberp (car x))) (return-from sum-block 0)))))
     0)

(defun bar (x y)
  (let ((foo #'car))
    (let ((result
	   (block bl
	     (setq foo (lambda () (return-from bl 'first-exit)))
	     (if x (return-from bl 'second-exit) 'third-exit))))
      (if y (funcall foo) nil)
      result)))

(eq (bar t nil) 'second-exit)
(eq (bar nil nil) 'third-exit)
(block a
  (with-handler (lambda (c)
		  (if (instancep c (class <error>))
		      (return-from a t)
		    (signal-condition c nil)))
		(bar nil t))
  nil)

(block a
  (with-handler (lambda (c)
		  (if (instancep c (class <error>))
		      (return-from a t)
		    (signal-condition c nil)))
		(bar t t))
  nil)



		
;;; catch throw
(defun foo (x)
  (catch 'block-sum (bar x)))

(defun bar (x)
  (for ((l x (cdr l))
	(sum 0 (+ sum (car l))))
       ((null l) sum)
       (cond ((not (numberp (car l)))
	      (throw 'block-sum 0)))))

(eql (foo '(1 2 3 4)) 10)
(eql (foo '(1 2 a 4)) 0)

;;; tagbody go

;;; unwind-protect
(defun foo (x)
  (catch 'duplicates
    (unwind-protect (bar x)
      (for ((l x (cdr l)))
	   ((null l) 'unused)
	   (remove-property (car l) 'label)))))

(defun bar (l)
  (cond
   ((and (symbolp l) (property l 'label))
    (throw 'duplicates 'found))
   ((symbolp l) (set-property t l 'label))
   ((bar (car l)) (bar (cdr l)))
   (t nil)))

(foo '(a b c))
(null (property 'a 'label))
(eq (foo '(a b a c)) 'found)
(null (property 'a 'label))

(defun test ()
  (catch 'outer (test2)))

(defun test2 ()
  (block inner
    (test3 (lambda ()
	     (return-from inner 7)))))


(defun test3 (fun)
  (unwind-protect (test4) (funcall fun)))

(defun test4 ()
  (throw 'outer 6))

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(test))
  nil)
