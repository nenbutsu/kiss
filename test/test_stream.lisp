;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_stream.lisp --- a bunch of forms with which ISLisp processor must return true.

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


;;; streamp
(eq (streamp (standard-input)) t)
(eq (streamp (standard-output)) t)
(eq (streamp (error-output)) t)
(eq (streamp '()) nil)
(eq (streamp "love") nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (streamp))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (streamp (standard-input) 't))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (streamp (standard-input) 't 'nil))
  nil)


;;; open-stream-p
(member (open-stream-p (standard-input)) '(t nil))
(member (open-stream-p (standard-output)) '(t nil))
(member (open-stream-p (error-output)) '(t nil))
(eq (open-stream-p "abc") 'nil)
(eq (open-stream-p 'xyz) 'nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (open-stream-p))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (open-stream-p (standard-input) 't))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (open-stream-p (standard-input) 't 'nil))
  nil)


;;; input-stream-p
(eq (input-stream-p (standard-input)) t)
(eq (input-stream-p (standard-output)) nil)
(eq (input-stream-p '(a b c)) nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (input-stream-p))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (input-stream-p (standard-input) 't))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (input-stream-p (standard-input) 't 'nil))
  nil)


;;; output-stream-p
(eq (output-stream-p (standard-output)) t)
(eq (output-stream-p (standard-input)) nil)
(eq (output-stream-p "hello") nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (output-stream-p))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (output-stream-p (standard-input) 't))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (output-stream-p (standard-input) 't 'nil))
  nil)


;;; standard-input
(eq (input-stream-p (standard-input)) t)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (standard-input 't))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (standard-input 1 2))
  nil)


;;; standard-output
(eq (output-stream-p (standard-output)) t)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (standard-output 't))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (standard-output 1 2))
  nil)


;;; error-output
(eq (output-stream-p (error-output)) t)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (error-output 't))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (error-output 1 2))
  nil)


;;; with-standard-input
(equal (with-standard-input (create-string-input-stream "this is a string")
			    (list (read) (read)))
       '(this is))
(eq (with-standard-input (create-string-input-stream "this is a string"))
    'nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (with-standard-input))
  nil)


;;; with-standard-output
(let ((message nil))
  (with-standard-output (create-string-output-stream)
	(format (standard-output) "love me tender...")
	(setq message (get-output-stream-string (standard-output))))
  (string= message "love me tender..."))
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (with-standard-output))
  nil)


;;; with-error-output
(let ((message nil))
  (with-error-output (create-string-output-stream)
	(format (error-output) "love me tender...")
	(setq message (get-output-stream-string (error-output))))
  (string= message "love me tender..."))
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (with-error-output))
  nil)


;; with-open-output-file, with-open-input-file
(null (with-open-output-file (outstream "example.dat")
			     (format outstream "hello")))
(eq (with-open-input-file (instream "example.dat")
			  (read instream))
    'hello)

;; create-string-input-stream
(equal (let ((str (create-string-input-stream "this is a string")))
	 (list (read str) (read str) (read str)))
       '(this is a))

;; create-string-output-stream
(equal (let ((str (create-string-output-stream)))
	 (format str "hello")
	 (format str "world")
	 (get-output-stream-string str))
       "helloworld")

;; get-output-stream-string stream
(equal (let ((out-str (create-string-output-stream)))
	 (format out-str "This is a string")
	 (let ((part1 (get-output-stream-string out-str)))
	   (format out-str "right!")
	   (list part1 (get-output-stream-string out-str))))
       '("This is a string" "right!"))

;; read
(defglobal str (create-string-input-stream "hello #(1 2 3) 123 #\\A"))
(eq (read str) 'hello)
(equal (read str) #(1 2 3))
(eql (read str) 123)
(eql (read str) #\A)

;; read-char
(defglobal str (create-string-input-stream "hi"))
(eql (read-char str) #\h)
(eql (read-char str) #\i)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from top t)
		    (signal-condition condition nil)))
		(read-char str))
  nil)

;; preview-char
(equal (let ((s (create-string-input-stream "foo")))
	 (list (preview-char s) (read-char s) (read-char s)))
       '(#\f #\f #\o))


;; read-line
(null (with-open-output-file (out "newfile")
		       (format out "This is an example")
		       (format out "~%")
		       (format out "look at the output file")))
(defglobal str (open-input-file "newfile"))
(equal (read-line str) "This is an example")
(equal (read-line str) "look at the output file")


