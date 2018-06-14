;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_string.lisp --- a bunch of forms with which ISLisp processor must return true.

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


;;; stringp
(eq (stringp "abc") 't)
(eq (stringp 'abc) 'nil)
(eq (stringp #()) 'nil)
(eq (stringp #2A((a) (b))) 'nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (stringp))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (stringp "abc" "xyz"))
  nil)


;; create-string
(equal (create-string 3 #\a) "aaa")
(equal (create-string 0 #\a) "")
(defglobal str (create-string 5))
(stringp str)
(= (length str) 5)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (create-string))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (create-string 3 #\a #\b))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (create-string -3 #\a))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (create-string -5))
  nil)


;;; string=
(eq (if (string= "abcd" "abcd") 't 'nil) 't)
(eq (if (string= "" "") 't 'nil) 't)
(eq (if (string= "love me tender" "love me tender") 't 'nil) 't)
(eq (if (string= "abc" "abcd") 't 'nil) 'nil)
(eq (if (string= "abcd" "abc") 't 'nil) 'nil)
(eq (if (string= "" "a") 't 'nil) 'nil)
(eq (if (string= "abcd" "wxyz") 't 'nil) 'nil)
(eq (if (string= "abcd" "abcde") 't 'nil) 'nil)
(eq (if (string= "abcde" "abcd") 't 'nil) 'nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string=))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string= "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string= "abc" "xyz" "foo"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string= "abc" 'abc))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string= 'abc "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string= 'abc 'xyz))
  nil)


;;; string/=
(eq (if (string/= "abcd" "abcd") 't 'nil) 'nil)
(eq (if (string/= "" "") 't 'nil) 'nil)
(eq (if (string/= "abc" "ABC") 't 'nil) 't)
(eq (if (string/= "abc" "abcd") 't 'nil) 't)
(eq (if (string/= "abcd" "abc") 't 'nil) 't)
(eq (if (string/= "" "abc") 't 'nil) 't)
(eq (if (string/= "abcd" "wxyz") 't 'nil) 't)
(eq (if (string/= "abcd" "abcde") 't 'nil) 't)
(eq (if (string/= "abcde" "abcd") 't 'nil) 't)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string/=))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string/= "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string/= "abc" "xyz" "foo"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string/= "abc" 'abc))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string/= 'abc "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string/= 'abc 'xyz))
  nil)


;;; string<
(eq (if (string< "abcd" "abcd") 't 'nil) 'nil)
(eq (if (string< "abcd" "wxyz") 't 'nil) 't)
(eq (if (string< "lo" "love") 't 'nil) 't)
(eq (if (string< "abcd" "abcde") 't 'nil) 't)
(eq (if (string< "abcde" "abcd") 't 'nil) 'nil)
(eq (if (string< "" "abcd") 't 'nil) 't)
(eq (if (string< "" "") 't 'nil) 'nil)
(eq (if (string< "b" "abcd") 't 'nil) 'nil)
(eq (if (string< "abc" "bbc") 't 'nil) 't)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string<))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string< "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string< "abc" "xyz" "foo"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string< "abc" 'abc))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string< 'abc "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string< 'abc 'xyz))
  nil)


;;; string<=
(eq (if (string<= "abcd" "abcd") 't 'nil) 't)
(eq (if (string<= "abcd" "wxyz") 't 'nil) 't)
(eq (if (string<= "abcd" "abcde") 't 'nil) 't)
(eq (if (string<= "abcde" "abcd") 't 'nil) 'nil)
(eq (if (string<= "" "") 't 'nil) 't)
(eq (if (string<= "" "a") 't 'nil) 't)
(eq (if (string<= "a" "") 't 'nil) 'nil)
(eq (if (string<= "love me tender" "love me tender") 't 'nil) 't)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string<=))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string<= "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string<= "abc" "xyz" "foo"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string<= "abc" 'abc))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string<= 'abc "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string<= 'abc 'xyz))
  nil)


;;; string>
(eq (if (string> "abcd" "wxyz") 't 'nil) 'nil)
(eq (if (string> "abcd" "abcd") 't 'nil) 'nil)
(eq (if (string> "abcd" "wxyz") 't 'nil) 'nil)
(eq (if (string> "lo" "love") 't 'nil) 'nil)
(eq (if (string> "abcd" "abcde") 't 'nil) 'nil)
(eq (if (string> "abcde" "abcd") 't 'nil) 't)
(eq (if (string> "" "abcd") 't 'nil) 'nil)
(eq (if (string> "" "") 't 'nil) 'nil)
(eq (if (string> "b" "abcd") 't 'nil) 't)
(eq (if (string> "abc" "bbc") 't 'nil) 'nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string>))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string> "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string> "abc" "xyz" "foo"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string> "abc" 'abc))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string> 'abc "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string> 'abc 'xyz))
  nil)



;;; string>=
(eq (if (string>= "abcd" "abcd") 't 'nil) 't)
(eq (if (string>= "abcd" "wxyz") 't 'nil) 'nil)
(eq (if (string>= "abcd" "abcde") 't 'nil) 'nil)
(eq (if (string>= "abcde" "abcd") 't 'nil) 't)
(eq (if (string>= "" "") 't 'nil) 't)
(eq (if (string>= "" "a") 't 'nil) 'nil)
(eq (if (string>= "a" "") 't 'nil) 't)
(eq (if (string>= "love me tender" "love me tender") 't 'nil) 't)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string>=))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string>= "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string>= "abc" "xyz" "foo"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string>= "abc" 'abc))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string>= 'abc "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (string>= 'abc 'xyz))
  nil)


;; char-index
(eql (char-index #\b "abcab") 1)
(eql (char-index #\B "abcab") nil)
(eql (char-index #\b "abcab" 2) 4)
(eql (char-index #\d "abcab") 'nil)
(eql (char-index #\a "abcab" 4) 'nil)
(eql (char-index #\a "") 'nil)
(eql (char-index #\b "abcab" 4) 4)
(eql (char-index #\b "abcab" 0) 1)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (char-index 'a "abc"))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (char-index 'a "abc" 0))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (char-index #\a 'abc))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (char-index #\a 'abc 0))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (char-index))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (char-index #\a))
  nil)
;;(block top
;;  (with-handler (lambda (condition)
;;		  (if (instancep condition (class <arity-error>))
;;		      (return-from top t)
;;                      (signal-condition condition nil)))
;;    (char-index #\a "abc" 0 9))
;;  nil)


;; string-index
(eql (string-index "foo" "foobar") 0)
(eql (string-index "bar" "foobar") 3)
(eql (string-index "FOO" "foobar") nil)
(eql (string-index "foo" "foobar" 1) nil)
(eql (string-index "bar" "foobar" 1) 3)
(eql (string-index "foo" "") nil)
(eql (string-index "" "foo") 0)

;; string-append
(equal (string-append "abc" "def") "abcdef")
(equal (string-append "abc" "abc") "abcabc")
(equal (string-append "abc" "") "abc")
(equal (string-append "" "abc") "abc")
(equal (string-append "abc" "" "def") "abcdef")

