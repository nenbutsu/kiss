;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_declaration.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; the
(equal (the <cons> '(a b c))
       '(a b c))

(= (the <integer> 1)
   1)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (the <cons>))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (the))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (the <cons> '(a b c) 1))
  nil)



;; assure
(equal (assure <cons> '(a b c))
       '(a b c))

(= (assure <integer> 1)
   1)

(string= (assure <string> "love")
         "love")

(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (assure <cons> 'nil))
  nil)

(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (assure <string> 'love))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (assure <foo> '(a b c)))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (assure <cons>))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (assure))
  nil)
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from top t)
                      (signal-condition condition nil)))
    (assure <cons> '(1 2 3) 'a))
  nil)
