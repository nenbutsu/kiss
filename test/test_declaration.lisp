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

;; assure
(equal (assure <cons> '(a b c))
       '(a b c))

(= (assure <integer> 1)
   1)

(string= (assure <string> "love")
         "love")

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (assure <cons> 'nil))
  nil)

(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (assure <string> 'love))
  nil)
