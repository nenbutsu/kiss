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
(eq (basic-vector-p #(a b c)) 't)
(eq (basic-vector-p #1A(a b c)) 't)
(eq (basic-vector-p #2a((a) (b) (c))) 'nil)

(equal (mapcar (lambda (x)
                 (list (basic-vector-p x)
                       (general-vector-p x)))
               '((a b c)
                 "abc"
                 #(a b c)
                 #1a(a b c)
                 #2a((a) (b) (c))))
       '((nil nil) (t nil) (t t) (t t) (nil nil)))

