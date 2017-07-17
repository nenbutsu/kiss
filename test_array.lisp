;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_array.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; basic-array-p, ...
(equal (mapcar (lambda (x)
		 (list (basic-array-p x)
		       (basic-array*-p x)
		       (general-array*-p x)))
	       '((a b c)
		 "abc"
		 #(a b c)
		 #1a(a b c)
		 #2a((a) (b) (c))))
       '((nil nil nil) (t nil nil) (t nil nil) (t nil nil) (t t t)))

;; create-array
(equal (create-array '(2 3) 0.0)
       #2a((0.0 0.0 0.0) (0.0 0.0 0.0)))
