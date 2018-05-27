;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_macro.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;;; defmacro
(eq (defmacro car-car(x) (list 'car (list 'car x))) 'car-car)
(eq (car-car '((x) y z)) 'x)

;;; quasiquote
(equal (let ((foo 'a)
             (bar '(x y z)))
         `(,foo ,@bar))
       '(a x y z))

