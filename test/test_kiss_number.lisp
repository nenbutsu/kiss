;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_kiss_number.lisp --- a bunch of KISS specific forms with which it must return true.

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

;; =
(not (= 1 (* *most-positive-fixnum* 3)))
(= (* *most-positive-fixnum* 3) (* *most-positive-fixnum* 3))
(not (= (* *most-positive-fixnum* 3) (* *most-positive-fixnum* 4)))
(not (= 3.0 (* *most-positive-fixnum* 3)))

;; <
(< *most-positive-fixnum* (+ *most-positive-fixnum* 1))
(< (* *most-positive-fixnum* 2) (* *most-positive-fixnum* 3))
(< 1 (* *most-positive-fixnum* 3))
(< 3.3 (* *most-positive-fixnum* 3))


;; integerp
(integerp *most-positive-fixnum*)
(integerp (+ *most-positive-fixnum* 1))
(integerp *most-negative-fixnum*)
(integerp (- *most-negative-fixnum* 1))

;; div
(= (div (- *most-negative-fixnum* 1) *most-negative-fixnum*) 1)
(= (div (- *most-negative-fixnum* 1) *most-positive-fixnum*) -2)

