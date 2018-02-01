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

;; integerp
(integerp *most-positive-fixnum*)
(integerp (+ *most-positive-fixnum* 1))
(integerp *most-negative-fixnum*)
(integerp (- *most-negative-fixnum* 1))

