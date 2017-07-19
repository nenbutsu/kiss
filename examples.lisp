;;; -*- mode: lisp; coding: utf-8 -*- 
;;; examples.lisp --- example functions

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

(defun euclidean-gcd (i1 i2)
  (if (< i1 i2)
      (let ((tmp i2))
	(setq i2 i1)
	(setq i1 tmp)))
  (let ((remainder (mod i1 i2)))
    (if (= remainder 0)
	i2
      (euclidean-gcd i2 remainder))))

