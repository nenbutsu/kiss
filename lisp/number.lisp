;;; -*- mode: lisp; coding: utf-8 -*- 
;;; number_l.lisp --- defines the number handling mechanism of ISLisp processor KISS.

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


;; function: (>= x1 x2) -> boolean
(defun >= (x1 x2) (not (< x1 x2)))

;; function: (> x1 x2) -> boolean
(defun > (x1 x2) (not (<= x1 x2)))


;; function: (min x+) -> <number>
;; The function min returns the least (closest to negative infinity) of its arguments.
;; The comparison is done by < .
(defun min (x1 &rest rest)
  (let ((min x1))
    (while rest
      (if (< (car rest) min)
	  (setq min (car rest)))
      (setq rest (cdr rest)))
    min))

;; function: (max x+) -> <number>
;; The function min returns the greatest (closest to positive infinity) of its arguments.
;; The comparison is done by > .
(defun max (x1 &rest rest)
  (let ((max x1))
    (while rest
      (if (> (car rest) max)
	  (setq max (car rest)))
      (setq rest (cdr rest)))
    max))

