;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_number.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; div, mod
;;(eql (div 12 3) 4)
;;(eql (div 14 3) 4)
;;(eql (div -12 3) -4)
;;(eql (div -14 3) -5)
;;(eql (div 12 -3) -4)
;;(eql (div 14 -3) -5)
;;(eql (div -12 -3) 4)
;;(eql (div -14 -3) 4)
;;(eql (mod 12 3) 0)
;;(eql (mod 7 247) 7)
;;(eql (mod 247 7) 2)
;;(eql (mod 14 3) 2)
;;(eql (mod -12 3) 0)
;;(eql (mod -14 3) 1)
;;(eql (mod 12 -3) 0)
;;(eql (mod 14 -3) -1)
;;(eql (mod -12 -3) 0)
;;(eql (mod -14 -3) -2)

