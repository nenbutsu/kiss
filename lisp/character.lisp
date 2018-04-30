;;; -*- mode: lisp; coding: utf-8 -*- 
;;; character_l.lisp --- defines character handling mechanism of ISLisp processor KISS.

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


(defun char/= (char1 char2) (not (char= char1 char2)))
(defun char<= (char1 char2) (or (char= char1 char2) (char< char1 char2)))
(defun char>  (char1 char2) (not (char<= char1 char2)))
(defun char>= (char1 char2) (not (char< char1 char2)))
