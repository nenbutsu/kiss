;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_kiss_convert.lisp --- a bunch of KISS specific forms with which it must return true.

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

;;; convert

;; character
(= (convert #\space <integer>) 32)
(char= (convert #\space <character>) #\space)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (convert #\a <string>))
  nil)

(eq (convert #\a <symbol>) 'a)

;; integer
(= (convert 12 <integer>) 12)
(char= (convert 32 <character>) #\space)
(string= (convert 12 <string>) "12")
(string= (convert 120000000000 <string>) "120000000000")
(= (convert 12 <float>) 12.0)
(= (convert 120000000000 <float>) 1.2e11)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (convert 12 <symbol>))
  nil)

;; float
(= (convert 1.0 <float>) 1.0)
(= (convert 1.5 <float>) 1.5)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (convert 12.5 <integer>))
  nil)

