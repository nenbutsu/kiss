;;; -*- mode: lisp; coding: utf-8 -*- 
;;; string.lisp --- defines the string handling mechanism of ISLisp processor KISS.

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

;; Two strings are string= if they are of the same length, l ,and if for every i,
;; where0 ≤ i < l, (char= (elt string1 i ) (elt string2 i)) holds.
;; if the test is satisfied, an implementation-defined non-nil value is returned; otherwise,
;; nil is returned.
(defun string= (string1 string2)
  (if (/= (length string1) (length string2))
      nil
    (let ((i 0)
	  (n (length string1)))
      (while (and (< i n) (char= (elt string1 i) (elt string2 i)))
	(setq i (+ i 1)))
      (if (= i n)
	  t
	nil))))


;; function: (string/= string1 string2) -> quasi-boolean
;; Two strings are string/= if and only if they are not string=.
;; if the test is satisfied, an implementation-defined non-nil value is returned; otherwise,
;; nil is returned.
(defun string/= (string1 string2)
  (not (string= string1 string2)))

;; function: (string< string1 string2) -> quasi-boolean
;; Two strings string1 and string2 are in order (string<) if in the first position in which
;; they differ the character of string1 is char< the corresponding character of string2,
;; or if the string1 is a proper prefix of string 2 (of shorter length and matching in all
;; the characters of string1).
(defun string< (string1 string2)
  (let ((i 0))
    (while (and (< i (length string1)) (< i (length string2))
		(char< (elt string1 i) (elt string2 i)))
      (setq i (+ i 1)))
    (if (< i (length string1))
	nil
      t)))

;; function: (string> string1 string2) -> quasi-boolean
;; Two strings are string> if and only if they are not string<=
(defun string> (string1 string2)
  (not (string<= string1 string2)))


;; function: (string<= string1 string2) -> quasi-boolean
;; Two strings are string<= if they are either string< or they are string=.
(defun string<= (string1 string2)
  (or (string< string1 string2) (string= string1 string2)))

;; function: (string>= string1 string2) -> quasi-boolean
;; Two strings are string>= if and only if they are not string<.
(defun string>= (string1 string2)
  (not (string< string1 string2)))

;; function: (char-index char string [start-position]) ¨ <object>
;; Returns the position of char in string,
;; The search starts from the position indicated by start-position (which is
;; 0-based and defaults to 0).
;; The value returned if the search succeeds is an offset from the beginning of
;; the string, not from the starting point.
;; If the char does not occur in the string, nil is returned.
;; The function char= is used for the comparisons.
;; An error shall be signaled if char is not a character or if string is not a string
;; (error-id. domain-error).
(defun char-index (char string &rest start-position)
  (let ((here (if start-position (car start-position) 0))
	(len (length string)))
    (while (and (< here len) (char/= char (elt string here)))
      (setq here (+ here 1)))
    (if (= here len)
	nil
      here)))

;; function: (string-index substring string [start-position]) -> <object>
;; Returns the position of the given substring within string.
;; The search starts from the position indicated by start-position
;; (which is 0-based and defaults to 0).
;; The value returned if the search succeeds is an offset from the beginning of
;; the string, not from the starting point. If that substring does not occur in
;; the string, nil is returned.
;; Presence of the substring is done by sequential use of char= on corresponding
;; elements of the two strings.
;; An error shall be signaled if either substring or string is not a string
;; (error-id. domain-error)
(defun string-index (substring string &rest start-position)
  (if (= (length substring) 0)
      0
    (if (= (length string) 0)
	nil
      (let ((here (char-index (elt substring 0) string (if start-position (car start-position) 0))))
	(while here
	  (let ((p (+ here 1))
		(n 1))
	    (while (and (< n (length substring))
			(< p (length string))
			(char= (elt substring n) (elt string p)))
	      (setq n (+ n 1))
	      (setq p (+ p 1)))
	    (if (= n (length substring))
		(return-from string-index here))
	    (setq here (+ here 1))
	    (if (= here (length string))
		(return-from string-index nil))
	    (setq here (char-index (elt substring 0) string here))))
	nil))))

