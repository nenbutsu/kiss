;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_stream.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;;; streamp
(eq (streamp (standard-input)) t)
(eq (streamp '()) nil)

;;; input-stream-p
(eq (input-stream-p (standard-input)) t)
(eq (input-stream-p (standard-output)) nil)
(eq (input-stream-p '(a b c)) nil)

;;; output-stream-p
(eq (output-stream-p (standard-output)) t)
(eq (output-stream-p (standard-input)) nil)
(eq (output-stream-p "hello") nil)

;; with-standard-input
(with-standard-input (create-string-input-stream "this is a string")
	(list (read) (read)))

;; with-standard-output
(let ((message nil))
  (with-standard-output (create-string-output-stream)
	(format (standard-output) "love me tender...")
	(setq message (get-output-stream-string (standard-output))))
  (string= message "love me tender..."))

;; with-error-output
(let ((message nil))
  (with-error-output (create-string-output-stream)
	(format (error-output) "love me tender...")
	(setq message (get-output-stream-string (error-output))))
  (string= message "love me tender..."))
