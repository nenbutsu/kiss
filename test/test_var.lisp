;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_var.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; https://nenbutsu.github.io/ISLispHyperDraft/islisp-v23.html#constant_violation
;; A complying ISLISP text shall not attempt to create a lexical
;; variable binding for any named constant defined in this
;; document. It is a violation if any such attempt is made.
(block top
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from top t)
		    (signal-condition condition nil)))
    (let ((*pi* 0))
      *pi*))
  nil)

