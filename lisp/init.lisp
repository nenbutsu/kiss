;;; -*- mode: lisp; coding: utf-8 -*- 
;;; init.lisp --- initialization of ISLisp processor KISS.

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

(defdynamic *tab-width* 8)
(defun open-output-stream (filename &rest element-class)
  (apply #'open-output-file filename element-class))
(defun open-input-stream (filename &rest element-class)
  (apply #'open-input-file filename element-class))

