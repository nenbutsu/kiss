;;; -*- mode: lisp; coding: utf-8 -*- 
;;; sequence_l.lisp --- defines the sequence handling mechanism of ISLisp processor KISS.

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


(defun sort (seq predicate)
  (if (not (listp seq))
      (error "sort: non list sequence not implemented yet ~S" seq))
  (if (null seq)
      nil
    (labels ((insert (elm list)
               (cond
                ((null list) (list elm))
                ((funcall predicate elm (car list)) (cons elm list))
                (t (cons (car list) (insert elm (cdr list)))))))
      (insert (car seq) (sort (cdr seq) predicate)))))

