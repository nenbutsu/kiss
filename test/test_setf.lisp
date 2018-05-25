;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_setf.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;;; setf

;; (setf (aref array ...))
(defdynamic dynamic-a 10)
(and (= (dynamic dynamic-a) 10)
     (setf (dynamic dynamic-a) 0)
     (= (dynamic dynamic-a) 0))

(defglobal array1 (create-array '(3 3 3) 0))
(equal array1
       #3a(((0 0 0) (0 0 0) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))))
(setf (aref array1 0 1 2) 'x)
(equal array1
       #3a(((0 0 0) (0 0 x) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))))
(setf (aref array1 2 1 0) 'z)
(equal array1
       #3a(((0 0 0) (0 0 x) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))
           ((0 0 0) (z 0 0) (0 0 0))))

(defglobal vec #(0 1 2 3 4))
(setf (aref vec 0) 'a)
(equal vec #(a 1 2 3 4))

(defglobal str (create-string 3 #\space))
(setf (aref str 2) #\z)
(string= str "  z")


;; (setf (garef garray ...))
(setq vec #(0 1 2 3 4))
(setf (garef vec 0) 'a)
(equal vec #(a 1 2 3 4))

(setq array1 (create-array '(3 3 3) 0))
(equal array1
       #3a(((0 0 0) (0 0 0) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))))
(setf (garef array1 0 1 2) 'x)
(equal array1
       #3a(((0 0 0) (0 0 x) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))))
(setf (garef array1 2 1 0) 'z)
(equal array1
       #3a(((0 0 0) (0 0 x) (0 0 0))
           ((0 0 0) (0 0 0) (0 0 0))
           ((0 0 0) (z 0 0) (0 0 0))))


;; (setf (elt list ...))
(defglobal list (create-list 4 nil))
(setf (elt list 1) 'a)
(equal list '(nil a nil nil))
(setf (elt list 3) 'z)
(equal list '(nil a nil z))

(setq vec #(0 1 2 3 4))
(setf (elt vec 0) 'a)
(equal vec #(a 1 2 3 4))
(setf (elt vec 3) 'z)
(equal vec #(a 1 2 z 4))

(setq str (create-string 3 #\space))
(setf (elt str 0) #\a)
(string= str "a  ")
(setf (elt str 2) #\z)
(string= str "a z")


;; (setf (car list) ...)
(setq list (create-list 3 t))
(setf (car list) 'a)
(equal list '(a t t))
(setf (car (cdr list)) 'b)
(equal list '(a b t))
(setf (car (cdr (cdr list))) 'c)
(equal list '(a b c))


;; (setf (cdr list) ...)
(setq list (create-list 3 t))
(progn (setf (cdr list) nil)
       (equal list '(t)))
(setq list (create-list 3 t))
(progn (setf (cdr (cdr list)) nil)
       (equal list '(t t)))


;; (setf (property symbol) ...)
(defglobal foo nil)
(setf (property 'foo 'counter) 10)
(= (property 'foo 'counter) 10)
(setf (property 'foo 'today) 'Monday)
(eq (property 'foo 'today) 'Monday)
