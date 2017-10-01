;;; -*- mode: lisp; coding: utf-8 -*- 
;;; cons_l.lisp --- defines the cons handling mechanism of ISLisp processor KISS.

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

;; Common Lisp fucntion: last list &optional n => tail
;; Arguments and Values:
;; list---a list, which might be a dotted list but must not be a circular list.
;; n---a non-negative integer. The default is 1.
;; tail---an object.
;;
;; Description:
;; last returns the last n conses (not the last n elements) of list).
;; If list is (), last returns ().
;; If n is zero, the atom that terminates list is returned. If n is greater
;; than or equal to the number of cons cells in list, the result is list.
(defun last (list &rest args)
  (let ((n (if (null args) 1 (car args))))
    (if (< n 0)
	(kiss::signal-non-negative-integer n nil))
    (let* ((len (length list))
           (i (- len n)))
      (while (> i 0)
        (setq i (- i 1))
        (setq list (cdr list)))
      list)))

;; Common Lisp, Emacs Lisp function: nconc &rest lists => concatenated-list
;; Arguments and Values:
;; list---each but the last must be a list (which might be a dotted list
;; but must not be a circular list); the last list may be any object.
;; concatenated-list---a list.
(defun nconc (&rest lists)
  (cond
   ((null lists)
    ;; (nconc) =>  ()
    nil)
   ((null (car lists))
    ;; (nconc nil . lists) ==  (nconc . lists)
    (apply #'nconc (cdr lists)))
   ((null (cdr lists))
    ;; (nconc list) =>  list
    (car lists))
   ((null (cddr lists))
    ;; (nconc list-1 list-2) ==  (progn (set-cdr list-2 (last list-1)) list-1)
    (set-cdr (cadr lists) (last (car lists)))
    (car lists))
   (t
    ;; (nconc list-1 list-2 . lists) ==  (nconc (nconc list-1 list-2) . lists)
    (apply #'nconc (nconc (car lists) (cadr lists)) (cddr lists)))))

(defun kiss::mapcar1 (function list)
  (let ((result nil))
    (for ((p list (cdr p))) ((not (consp p)))
         (setq result `(,(funcall function (car p)) ,@result)))
    (nreverse result)))

;; function: (mapcan function list+) -> <list>
;; mapcan is like mapcar, except that the results of applying function are combined
;; into a list by the use of an operation that performs a destructive form of
;; append rather than list.
(defun mapcan (function list1 &rest rest)
  (let ((lists `(,list1 ,@rest))
        (result nil))
    (kiss::mapcar1 #'kiss::assure-list lists)
    (while (not (member nil lists))
      (setq result `(,(apply function (kiss::mapcar1 #'car lists)) ,@result))
      (setq lists (kiss::mapcar1 #'cdr lists)))
    (apply #'nconc (nreverse result))))

;; function: (maplist function list+) -> <list>
;; maplist is like mapcar except that function is applied to successive sublists of
;; the lists. function is first applied to the lists themselves, and then to the cdr of
;; each list, and then to the cdr of the cdr of each list, and so on.
(defun maplist (function list1 &rest rest)
  (let ((lists `(,list1 ,@rest))
        (result nil))
    (kiss::mapcar1 #'kiss::assure-list lists)
    (while (not (member nil lists))
      (setq result `(,(apply function lists) ,@result))
      (setq lists (kiss::mapcar1 #'cdr lists)))
    (nreverse result)))

;; function: (mapl function list1 list*) -> list1
;; mapl is like maplist except that the results of applying function are not accumulated;
;; list1 is returned.
(defun mapl (function list1 &rest rest)
  (let ((lists `(,list1 ,@rest)))
    (kiss::mapcar1 #'kiss::assure-list lists)
    (while (not (member nil lists))
      (apply function lists)
      (setq lists (kiss::mapcar1 #'cdr lists)))
    list1))

;; function: (mapcon function list+) -> <list>
;; mapcon  are like maplist, except that the results of applying function are combined
;; into a list by the use of an operation that performs a destructive form of append
;; rather than list.
(defun mapcon (function list1 &rest rest)
  (let ((lists `(,list1 ,@rest))
        (result nil))
    (kiss::mapcar1 #'kiss::assure-list lists)
    (while (not (member nil lists))
      (setq result `(,(apply function lists) ,@result))
      (setq lists (kiss::mapcar1 #'cdr lists)))
    (apply #'nconc (nreverse result))))


(defun plist-mapc (fun plist)
  (while (consp plist)
    (funcall fun (car plist) (cadr plist))
    (setq plist (cddr plist))))

;; Common Lisp
;; If object is the same as some tail of list, ldiff returns a fresh
;; list of the elements of list that precede object in the list
;; structure of list; otherwise, it returns a copy[2] of list.
;; tail n. (of a list): an object that is the same as either some cons
;; which makes up that list or the atom (if any) which terminates the
;; list.
(defun ldiff (list tail)
  (let* ((result (cons nil nil))
         (here result))
    (while (and (consp list) (not (eq list tail)))
      (set-cdr (cons (car list) nil) here)
      (setq here (cdr here))
      (setq list (cdr list)))
    (if (not (eq list tail))
        (set-cdr list here))
    (cdr result)))

;; Common Lisp
;; If object is the same as some tail of list, tailp returns true;
;; otherwise, it returns false. 
(defun tailp (tail list)
  (while (and (consp list) (not (eq list tail)))
    (setq list (cdr list)))
  (if (eq list tail)
      t
    nil))


;; Common Lisp
;; function: copy-tree tree => new-tree
;; Creates a copy of a tree of conses. 
;; If tree is not a cons, it is returned; otherwise, the result is a
;; new cons of the results of calling copy-tree on the car and cdr of
;; tree. In other words, all conses in the tree represented by tree
;; are copied recursively, stopping only when non-conses are
;; encountered.
(defun copy-tree (tree)
  (if (not (consp tree))
      tree
    (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))


(defmacro pop (listname)
  `(prog1
     (car ,listname)
     (setq ,listname (cdr ,listname))))
