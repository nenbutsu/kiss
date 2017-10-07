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
