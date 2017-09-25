;;; -*- mode: lisp; coding: utf-8 -*- 
;;; class_l.lisp --- defines class handling mechanism of ISLisp processor KISS.

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

(defun kiss::make-class (name supers metaclass)
  (if (and (not (eq metaclass (class <built-in-class>)))
           (null supers))
      (setq supers (list (class <standard-object>))))
  (let* ((class (kiss::intern-class name))
         (cpl (kiss::compute-cpl class supers)))
    (object-plist-put class ':name name)
    (object-plist-put class ':direct-super-classes supers)
    (object-plist-put class ':class metaclass)
    (object-plist-put class ':class-precedence-list cpl)
    class))

;;defining operator
;;  (defclass class-name (sc-name*) (slot-spec*) class-opt*) -> <symbol>
;;    class-name ::= identifier
;;    sc-name ::= identifier
;;    slot-spec ::= slot-name | (slot-name slot-opt *)
;;    slot-name ::= identifier
;;    slot-opt ::= :reader reader-function-name |
;;                 :writer writer-function-name |
;;                 :accessor reader-function-name |
;;                 :boundp boundp-function-name |
;;                 :initform form |
;;                 :initarg initarg-name
;;    initarg-name ::= identifier
;;    reader-function-name ::= identifier
;;    writer-function-name ::= identifier
;;    boundp-function-name ::= identifier
;;    class-opt ::= (:metaclass class-name) | (:abstractp abstract-flag)
;;    abstractp ::= t | nil
(defmacro defclass (class-name sc-names slot-specs &rest class-opts)
  `(kiss::defclass ',class-name ',sc-names ',slot-specs ',class-opts))
(defun kiss::defclass (class-name sc-names slot-specs class-opts)
  (let* ((supers (mapcar #'kiss::class sc-names))
         (metaclass-spec (assoc ':metaclass class-opts))
         (metaclass (kiss::class (if metaclass-spec
                                     (cadr metaclass-spec)
                                   '<standard-class>)))
         (class (kiss::make-class class-name supers metaclass)))
    (object-plist-put class ':slot-specs
                      (canonicalize-slot-specs class slot-specs))
    class-name))

(defun kiss::slot-read (object slot-name)
  (let* ((slots (object-plist-get object ':slots))
         (binding (assoc slot-name slots)))
    (if (null binding)
        (error "Unbound slot ~S" slot-name))
    (cdr binding)))

(defun kiss::slot-write (object slot-name value)
  (let* ((slots (object-plist-get object ':slots))
         (binding (assoc slot-name slots)))
    (if (null binding)
        (progn
          (setq slots (cons (cons slot-name value) slots))
          (object-plist-put object ':slots slots))
      (set-cdr value binding)))
  value)

(defun kiss::slot-boundp (object slot-name)
  (let* ((slots (object-plist-get object ':slots))
         (binding (assoc slot-name slots)))
    (consp binding)))

(defun define-slot-reader (class slot-name reader-name)
  (if (not (fboundp reader-name))
      (kiss::defgeneric reader-name '(object) nil))
  (let ((object-var (gensym)))
    (kiss::defmethod reader-name `((,object-var ,(class-name class)))
                     `((kiss::slot-read ,object-var ',slot-name)))))

(defun define-slot-writer (class slot-name writer-name)
  (if (not (fboundp writer-name))
      (kiss::defgeneric writer-name '(value object) nil))
  (let ((object-var (gensym))
        (value-var (gensym)))
    (kiss::defmethod writer-name `((,value-var <object>)
                                   (,object-var ,(class-name class)))
                     `((kiss::slot-write ,object-var ',slot-name ,value-var)))))

(defun define-slot-boundp (class slot-name boundp-name)
  (if (not (fboundp boundp-name))
      (kiss::defgeneric boundp-name '(object) nil))
  (let ((object-var (gensym)))
    (kiss::defmethod boundp-name `((,object-var ,(class-name class)))
                     `((kiss::slot-boundp ,object-var ',slot-name)))))

  
(defun canonicalize-slot-spec (class spec)
  (if (not (consp spec))
      (list spec)
    (progn
      (let ((slot-name (car spec))
            (plist (cdr spec)))
        (while plist
          (let ((property (car plist))
                (value (cadr plist)))
            (case property
              ((:reader) (define-slot-reader class slot-name value))
              ((:writer) (define-slot-writer class slot-name value))
              ((:boundp) (define-slot-boundp class slot-name value))
              ((:initarg)
               )
              ((:initform)
               )
              ((:accessor)
               (error "slot's assessor not implemeted yet ~S" plist))
              (t
               )))
          (setq plist (cddr plist))))
      spec)))

(defun canonicalize-slot-specs (class slot-specs)
  ;;(print "slot-specs")
  ;;(print slot-specs)
  (mapcar (lambda (spec) (canonicalize-slot-spec class spec))
          slot-specs))


(defun kiss::assure-class (class)
  (let ((metaclass (class-of class)))
    (if (or (eq (class <built-in-class>) metaclass)
            (eq (class <standard-class>) metaclass))
        class
      (error "Not a class ~S" class))))


(provide 'class)
