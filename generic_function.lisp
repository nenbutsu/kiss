;;; -*- mode: lisp; coding: utf-8 -*-
;;; generic_function.lisp --- defines the generic function mechanism of ISLisp processor KISS.

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

(defclass <method> (<object>) ()
  (:metaclass <built-in-class>))

(defun functionp (obj)
  (if (or (simple-function-p obj) (generic-function-p obj))
      t
    nil))

;; function: (generic-function-p obj ) -> boolean
;; Returns t if obj is a generic function; otherwise, returns nil.
;; obj may be any ISLISP object.
(defun generic-function-p (obj)
  (if (eq (class-of obj) (class <standard-generic-function>))
      t
    nil))

;; defining operator
;; (defgeneric func-spec lambda-list {option | method-desc}*) -> <symbol>
;;   func-spec ::= identifier | (setf identifier)
;;   lambda-list ::= (var* [&rest var]) |
;;                   (var* [:rest var])
;;   option ::= (:method-combination {identifier | keyword}) |
;;              (:generic-function-class class-name)
;;   method-desc ::= (:method method-qualifier* parameter-profile form*)
;;   method-qualifier ::= identifier | keyword
;;   parameter-profile ::= ({var | (var parameter-specializer-name)}*
;;                          [{&rest | :rest}var])
;;   parameter-specializer-name ::= class-name
;;   class-name ::= identifier
;;   var ::= identifier
(defmacro defgeneric (func-spec lambda-list &rest rest)
  `(kiss::defgeneric ',func-spec ',lambda-list ',rest))
(defun kiss::defgeneric (func-spec lambda-list rest)
  (let ((gf (kiss::make-generic-function func-spec lambda-list)))
    (set-symbol-function gf func-spec)
    (while (consp rest)
      (let ((method-desc (car rest)))
        (kiss::defmethod func-spec (cadr method-desc) (cddr method-desc)))
      (setq rest (cdr rest)))
    func-spec))

(defun kiss::make-generic-function (name lambda-list)
  (let ((plist (list ':name name
                     ':class (class <standard-generic-function>)
                     ':lambda-list lambda-list
                     ':primary nil
                     ':before nil
                     ':after nil
                     ':around nil)))
    (kiss::make-object plist)))


;; defining operator
;; (defmethod func-spec method-qualifier* parameter-profile form*) -> <symbol>
;;   func-spec ::= identifier | (setf identifier)
;;   method-qualifier ::= identifier | keyword
;;   parameter-profile ::= ({var | (var parameter-specializer-name)}*
;;                          [{&rest | :rest} var])
;;   parameter-specializer-name ::= class-name
;;   class-name ::= identifier
;;   var ::= identifier
(defmacro defmethod (func-spec arg &rest rest)
  `(kiss::defmethod ',func-spec ',arg ',rest))
(defun kiss::defmethod (func-spec arg rest)
  (if (not (generic-function-p (symbol-function func-spec)))
      (error "Not a generic function ~S" func-spec))
  (if (consp arg)
      (progn
        (setq rest (cons arg rest))
        (setq arg ':primary)))
  (let ((gf (symbol-function func-spec))
        (m (kiss::make-method arg (car rest) (cdr rest))))
    (flet ((add-method (gf m)
             (flet ((agreeing-member (methods method)
                      (block nil
                        (mapl (lambda (methods)
                                (if (equal (object-plist-get method
                                                             ':specializers)
                                           (object-plist-get (car methods)
                                                             ':specializers))
                                    (return-from nil methods)))
                              methods)
                          nil)))
               (let* ((qualifier (object-plist-get m ':qualifier))
                      (methods (object-plist-get gf qualifier))
                      (here (agreeing-member methods m)))
                 (if here
                     (set-car m here)
                   (setq methods (cons m methods)))
                 (object-plist-put gf qualifier methods)))))
      (add-method gf m)
      func-spec)))

(defun kiss::make-method (qualifier parameter-profile body)
  (flet ((specializers (profile)
           (let ((rest (or (member ':rest profile)
                           (member '&rest profile))))
             (mapcar (lambda (spec)
                       (kiss::class (if (consp spec) (cadr spec) '<object>)))
                     (ldiff profile rest))))
         (lambda-list (profile)
           (mapcar (lambda (spec) (if (consp spec) (car spec) spec))
                   profile)))
    (let ((plist (list ':class (class <method>)
                       ':qualifier qualifier
                       ':specializers (specializers parameter-profile)
                       ':lambda-list (lambda-list parameter-profile)
                       ':body body)))
      (kiss::make-object plist))))

(defgeneric create (class &rest initargs))
(defmethod  create (class &rest initargs)
  (let ((obj (kiss::make-object `(:class ,class))))
    (initialize-object obj initargs)))

(defgeneric initialize-object (obj initargs))
(defmethod  initialize-object (obj initargs)
  (let* ((class (class-of obj))
         (cpl (class-cpl class))
         (slot-specs (mapcan (lambda (c)
                               (copy-list (object-plist-get c ':slot-specs)))
                             cpl)))
    (flet ((initarg-to-slot-name (obj initarg)
             (block nil
               (mapc (lambda (spec)
                       (let ((slot-name (car spec))
                             (plist (cdr spec)))
                         (plist-mapc (lambda (property value)
                                       (if (and (eq property ':initarg)
                                                (eq value initarg))
                                           (return-from nil slot-name)))
                                     plist)))
                     slot-specs)
               (error "Undefined initarg ~S" initarg))))
      (plist-mapc (lambda (initarg value)
                    (let ((slot-name (initarg-to-slot-name obj initarg)))
                      (if (not (kiss::slot-boundp obj slot-name))
                          (kiss::slot-write obj slot-name value))))
                  initargs)
      (mapc (lambda (spec)
              (let ((slot-name (car spec))
                    (plist (cdr spec)))
                (if (not (kiss::slot-boundp obj slot-name))
                    (block nil
                      (plist-mapc (lambda (property value)
                                    (if (eq property ':initform)
                                        (progn
                                          (kiss::slot-write obj slot-name
                                                            (eval value))
                                          (return-from nil nil))))
                                  plist)))))
            slot-specs)
      obj)))

(provide 'generic-function)
