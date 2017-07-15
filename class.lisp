;;; -*- mode: lisp; coding: utf-8 -*- 
;;; class.lisp --- defines class handling mechanism of ISLisp processor KISS.

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

;; kiss::classes = ((name-symbol1 . class-object1) ...) ;; that is, an alist.
(defglobal kiss::classes
  `((<built-in-class> . ,(kiss::make-object 'nil))))

;; special operator: (class class-name) -> <class>
;; Returns the class object that corresponds to the class named class-name.
;; On error, signal <undefined-entity> see spec. p.119
(defmacro class (class-name)
  `(kiss::class ',class-name))
(defun kiss::class (class-name)
  (let ((binding (assoc class-name kiss::classes)))
    (if binding
        (cdr binding)
      (error "Undefined class ~S" class-name))))

(defun class-supers (c)
  (kiss::assure-class c)
  (object-plist-get c ':direct-super-classes))

(defun class-cpl (c)
  (kiss::assure-class c)
  (object-plist-get c ':class-precedence-list))

;; spec. p. 51
;; Let C1, . . . , Cn be the direct superclasses of C in the order defined in
;; the defclass defining form for C. Let P1, . . ., Pn be the class precedence
;; lists for C1, . . . , Cn, respectively. Define P . Q on class precedence
;; lists P and Q to be the two lists appended. Then the class precedence
;; list for C is C . P1 . . . . Pn with duplicate classes removed by
;; repeated application of the following rule: If a class appears twice in
;; the resulting class precedence list, the leftmost occurrence is removed.
(defun kiss::compute-cpl (c supers)
  (let ((cpl (cons c (apply #'append (mapcar #'class-cpl supers)))))
    ;; cpl = `(,c ,@cpl1 ,@cpl2 ,@cpl3 ... ,@cpln)
    (mapcon (lambda (l)
              (if (member (car l) (cdr l))
                  '()
                (list (car l))))
            cpl)))

(defun class-name (class)
  (kiss::assure-class class)
  (object-plist-get class ':name))

(defun kiss::intern-class (name)
  (let ((binding (assoc name kiss::classes)))
    (cond
     ((consp binding)
      (cdr binding))
     (t (let ((class (kiss::make-object nil)))
          (setq kiss::classes `(,(cons name class) ,@kiss::classes))
          class)))))

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


;; function: (class-of obj) -> <class>
;; Returns the class of which the  given obj is a direct instance.
;; obj may be any ISLISP object.
(defun class-of (obj)
  (cond
   ((null obj)              (class <null>))
   ((consp obj)             (class <cons>))
   ((symbolp obj)           (class <symbol>))
   ((characterp obj)        (class <character>))
   ((integerp obj)          (class <integer>))
   ((floatp obj)            (class <float>))
   ((stringp obj)           (class <string>))
   ((general-vector-p obj)  (class <general-vector>))
   ((streamp obj)           (class <stream>))
   ((simple-function-p obj) (class <function>))
   ((object-p obj)          (object-plist-get obj ':class))
   (t (error "class-of: not-yet-implemented-class of ~S" obj))))

(defun kiss::assure-class (class)
  (let ((metaclass (class-of class)))
    (if (or (eq (class <built-in-class>) metaclass)
            (eq (class <standard-class>) metaclass))
        class
      (error "Not a class ~S" class))))

;; function: (subclassp subclass superclass) -> boolean
;; Returns t if the class subclass is a subclass of the class superclass;
;; otherwise, returns nil. An error shall be signaled if either subclass or
;; superclass is not a class object (error-id. domain-error). */
(defun subclassp (sub super)
  (kiss::assure-class sub)
  (kiss::assure-class super)
  (let ((class-precedence-list (class-cpl sub)))
    (if (member super class-precedence-list)
        t
      nil)))

;; function: (instancep obj class) -> boolean
;; Returns t if obj is an instance (directly or otherwise) of the class
;; class; otherwise, returns nil obj may be any ISLISP object. An error
;; shall be signaled if class is not a class object
;; (error-id. domain-error ). */
(defun instancep (obj class)
  (kiss::assure-class class)
  (let ((c (class-of obj)))
    (if (or (eq c class) (subclassp c class))
        t
      nil)))


(provide 'class)
