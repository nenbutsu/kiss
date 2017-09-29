;;; -*- mode: lisp; coding: utf-8 -*- 
;;; ilos.lisp --- defines class handling mechanism of ISLisp processor KISS.

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

(defun kiss::oref (object property)
  (let ((plist (object-plist object)))
    (plist-get plist property)))

(defun kiss::set-oref (value object property)
  (let ((plist (object-plist object)))
    (set-object-plist (plist-put plist property value) object)))


;; kiss::classes = ((name-symbol1 . class-object1) ...) ;; that is, an alist.
(defglobal kiss::classes
  `((<built-in-class> . ,(kiss::make-ilos-obj 'nil))))

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
  (kiss::oref c ':direct-super-classes))

(defun class-cpl (c)
  (kiss::assure-class c)
  (kiss::oref c ':class-precedence-list))

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
  (kiss::oref class ':name))

(defun kiss::intern-class (name)
  (let ((binding (assoc name kiss::classes)))
    (cond
     ((consp binding)
      (cdr binding))
     (t (let ((class (kiss::make-ilos-obj nil)))
          (setq kiss::classes `(,(cons name class) ,@kiss::classes))
          class)))))

(defun kiss::make-class (name supers metaclass)
  (if (and (not (eq metaclass (class <built-in-class>)))
           (null supers))
      (setq supers (list (class <standard-object>))))
  (let* ((class (kiss::intern-class name))
         (cpl (kiss::compute-cpl class supers)))
    (kiss::set-oref name class ':name)
    (kiss::set-oref supers class ':direct-super-classes)
    (kiss::set-oref metaclass class ':class)
    (kiss::set-oref cpl class ':class-precedence-list)
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
    (kiss::set-oref (canonicalize-slot-specs class slot-specs) class ':slot-specs)
    class-name))

(defun kiss::slot-read (object slot-name)
  (let* ((slots (kiss::oref object ':slots))
         (binding (assoc slot-name slots)))
    (if (null binding)
        (error "Unbound slot ~S" slot-name))
    (cdr binding)))

(defun kiss::slot-write (object slot-name value)
  (let* ((slots (kiss::oref object ':slots))
         (binding (assoc slot-name slots)))
    (if (null binding)
        (progn
          (setq slots (cons (cons slot-name value) slots))
          (kiss::set-oref slots object ':slots))
      (set-cdr value binding)))
  value)

(defun kiss::slot-boundp (object slot-name)
  (let* ((slots (kiss::oref object ':slots))
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
   ((general-array*-p obj)  (class <general-array*>))
   ((streamp obj)           (class <stream>))
   ((simple-function-p obj) (class <function>))
   ((object-p obj)          (kiss::oref obj ':class))
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

;;  <object>
;;     |
;;     +--> <basic-array>
;;     |        |
;;     |        +--> <basic-array*>
;;     |        |        |
;;     |        |        +--> <general-array*>
;;     |        |
;;     |        +--> <basic-vector>
;;     |                 |
;;     |                 +--> <general-vector>
;;     |                 +--> <string>
;;     |
;;     +--> <built-in-class>
;;     +--> <character>
;;     +--> <function>
;;     |        |
;;     |        +--> <generic-function>
;;     |                 |
;;     |                 +--> <standard-generic-function>
;;     +--> <list>
;;     |        |
;;     |        +--> <cons>
;;     |        +--> <null> cpl = (<null> <symbol> <list> <object>)
;;     |                 ^
;;     |                 |
;;     |        +--------+
;;     |        |
;;     +--> <symbol>
;;     |
;;     +--> <number>
;;     |        |
;;     |        +--> <float>
;;     |        +--> <integer>
;;     |
;;     +--> <serious-condition>
;;     |        |
;;     |        +--> <error>
;;     |        |       |
;;     |        |       +--> <arithmetic-error>
;;     |        |       |           |
;;     |        |       |           +--> <division-by-zero>
;;     |        |       |           +--> <floating-point-overflow>
;;     |        |       |           +--> <floating-point-underflow>
;;     |        |       |
;;     |        |       +--> <control-error>
;;     |        |       +--> <parse-error>
;;     |        |       +--> <program-error>
;;     |        |       |           |
;;     |        |       |           +--> <domain-error>
;;     |        |       |           +--> <undefined-entity>
;;     |        |       |                     |
;;     |        |       |                     +--> <unbound-variable>
;;     |        |       |                     +--> <undefined-function>
;;     |        |       +--> <simple-error>
;;     |        |       +--> <stream-error>
;;     |        |                   |
;;     |        |                   +--> <end-of-stream>
;;     |        +--> <storage-exhausted>
;;     |
;;     +--> <standard-class>
;;     +--> <standard-object>
;;     +--> <stream>

(defclass <object> () ()
  (:metaclass <built-in-class>)
  (:abstractp t))

(defclass <built-in-class> (<object>) ()
  (:metaclass <built-in-class>)
  (:abstractp t))

(defclass <character> (<object>) ()
  (:metaclass <built-in-class>))

(defclass <symbol> (<object>) ()
  (:metaclass <built-in-class>))

(defclass <list> (<object>) ()
  (:metaclass <built-in-class>)
  (:abstractp t))
(defclass <cons> (<list>) ()
  (:metaclass <built-in-class>))

(defclass <null> (<symbol> <list>) ()
  (:metaclass <built-in-class>))

(defclass <standard-class> (<object>) ()
  (:metaclass <built-in-class>))

;; !!!!!!!!!!!!!!
;; The class named <standard-object> is an instance of the class
;; <standard-class> and is a superclass of every class that is an
;; instance of <standard-class> except itself. spec. p.13.
(defclass <standard-object> (<object>) ()
  (:metaclass <built-in-class>))

(defclass <stream> (<object>) ()
  (:metaclass <built-in-class>))

(defclass <function> (<object>) ()
  (:metaclass <built-in-class>))
(defclass <generic-function> (<function>) ()
  (:metaclass <built-in-class>)
  (:abstractp t))
(defclass <standard-generic-function> (<generic-function>) ()
  (:metaclass <built-in-class>))

(defclass <number> (<object>) ()
  (:metaclass <built-in-class>)
  (:abstractp t))
(defclass <integer> (<number>) ()
  (:metaclass <built-in-class>))
(defclass <non-negative-integer> (<integer>) ()
  (:metaclass <built-in-class>))
(defclass <float> (<number>) ()
  (:metaclass <built-in-class>))

(defclass <basic-array> (<object>) ()
  (:metaclass <built-in-class>)
  (:abstractp t))
(defclass <basic-array*> (<basic-array>) ()
  (:metaclass <built-in-class>)
  (:abstractp t))
(defclass <general-array*> (<basic-array*>) ()
  (:metaclass <built-in-class>))

(defclass <basic-vector> (<basic-array>) ()
  (:metaclass <built-in-class>)
  (:abstractp t))
(defclass <general-vector> (<basic-vector>) ()
  (:metaclass <built-in-class>))
(defclass <string> (<basic-vector>) ()
  (:metaclass <built-in-class>))


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
    (kiss::make-ilos-obj plist)))


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
                                (if (equal (kiss::oref method
                                                             ':specializers)
                                           (kiss::oref (car methods)
                                                             ':specializers))
                                    (return-from nil methods)))
                              methods)
                          nil)))
               (let* ((qualifier (kiss::oref m ':qualifier))
                      (methods (kiss::oref gf qualifier))
                      (here (agreeing-member methods m)))
                 (if here
                     (set-car m here)
                   (setq methods (cons m methods)))
                 (kiss::set-oref methods gf qualifier)))))
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
      (kiss::make-ilos-obj plist))))

(defgeneric create (class &rest initargs))
(defmethod  create (class &rest initargs)
  (let ((obj (kiss::make-ilos-obj `(:class ,class))))
    (initialize-object obj initargs)))

(defgeneric initialize-object (obj initargs))
(defmethod  initialize-object (obj initargs)
  (let* ((class (class-of obj))
         (cpl (class-cpl class))
         (slot-specs (mapcan (lambda (c)
                               (copy-list (kiss::oref c ':slot-specs)))
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


(defun applicable-methods (methods args)
  (let ((arg-classes (mapcar #'class-of args)))
    (flet ((applicablep (method arg-classes)
             (let ((specializers (kiss::oref method ':specializers)))
               (block nil
                 (mapc (lambda (c1 c2)
                         (if (and (not (eq c1 c2)) (not (subclassp c2 c1)))
                             (return-from nil nil)))
                       specializers arg-classes)
                 t)))
           (more-specific-p (m1 m2)
             (block nil
               (mapc (lambda (c1 c2)
                       (if (subclassp c1 c2)
                           (return-from nil t)))
                     (kiss::oref m1 ':specializers)
                     (kiss::oref m2 ':specializers))
               nil))
           (make-applicable (method args)
             (let ((m (kiss::make-ilos-obj (copy-list (object-plist method)))))
               (kiss::set-oref args m ':args)
               m)))
      (sort (mapcan (lambda (method)
                      (if (applicablep method arg-classes)
                          (list (make-applicable method args))
                        nil))
                    methods)
            #'more-specific-p))))

(defun splice-methods (methods)
  (mapl (lambda (methods)
          (let ((method (car methods)))
            (if (consp (cdr methods))
                (kiss::set-oref (cadr methods) method ':next))))
        methods)
  nil)

(defun generic-function-invoke (gf args)
  (let ((around (applicable-methods (kiss::oref gf ':around) args))
        (before (applicable-methods (kiss::oref gf ':before) args))
        (primary (applicable-methods (kiss::oref gf ':primary) args))
        (after (nreverse (applicable-methods (kiss::oref gf ':after)
                                              args))))
    (if (null primary)
        (error "No applicable primary method ~S" gf))
    (splice-methods (append around primary))
    (kiss::set-oref before (car primary) ':before)
    (kiss::set-oref after (car primary) ':after)
    (if around
        (method-invoke (car around))
      (method-invoke (car primary)))))


(defun kiss::format-oo-object (out obj escapep)
  (let* ((plist (object-plist obj))
         (name (plist-get plist ':name))
         (class (plist-get plist ':class))
         (class-name (class-name class)))
    (if (not name)
        (setq name "no name"))
    (format out "#<:~S: " class-name)
    (format out "~S(" name)
    (kiss::format-pointer out obj)
    (format out ")>")
    nil))
