;;; -*- mode: lisp; coding: utf-8 -*- 
;;; gf_invoke.lisp --- defines the generic function invocation mechanism of ISLisp processor KISS.

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


(defun applicable-methods (methods args)
  (let ((arg-classes (mapcar #'class-of args)))
    (flet ((applicablep (method arg-classes)
             (let ((specializers (object-plist-get method ':specializers)))
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
                     (object-plist-get m1 ':specializers)
                     (object-plist-get m2 ':specializers))
               nil))
           (make-applicable (method args)
             (let ((m (kiss::make-object (copy-list (object-plist method)))))
               (object-plist-put m ':args args)
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
                (object-plist-put method ':next (cadr methods)))))
        methods)
  nil)

(defun generic-function-invoke (gf args)
  (let ((around (applicable-methods (object-plist-get gf ':around) args))
        (before (applicable-methods (object-plist-get gf ':before) args))
        (primary (applicable-methods (object-plist-get gf ':primary) args))
        (after (nreverse (applicable-methods (object-plist-get gf ':after)
                                              args))))
    (if (null primary)
        (error "No applicable primary method ~S" gf))
    (splice-methods (append around primary))
    (object-plist-put (car primary) ':before before)
    (object-plist-put (car primary) ':after after)
    (if around
        (method-invoke (car around))
      (method-invoke (car primary)))))

(provide 'generic-function)

;;for test
;;(defgeneric foo (a))
;;(defmethod foo (a) a)
;;(defmethod foo :before (a) (print 'before-<object>))
;;(defmethod foo :before ((a <list>)) (print 'before-<list>))
;;
;;(defmethod foo :after (a) (print 'after-<object>))
;;(defmethod foo :after ((a <list>)) (print 'after-<list>))
;;(defmethod foo :after ((a <list>)) (print 'after-<list>))
;;
;;(defmethod foo :around (a) (print 'around-<object>)  (if (next-method-p) (call-next-method)))
;;(defmethod foo :around ((a <list>)) (print 'around-<list>) (if (next-method-p) (call-next-method)))
