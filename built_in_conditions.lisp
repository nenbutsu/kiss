;;; -*- mode: lisp; coding: utf-8 -*- 
;;; built_in_conditions.lisp --- defines builtin conditions of ISLisp processor KISS.

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

;;  <object>
;;     |
;;     +--> <serious-condition>
;;              |
;;              +--> <error>
;;              |       |
;;              |       +--> <arithmetic-error>
;;              |       |           |
;;              |       |           +--> <division-by-zero>
;;              |       |           +--> <floating-point-overflow>
;;              |       |           +--> <floating-point-underflow>
;;              |       |
;;              |       +--> <control-error>
;;              |       +--> <parse-error>
;;              |       +--> <program-error>
;;              |       |           |
;;              |       |           +--> <index-out-of-range> *
;;              |       |           +--> <domain-error>
;;              |       |           +--> <undefined-entity>
;;              |       |                     |
;;              |       |                     +--> <unbound-variable>
;;              |       |                     +--> <undefined-function>
;;              |       +--> <simple-error>
;;              |       +--> <stream-error>
;;              |                   |
;;              |                   +--> <end-of-stream>
;;              +--> <storage-exhausted>
;;      
;; `*' indicates erros not defined in the spec.


(defclass <serious-condition> (<object>)
  ((continuable :reader condition-continuable :initarg continuable
                :writer set-condition-continuable :initform 'nil))
  (:metaclass <built-in-class>)
  (:abstractp t))

(defclass <storage-exhausted> (<serious-condition>) ()
  (:metaclass <built-in-class>))

(defclass <error> (<serious-condition>)
  ((id :reader error-id :initarg error-id :initform 'nil))
  (:metaclass <built-in-class>)
  (:abstractp t))

(defclass <simple-error> (<error>)
  ((format-string :reader simple-error-format-string :initarg format-string
                  :initform "simple error")
   (format-arguments :reader simple-error-format-arguments
                     :initarg format-arguments
                     :initform nil))
  (:metaclass <built-in-class>))

(defclass <stream-error> (<error>)
  ((stream :reader stream-error-stream :initarg stream))
  (:metaclass <built-in-class>))

(defclass <end-of-stream> (<stream-error>) ()
  (:metaclass <built-in-class>))

(defclass <arithmetic-error> (<error>)
  ((operation :reader arithmetic-error-operation :initarg operation)
   (operands :reader arithmetic-error-operands :initarg operands))
  (:metaclass <built-in-class>))

(defclass <division-by-zero> (<arithmetic-error>) ()
  (:metaclass <built-in-class>))

(defclass <floating-point-overflow> (<arithmetic-error>) ()
  (:metaclass <built-in-class>))

(defclass <floating-point-underflow> (<arithmetic-error>) ()
  (:metaclass <built-in-class>))

(defclass <control-error> (<error>)
  ((name :reader control-error-name :initarg name)
   (namespace :reader control-error-namespace :initarg namespace))
  (:metaclass <built-in-class>))

(defclass <parse-error> (<error>)
  ((string :reader parse-error-string :initarg string)
   (expected-class :reader parse-error-expected-class :initarg expected-class))
  (:metaclass <built-in-class>))

(defclass <program-error> (<error>) ()
  (:metaclass <built-in-class>))

(defclass <index-out-of-range> (<program-error>) ;; not in the spec.
  ((sequence :reader index-out-of-range-sequence :initarg sequence)
   (index :reader index-out-of-range-index :initarg index))
  (:metaclass <built-in-class>))

(defclass <domain-error> (<program-error>)
  ((object :reader domain-error-object :initarg object)
   (expected-class :reader domain-error-expected-class :initarg expected-class))
  (:metaclass <built-in-class>))

(defclass <undefined-entity> (<program-error>)
  ((name :reader undefined-entity-name :initarg name)
   (namespace :reader undefined-entity-namespace :initarg namespace))
  (:metaclass <built-in-class>))

(defclass <unbound-variable> (<undefined-entity>) ()
  (:metaclass <built-in-class>))

(defclass <undefined-function> (<undefined-entity>) ()
  (:metaclass <built-in-class>))

