;;; -*- mode: lisp; coding: utf-8 -*- 
;;; built_in_classes.lisp --- defines builtin classes of ISLisp processor KISS.

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
