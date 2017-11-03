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

(defmacro setf (place form)
  (if (symbolp place)
      `(setq ,place ,form)
      (case (car place)
        ((car)
         `(set-car ,form ,(cadr place)))
        ((cdr)
         `(set-cdr ,form ,(cadr place)))
        ((dynamic)
         `(set-dynamic ,form ,(cadr place)))
        ((elt)
         `(set-elt ,form ,@(cdr place)))
        ((property)
         `(set-property ,form ,@(cdr place)))
        ((aref)
         `(set-aref ,form ,@(cdr place)))
        ((garef)
         `(set-garef ,form ,@(cdr place)))
        (t
         (error "setf. unimplemented place: ~S" place)))))
