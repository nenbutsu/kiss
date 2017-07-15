;;; -*- mode: lisp; coding: utf-8 -*- 
;;; object.lisp --- defines the object mechanism of ISLisp processor KISS.

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


(defun object-plist-get (object property)
  (let ((plist (object-plist object)))
    (plist-get plist property)))

(defun object-plist-put (object property value)
  (let ((plist (object-plist object)))
    (set-object-plist (plist-put plist property value) object)))
