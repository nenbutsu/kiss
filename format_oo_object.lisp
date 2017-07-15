;;; -*- mode: lisp; coding: utf-8 -*-
;;; format_object.lisp --- defines the formatting mechanism of ISLisp processor KISS.

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
