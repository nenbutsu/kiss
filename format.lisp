;;; -*- mode: lisp; coding: utf-8 -*- 
;;; format.lisp --- defines the formatting mechanism of ISLisp processor KISS.

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

;; function: (format output-stream format-string obj*) → <null> 
(defun format (output-stream format-string &rest objects)
  (let ((i 0)
        (n (length format-string)))
    (while (< i n)
      (let ((c (elt format-string i)))
        (if (char/= c #\~)
            (format-char output-stream c)
          (progn
            (setq i (+ i 1))
            (if (= i n) (error "format string ends with ~S" #\~))
            (setq c (elt format-string i))
            (case c
              ((#\A) (format-object output-stream (pop objects) nil))
              ((#\B) (format-integer output-stream (pop objects) 2))
              ((#\C) (format-char output-stream (pop objects)))
              ((#\D) (format-integer output-stream (pop objects) 10))
              ((#\G) (format-float output-stream (pop objects)))
              ((#\O) (format-integer output-stream (pop objects) 8))
              ((#\S) (format-object output-stream (pop objects) t))
              ((#\X) (format-integer output-stream (pop objects) 16))
              ((#\%) (format-char output-stream #\newline))
              ((#\&) (format-fresh-line output-stream))
              ((#\~) (format-char output-stream #\~))
              (t (error "internal error: non-implemented directive ~S" c))))))
      (setq i (+ i 1)))
    nil))
