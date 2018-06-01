;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_symbol.lisp --- a bunch of forms with which ISLisp processor must return true.

;; Copyright (C) 2017, 2018 Yuji Minejima <yuji@minejima.jp>

;; This file is part of ISLisp processor KISS.

;; KISS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; KISS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; symbolp
(eq (symbolp 'a) t)
(eq (symbolp "a") nil)
(eq (symbolp #\a) nil)
(eq (symbolp 't) t)
(eq (symbolp t) t)
(eq (symbolp 'nil) t)
(eq (symbolp nil) t)
(eq (symbolp '()) t)
(eq (symbolp '*pi*) t)
(eq (symbolp *pi*) nil)
(eq (symbolp 3) nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (symbolp))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (symbolp 'a 'b))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (symbolp 'a 'b 'c))
  nil)

;; symbol names
(symbolp '+)
(symbolp '-)
(symbolp '1+)
(symbolp '1-)
(eq 'foo 'foo)
(eq 'foO 'foo)
(eq 'fOo 'foo)
(eq 'fOO 'foo)
(eq 'Foo 'foo)
(eq 'FoO 'foo)
(eq 'FOo 'foo)
(eq 'FOO 'foo)
(eq '() 'nil)
(eq '() 'NIL)


;; property, set-property, remove-property
(null (property 'foo 'none))
(and (eq (set-property 'athena 'zeus 'daughter) 'athena)
     (eq (remove-property 'zeus 'daughter) 'athena))
(eq (set-property 'love 'man 'feeling) 'love)
(eq (property 'man 'feeling) 'love)
(eq (remove-property 'man 'feeling) 'love)
(null (property 'man 'feeling))
(eq (property 'thing 'feeling 'love) 'love)
(eq (property 'thing 'feeling t) t)
(= (set-property 10 'me 'wallet) 10)
(= (property 'me 'wallet 0) 10)
(= (set-property 1000 'me 'wallet) 1000)
(= (property 'me 'wallet 0) 1000)


(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (property))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (property 'foo))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (property 'foo 'bar 'baz 'boo))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (property "love" 'who))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
                      (signal-condition condition nil)))
    (property 'me "who"))
  nil)


;; gensym
(symbolp (gensym))





