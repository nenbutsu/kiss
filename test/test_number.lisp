;;; -*- mode: lisp; coding: utf-8 -*- 
;;; test_number.lisp --- a bunch of forms with which ISLisp processor must return true.

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

;; numberp
(eq (numberp 3) t)
(eq (numberp -0.3) t)
(eq (numberp '(a b c)) nil)
(eq (numberp "17") nil)

;; parse-number
(= (parse-number "123.34") 123.34)
(= (parse-number "#XFACE") 64206)
(= (parse-number "#xFACE") 64206)
(= (parse-number "#o10") 8)
(= (parse-number "#O10") 8)
(= (parse-number "#b1010") 10)
(= (parse-number "#B1010") 10)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(parse-number "-37."))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
		(parse-number "-.5"))
  nil)

;; =
(not (= 3 4))
(= 3 3.0)
(= (parse-number "134.54") 134.54)
(= 0.0 -0.0)
(= 1 1)
(= 1 1.0)
(= 0 0.0)
(= 10.0 10)
(= 1.2 1.2)
(= 123 123)
(= 3.4 0.34e1)
(not (= 1 5))
(not (= 0 3))
(not (= 0 0.1))
(not (= 3.5 1))
(not (= 5.3 2.0))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (= 'a 1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (= "0" 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (= 1 #(0 1)))
  nil)

;; /=
(/= 3 4)
(not (/= 3 3.0))
(not (/= (parse-number "134.54") 134.54))
(not (/= 0.0 -0.0))
(not (/= 1 1))
(not (/= 1 1.0))
(not (/= 0 0.0))
(not (/= 10.0 10))
(not (/= 1.2 1.2))
(not (/= 123 123))
(not (/= 3.4 0.34e1))
(/= 1 5)
(/= 0 3)
(/= 0 0.1)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (/= 'a 1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (/= "0" 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (/= 1 #(0 1)))
  nil)

;; <
(< 3 4)
(not (< 3 3.0))
(< 3.5 5)
(< 3.5 5.6)
(not (< (parse-number "134.54") 134.54))
(not (< 0.0 -0.0))
(not (< 1 1))
(not (< 1 1.0))
(not (< 0 0.0))
(not (< 10.0 10))
(not (< 1.2 1.2))
(not (< 123 123))
(not (< 3.4 0.34e1))
(< 1 5)
(< 0 3)
(< 0 0.1)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (< 'a 1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (< "0" 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (< 1 #(0 1)))
  nil)

;; <=
(<= 3 4)
(<= 3 3.0)
(<= 3.5 5)
(<= 3.5 5.6)
(<= (parse-number "134.54") 134.54)
(<= 0.0 -0.0)
(<= 1 1)
(<= 1 1.0)
(<= 0 0.0)
(<= 10.0 10)
(<= 1.2 1.2)
(<= 123 123)
(<= 3.4 0.34e1)
(<= 1 5)
(<= 0 3)
(<= 0 0.1)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (<= 'a 1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (<= "0" 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (<= 1 #(0 1)))
  nil)

;; >
(> 5 3)
(> 5 3.0)
(> 5.0 1)
(> 4.3 2.0)
(not (> 3 4))
(not (> 3 3.0))
(not (> 3.5 5))
(not (> 3.5 5.6))
(not (> (parse-number "134.54") 134.54))
(not (> 0.0 -0.0))
(not (> 1 1))
(not (> 1 1.0))
(not (> 0 0.0))
(not (> 10.0 10))
(not (> 1.2 1.2))
(not (> 123 123))
(not (> 3.4 0.34e1))
(not (> 1 5))
(not (> 0 3))
(not (> 0 0.1))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (> 'a 1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (> "0" 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (> 1 #(0 1)))
  nil)

;; >=
(>= 5 3)
(>= 5 3.0)
(>= 5.0 1)
(>= 4.3 2.0)
(not (>= 3 4))
(>= 3 3.0)
(not (> 3.5 5))
(not (> 3.5 5.6))
(>= (parse-number "134.54") 134.54)
(>= 0.0 -0.0)
(>= 1 1)
(>= 1 1.0)
(>= 0 0.0)
(>= 10.0 10)
(>= 1.2 1.2)
(>= 123 123)
(>= 3.4 0.34e1)
(not (>= 1 5))
(not (>= 0 3))
(not (>= 0 0.1))
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (>= 'a 1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (>= "0" 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (>= 1 #(0 1)))
  nil)

;; max
(= (max 0 5 10) 10)
(= (max 0 10 9) 10)
(= (max -10) -10)
(= (max -10 -3 -1 -5) -1)
(= (max 1000 0 5 10) 1000)
(= (max 5.1 3.0 1000 5432.1 0 5 10) 5432.1)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (max))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (max 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (max 5 3 'a 1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (max 5 3 'a))
  nil)

;; min
(= (min 0) 0)
(= (min 5 4 -10 1) -10)
(= (min 3 2 1) 1)
(= (min 3.5 2.1 1.5) 1.5)
(= (min 3.5 10 2.1 1.5) 1.5)
(= (min -5 -2 -3 10 100) -5)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (min))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (min 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (min 5 3 'a 1))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (min 5 3 'a))
  nil)

;; +
(= (+ 12 3) 15)
(= (+ 1 2 3) 6)
(= (+ 12 3.0) 15.0)
(= (+ 4 0.0) 4.0)
(= (+) 0)
(= (+ 1) 1)
(= (+ 10) 10)
(= (+ 3.0 2 5.5) 10.5)
(= (+ 3.3 2 5.0 6.0) 16.3)
(= (+ -1 3) 2)
(= (+ -3 -5) -8)
(= (+ 5 -6) -1)
(= (+ -1) -1)
(= (+ -1 -5 10 2) 6)
(= (+ 10 10 10 10) 40)

;; *
(= (* 12 3) 36)
(= (* 12 3.0) 36.0)
(= (* 4.0 0) 0.0)
(= (* 2 3 4) 24)
(= (*) 1)
(= (* 3) 3)
(= (* 5.5) 5.5)
(= (* 2 -2) -4)
(= (* -2 -3) 6)
(= (* 1 2 3 4) 24)
(= (* 10 100) 1000)

;; -
(= (- 1) -1)
(= (- -4.0) 4.0)
(= (- 4.0) -4.0)
(eql (- 0.0) -0.0)
(eql (- -0.0) 0.0)
(= (- 12 3) 9)
(= (- 1 2 3) -4)
(= (- 12 3.0) 9.0)
(= (- 4 0.0) 4.0)
(= (- 1) -1)
(= (- 10) -10)
(= (- 3.0 2 5.5) -4.5)
(= (- 3.3 2 5.0 6.0) -9.7)
(= (- -1 3) -4)
(= (- -3 -5) 2)
(= (- 5 -6) 11)
(= (- -1) 1)
(= (- -1 -5 10 2) -8)
(= (- 10 10 10 10) -20)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (-))
  nil)


;; integerp
(integerp 0)
(integerp -10)
(integerp 100)
(not (integerp 3.4))
(not (integerp "4"))
(not (integerp '(a b c)))


;; div, mod
(eql (div 12 3) 4)
(eql (div 14 3) 4)
(eql (div -12 3) -4)
(eql (div -14 3) -5)
(eql (div 12 -3) -4)
(eql (div 14 -3) -5)
(eql (div -12 -3) 4)
(eql (div -14 -3) 4)
(eql (div 3 5) 0)
(eql (div 3 -5) 0)
(eql (div -3 5) 0)
(eql (div -3 -5) 0)

(eql (mod 12 3) 0)
(eql (mod 7 247) 7)
(eql (mod 247 7) 2)
(eql (mod 14 3) 2)
(eql (mod -12 3) 0)
(eql (mod -14 3) 1)
(eql (mod 12 -3) 0)
(eql (mod 14 -3) -1)
(eql (mod -12 -3) 0)
(eql (mod -14 -3) -2)

;; quotient reciprocal
(= (reciprocal 2) 0.5)
(= (reciprocal 4) 0.25)
(and (integerp (quotient 10 5))
     (= (quotient 10 5) 2))
(and (integerp (quotient 20 2))
     (= (quotient 20 2) 10))
(= (quotient 1 2) 0.5)
(= (quotient 2 -0.5) -4.0)
(= (quotient 10 5 2) 1)
(= (quotient 10 -5 -2) 1)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (quotient 0 0.0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (reciprocal 0))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (reciprocal))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (quotient 3 5 0 4))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (quotient))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (quotient 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (reciprocal 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (quotient 'a 'b))
  nil)

;; abs
(= (abs -3) 3)
(= (abs 2.0) 2.0)
(= (abs -0.0) 0.0)
(= (abs 3.4) 3.4)
(= (abs 5) 5)
(= (abs -100) 100)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (abs))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (abs 1 2))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (abs 'a))
  nil)

;; exp
(= (exp 0) 1)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (exp))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (exp 2 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (exp 'a))
  nil)

;; floor
(= (floor 1) 1)
(integerp (floor 1))
(= (floor 1.5) 1)
(integerp (floor 1.5))
(integerp (floor 1234.5))
(= (floor 3.9) 3)
(= (floor -1.5) -2)
(= (floor -10.9) -11)
(= (floor (exp 2)) 7)
(= (floor -1000) -1000)
(= (floor 1000) 1000)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (floor))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (floor 2 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (floor 'a))
  nil)

;; ceiling
(= (ceiling 1) 1)
(integerp (ceiling 1))
(= (ceiling 1.5) 2)
(integerp (ceiling 1.5))
(integerp (ceiling 1234.5))
(= (ceiling 3.9) 4)
(= (ceiling -1.5) -1)
(= (ceiling -10.9) -10)
(= (ceiling (exp 2)) 8)
(= (ceiling -1000) -1000)
(= (ceiling 1000) 1000)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (ceiling))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (ceiling 2 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (ceiling 'a))
  nil)

;; truncate
(= (truncate 1) 1)
(integerp (truncate 1))
(= (truncate 1.5) 1)
(integerp (truncate 1.5))
(integerp (truncate 1234.5))
(= (truncate 3.9) 3)
(= (truncate -1.5) -1)
(= (truncate -10.9) -10)
(= (truncate (exp 2)) 7)
(= (truncate -1000) -1000)
(= (truncate 1000) 1000)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (truncate))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (truncate 2 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (truncate 'a))
  nil)

;; round
(= (round 1) 1)
(integerp (round 1))
(= (round 1.5) 2)
(integerp (round 1.5))
(integerp (round 1234.5))
(= (round -3.5) -4)
(= (round -2.5) -2)
(= (round 3.9) 4)
(= (round -1.5) -2)
(= (round -10.9) -11)
(= (round (exp 2)) 7)
(= (round -1000) -1000)
(= (round 1000) 1000)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (round))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (round 2 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (round 'a))
  nil)

;; log
(= (log 2.718281828459045) 1)
(= (log 1) 0)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (log))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <arity-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (log 2 3))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <domain-error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (log 'a))
  nil)
(block a
  (with-handler (lambda (condition)
		  (if (instancep condition (class <error>))
		      (return-from a t)
		    (signal-condition condition nil)))
                (log -10))
  nil)

