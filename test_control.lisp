;; defglobal
(and (eq (defglobal x 0) 'x)
     (eql (let ((x 1)) x) 1)
     (eql x 0))
(and (eq (defglobal x 2) 'x)
     (eql (+ x 1) 3)
     (eql (setq x 4) 4)
     (eql (+ x 1) 5)
     (eql (let ((x 1)) (setq x 2) x) 2)
     (eql (+ x 1) 5))


;; let let*
(eql (let ((x 2)
	   (y 3))
       (let* ((x 7)
	      (z (+ x y)))
	 (* z x)))
     70)

(equal (let ((x 1)
	     (y 2))
	 (let* ((x y)
		(y x))
	   (list x y)))
       '(2 2))

;; dynamic-let
(and (eq (defun foo (x)
	   (dynamic-let ((y x))
			(bar 1)))
	 'foo)
     (eq (defun bar (x)
	   (+ x (dynamic y)))
	 'bar)
     (eql (foo 2) 3))

;; if
(eq (if (> 3 2) 'yes 'no) 'yes)
(eq (if (> 2 3) 'yes 'no) 'no)
(eq (if (> 2 3) 'yes) nil)
(eql (if(> 3 2)
	 (- 3 2)
       (+ 3 2))
     1)
(eql (let ((x 7))
       (if (< x 0) x (- x)))
     -7)

;; cond
(eq (cond ((> 3 2) 'greater)
	  ((< 3 2) 'less))
    'greater)
(eq (cond ((> 3 3) 'greater)
	  ((< 3 3) 'less))
    'nil)
(eq (cond ((> 3 3) 'greater)
	  ((< 3 3) 'less)
	  (t       'equal))
    'equal)

;; case case-using
(eq (case (* 2 3)
      ((2 3 5 7)
       'prime)
      ((4 6 8 9)
       'composite))
    'composite)
(eq (case (car '(c d))
      ((a) 'a)
      ((b) 'b))
    'nil)
(eq (case (car '(c d))
      ((a e i o u)
       'vowel)
      ((y) 'semivowel)
      (t 'consonant))
    'consonant)
(eq (let ((char #\u))
      (case char
	((#\a #\e #\o #\u #\i)
	 'vowels)
	(t 'consonants)))
    'vowels)

(eq (case-using #'= (+ 1.0 1.0)
		((1) 'one)
		((2) 'two)
		(t 'more))
    'two)
(eql (case-using #'string= "bar"
		 (("foo") 1)
		 (("bar") 2))
     2)


;; progn
(and (eq (defglobal x 0) 'x)
     (eql (progn
	    (setq x 5)
	    (+ x 1))
	  6))
;; while
(equal (let ((x '())
	     (i 5))
	 (while (> i 0) (setq x (cons i x)) (setq i (- i 1)))
	 x)
       '(1 2 3 4 5))

;; for
(eql (let ((x '(1 3 5 7 9)))
       (for ((x x (cdr x))
	     (sum 0 (+ sum (car x))))
	    ((null x) sum)))
     25)

;;(equal (for ((vec (vector 0 0 0 0 0))
;;	     (i 0 (+ i 1)))
;;	    ((= i 5) vec)
;;	    (setf (elt vec i) i))
;;       #(0 1 2 3 4))

(eql 6 (block x 
	 (+ 10 (return-from x 6) 22)))

(eq 'exit (labels ((f2 (g)
		       (funcall g))
		   (f1 ()
		       (block b
			 (let ((f (lambda () (return-from b 'exit))))
			   (f2 f)))))
	    (f1)))

