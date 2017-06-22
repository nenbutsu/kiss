
(eql 6 (block x 
	 (+ 10 (return-from x 6) 22)))

(eq 'exit (labels ((f2 (g)
		       (funcall g))
		   (f1 ()
		       (block b
			 (let ((f (lambda () (return-from b 'exit))))
			   (f2 f)))))
	    (f1)))
