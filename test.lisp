;;
;;
;;

(defglobal test-files
  '("test_symbol.lisp"
    "test_cons.lisp"
    "test_control.lisp"
    "test_predicate.lisp"))

(defun test-file (name)
  (let* ((file (open-input-file name))
	 (form nil))
    (format (standard-output) "~%Testing ~S~%" name)
    (setq form (read file nil 'eof))
    (while (not (eq form 'eof))
      (with-handler (lambda (c)
		      (format (standard-output) "Unhandled error signaled: ~S~%" form)
		      (signal-condition c nil))
		    (eval form))
      (setq form (read file nil 'eof)))
    (if (not (eq form 'eof))
	(format (standard-output) "NIL returned. test: ~S~%" form))
    (format (standard-output) "OK~%"))
  nil)

(while test-files
  (test-file (car test-files))
  (setq test-files (cdr test-files)))

