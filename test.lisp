;;
;;
;;

(defglobal test-files
  '("test_symbol.lisp"
    "test_cons.lisp"
    "test_control.lisp"
    "test_predicate.lisp"
    "test_string.lisp"
    "test_sequence.lisp"
    "test_array.lisp"
    "test_number.lisp"
    "test_stream.lisp"))

(defun test-file (name)
  (let* ((file (open-input-file name))
	 (form nil))
    (format (standard-output) "~%Testing ~S ..." name)
    (setq form (read file nil 'eof))
    (while (not (eq form 'eof))
      (with-handler (lambda (c)
		      (format (standard-output) "Unhandled error signaled: ~S~%" form)
		      (signal-condition c nil))
                    (if (null (eval form))
                        (progn
                          (format (standard-output) "NIL returned. test: ~S~%" form)
                          (return-from test-file nil))))
      (setq form (read file nil 'eof)))
    (format (standard-output) "OK~%"))
  nil)

(while test-files
  (test-file (car test-files))
  (setq test-files (cdr test-files)))

