;;
;;
;;

(defglobal test-files
    '("test/test_read.lisp"
      "test/test_symbol.lisp"
      "test/test_cons.lisp"
      "test/test_control.lisp"
      "test/test_predicate.lisp"
      "test/test_string.lisp"
      "test/test_sequence.lisp"
      "test/test_array.lisp"
      "test/test_number.lisp"
      "test/test_stream.lisp"))

(defun test-file (name)
  (let* ((file (open-input-file name))
	 (form nil)
         (count 0))
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
      (setq count (+ count 1))
      (setq form (read file nil 'eof)))
    (format (standard-output) "OK (~A)~%" count))
  nil)

(while test-files
  (test-file (car test-files))
  (setq test-files (cdr test-files)))

