;;
;;
;;

(defglobal test-files
    '("test/test_var.lisp"
      "test/test_read.lisp"
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
         (count 0)
         (errors 0))
    (format (standard-output) "~%Testing ~S ..." name)
    (setq form (read file nil 'eof))
    (while (not (eq form 'eof))
      (block t
        (with-handler (lambda (c)
                        (format (standard-output) "~%Unhandled error: ~S signaled: ~S~%" c form)
                        (setq errors (+ errors 1))
                        (return-from t nil))
          (if (null (eval form))
              (progn
                (format (standard-output) "~%NIL returned. test: ~S~%" form)
                (return-from t nil)))))
      (setq count (+ count 1))
      (setq form (read file nil 'eof)))
    (if (= errors 0)
        (format (standard-output) "~50TOK (0 errors / ~A)" count)
        (format (standard-output) "~50T~A errors / ~S" errors count))))

(while test-files
  (test-file (car test-files))
  (setq test-files (cdr test-files)))

