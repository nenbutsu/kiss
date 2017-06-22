;;
;;
;;

(defglobal test-files
  '("test_cons.lisp"
    "test_control.lisp"))

(while test-files
  (let* ((name (car test-files))
	 (file (open-input-file name))
	 (form nil))
    (format (standard-output) "~%Testing ~S~%" name)
    (setq form (read file nil 'eof))
    (while (and (not (eq form 'eof)) (eval form))
      ;;(format (standard-output) "~S => t~%" form)
      (setq form (read file nil 'eof)))
    (if (not (eq form 'eof))
	(format (standard-output) "NIL returned. test: ~S" form))
    (format (standard-output) "OK~%"))
  (setq test-files (cdr test-files)))

