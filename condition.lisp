;;; condition.lisp --- defines the condition handling mechanism of ISLisp processor KISS.

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


(defdynamic *kiss::handlers* nil)

;; special operator: (IGNORE-ERRORS FORM*) -> <object>
;;   Establishes a handler for <error>, such that if an error occurs during
;;   execution of FORMs, ignore-errors will immediately return nil. Then it
;;   executes FORMs, returning the value returned by the last form (or nil
;;   if there were no FORMs) if execution terminates normally.
(defmacro ignore-errors (&rest forms)
  (let ((block-name (gensym)))
  `(block ,block-name
     (with-handler (lambda (condition)
                     (if (instancep condition (class <error>))
                         (return-from ,block-name nil)
                       (signal-condition condition
                                         (condition-continuable condition))))
       nil
       ,@forms))))


;; function: (ERROR ERROR-STRING OBJ*) -> <object>
;;   An error shall be signaled. ERROR-STRING and the OBJs are advice to
;;   the implementation about how the error message might be textually
;;   described (using format), but whether or not that advice is used is
;;   implementation defined.
(defun error (error-string &rest obj*)
  (signal-condition (create (class <simple-error>)
                            'format-string error-string
                            'format-arguments obj*)
                    nil))

;; function: (CERROR CONTINUE-STRING ERROR-STRING OBJ*) -> <object>
;;   Like error, but the error that it signals is “continuable” (see
;;   continue-condition). The extra argument CONTINUE-STRING describes
;;   what happens if this function returns.
(defun cerror (continue-string error-string obj*)
  (signal-condition (create (class <simple-error>)
                            'format-string error-string
                            'format-arguments (list obj*))
                    (let ((str (create-string-output-stream)))
                      (format str continue-string obj*)
                      (get-output-stream-string str))))

;; function: (SIGNAL-CONDITION CONDITION CONTINUABLE) -> <object>
;;   Invokes the condition handling system on CONDITION. If CONTINUABLE is
;;   nil, the results of attempting to “continue” (see
;;   continue-condition) are not defined except that the call to
;;   signal-condition will not return normally.  If CONTINUABLE is not nil,
;;   it will be possible to return from the call to signal-condition (see
;;   continue-condition). In this case, the specific value of CONTINUABLE
;;   may be a string indicating the effect of continuing, or it may be the
;;   symbol t, indicating that an implementation-defined
;;   string such as "Continue with no special action." is to be used.
(defun signal-condition (condition continuable)
  (if (eq continuable t)
      (setq continuable "Continue with no special action"))
  (set-condition-continuable continuable condition)
  (catch 'kiss::continue
    (let ((handlers (dynamic *kiss::handlers*)))
      (if handlers
          (let ((handler (car handlers)))
            (set-dynamic (cdr handlers) *kiss::handlers*)
            (funcall handler condition))))
    (let ((string-stream (create-string-output-stream)))
      (report-condition condition string-stream)
      (kiss::err "~S" (get-output-stream-string string-stream)))))

;; special operator: (with-handler handler form*) → <object>
;;  Evaluates handler, which must yield a function (called the “handler
;;  function”). The handler function is established as active handler
;;  (see §29.2) and then the forms are executed. If execution of forms
;;  finishes normally, the value of the last form (or nil if there are no
;;  forms) is returned.
(defmacro with-handler (handler &rest form*)
  `(dynamic-let ((*kiss::handlers* (cons ,handler (dynamic *kiss::handlers*))))
     nil
     ,@form*))

;; function: (continue-condition condition [value]) transfers control and data
;;  “Continues” from condition by finding the call to signal-condition
;;  and arranging for it to perform a normal return of the value, which
;;  defaults to nil.
;;  The consequences are undefined if the condition is not continuable.
(defun continue-condition (condition &rest args)
  (let ((value (if args
                   (car args)
                 nil)))
    (throw 'kiss::continue value)))



;; special operator: (assure class-name form) -> <object>
;;  Evaluate form. If form returns, the returned value is returned.
;;  An error shall be signaled if the value of form is not of the class
;;  or a subclass of the class designated by class-name
;;  (error-id. domain-error).
(defmacro assure (class-name form)
  `(kiss::assure ',class-name ,form))
(defun kiss::assure (class-name value)
  (let ((class (kiss::class class-name)))
    (if (instancep value class)
        value
      (signal-condition (create (class <domain-error>)
                                'object value
                                'expected-class class)
                        nil))))

;; generic function (report-condition condition stream) → <condition>
;;   Presents a natural language description of condition to stream. This
;;   generic function may be specialized for user-defined condition
;;   classes.
(defgeneric report-condition (condition stream))

(defmethod report-condition ((condition <serious-condition>) (stream <stream>))
  (format stream "Serious condition")
  condition)

(defmethod report-condition ((condition <storage-exhausted>) (stream <stream>))
  (format stream "Storage exhausted")
  condition)

(defmethod report-condition ((condition <error>) (stream <stream>))
  (format stream "Error")
  condition)

(defmethod report-condition ((condition <simple-error>) (stream <stream>))
  (apply #'format stream
         (simple-error-format-string condition)
         (simple-error-format-arguments condition))
  condition)

(defmethod report-condition ((condition <stream-error>) (stream <stream>))
  (format stream "Stream error ~S" (stream-error-stream condition))
  condition)

(defmethod report-condition ((condition <end-of-stream>) (stream <stream>))
  (format stream "End of stream error ~S" (stream-error-stream condition))
  condition)

(defmethod report-condition ((condition <arithmetic-error>) (stream <stream>))
  (format stream "Arithmetic error ~S ~S"
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition))
  condition)

(defmethod report-condition ((condition <division-by-zero>) (stream <stream>))
  (format stream "Division by zero error ~S ~S"
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition))
  condition)

(defmethod report-condition ((condition <floating-point-overflow>)
                             (stream <stream>))
  (format stream "Floating point overflow error ~S ~S"
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition))
  condition)

(defmethod report-condition ((condition <floating-point-underflow>)
                             (stream <stream>))
  (format stream "Floating point overflow error ~S ~S"
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition))
  condition)

(defmethod report-condition ((condition <control-error>) (stream <stream>))
  (format stream "Tag ~S not found for ~S"
          (control-error-name condition)
          (control-error-namespace condition))
  condition)

(defmethod report-condition ((condition <parse-error>) (stream <stream>))
  (format stream "Parse error ~S expected ~S"
          (parse-error-string condition)
          (parse-error-expected-class condition))
  condition)

(defmethod report-condition ((condition <program-error>) (stream <stream>))
  (format stream "Program error")
  condition)

(defmethod report-condition ((condition <index-out-of-range>) (stream <stream>))
  (format stream "Index out of range error ~S ~S"
          (index-out-of-range-sequence condition)
          (index-out-of-range-index condition))
  condition)

(defmethod report-condition ((condition <domain-error>) (stream <stream>))
  (if (error-id condition)
      (format stream "Domain error ~S is not ~S"
          (domain-error-object condition)
          (error-id condition))
    (format stream "Domain error ~S is not ~S"
            (domain-error-object condition)
            (class-name (domain-error-expected-class condition))))
  condition)

(defmethod report-condition ((condition <undefined-entity>) (stream <stream>))
  (format stream "Undefined entity ~S not defined in namespace ~S"
          (undefined-entity-name condition)
          (undefined-entity-namespace condition))
  condition)

(defmethod report-condition ((condition <unbound-variable>) (stream <stream>))
  (format stream "Unbound variable ~S" (undefined-entity-name condition))
  condition)

(defmethod report-condition ((condition <undefined-function>) (stream <stream>))
  (format stream "Undefined function ~S" (undefined-entity-name condition))
  condition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kiss::signal-non-negative-integer (integer continuable)
  (signal-condition (create (class <domain-error>)
				  'object integer
				  'expected-class (class <non-negative-integer>))
		    continuable))
(defun kiss::signal-end-of-stream (stream continuable)
  (signal-condition (create (class <end-of-stream>) 'stream stream) continuable))

(defun kiss::signal-unbound-variable (name continuable)
  (signal-condition (create (class <unbound-variable>) 'name name 'namespace 'variable)
		    continuable))

(defun kiss::signal-catcher-not-found (name continuable)
  (signal-condition (create (class <control-error>) 'name name 'namespace 'catch) continuable))

(defun kiss::signal-block-not-found (name continuable)
  (signal-condition (create (class <control-error>) 'name name 'namespace 'block) continuable))

(defun kiss::signal-tagbody-not-found (name continuable)
  (signal-condition (create (class <control-error>) 'name name 'namespace 'tagbody) continuable))

(defun kiss::signal-index-out-of-range (sequence index continuable)
  (signal-condition (create (class <index-out-of-range>) 'sequence sequence 'index index)
                    continuable))



(provide 'condition)
;;;
