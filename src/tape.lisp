;;;; tape.lisp
;;;; Tape-based automatic differentiation infrastructure

(in-package #:difflisp.tape)

;; Implementation plan:
;; Tape Struct
;; - Stack of operations (struct)
;; - Private Methods:
;;   - (add-to-tape operation)
;; Operation Struct
;; - Stores a list of input Values
;; - Stores an output Value
;; - Stores a gradient closure. Updates each input with respect to the inputs
;; Value Struct
;; - Stores double-float value (initialized to zero)
;; - Stores double-float gradient (initialized to 0.0)

;; Generic value
(defstruct (value
            (:print-function print-variable))
    (val 0.0)
    (grad 0.0))

(defun print-variable (v stream depth)
  (declare (ignore depth))
  (format stream "#<VAR val:~5,2f grad:~5,2f>"
          (variable-val v)
          (variable-grad v)))


