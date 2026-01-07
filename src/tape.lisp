;;;; tape.lisp
;;;; Tape-based automatic differentiation infrastructure

(in-package #:difflisp.tape)

;; Implementation plan:
;; Tape Variable
;; - Stack of operations
;; Operation Struct
;; - Stores a list of input Values
;; - Stores an output Value
;; - Stores a gradient closure. Updates each input with respect to the inputs
;; Value Struct
;; - Stores double-float value (initialized to zero)
;; - Stores double-float gradient (initialized to 0.0)

;; Generic dual-number
(defstruct (dnumber
            (:constructor %make-dnumber)
            (:print-function print-dnumber))
    (value 0.0)
    (grad 0.0))

(defun make-dnumber (&key (value 0.0d0) (grad 0.0d0))
  (%make-dnumber :value (coerce value 'double-float)
                 :grad (coerce grad 'double-float)))

(defun print-dnumber (d stream depth)
  (declare (ignore depth))
  (format stream "#<DNUM val:~5,2f grad:~5,2f>"
          (dnumber-value v)
          (dnumber-grad v)))

;; Operation struct
(defstruct (operation
            (:print-function print-operation))
  (closure nil :type (or function null)))

(defun print-operation (o stream depth)
  (declare (ignore depth))
  (format stream "#<OP input:~a output:~a closure:~s>"
          (operation-input o)
          (operation-output o)
          (operation-closure o)))

(defparameter *tape* (make-array 1000
                                 :element-type 'operation
                                 :adjustable t
                                 :fill-pointer 0))

;; Reset tape. Called on every (grad)
(defun reset-tape ()
  (setf (fill-pointer *tape*) 0))

(defun push-to-tape (op)
  (vector-push-extend op *tape*))
