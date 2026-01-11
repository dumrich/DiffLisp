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
(defstruct dnumber
  (value 0.0d0 :type double-float)
  (grad 0.0d0 :type double-float))

(defmethod print-object ((d dnumber) stream)
  (format stream "#<DNUM val:~5,2f grad:~5,2f>"
          (dnumber-value d)
          (dnumber-grad d)))

;; Operation struct
(defstruct operation
  (closure nil :type (or function null)))

(defmethod print-object ((o operation) stream)
  (format stream "#<OP closure:~s>"
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

(defun pop-from-tape ()
  (vector-pop *tape*))

(defun tape-size ()
  (fill-pointer *tape*))
