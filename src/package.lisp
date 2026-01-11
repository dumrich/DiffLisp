;;;; package.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *tensor-ops* '(#:+ #:- #:* #:/ #:sin #:cos)))

(defpackage #:difflisp.tape
  (:use #:cl)
  (:export #:dnumber #:make-dnumber #:dnumber-p #:dnumber-value #:dnumber-grad
           #:operation #:make-operation #:operation-closure
           #:reset-tape #:push-to-tape #:pop-from-tape #:tape-size))

(defpackage #:difflisp.ops
  (:use #:cl #:difflisp.tape)
  (:shadow . #.*tensor-ops*)
  (:export . #.*tensor-ops*))

(defpackage #:difflisp.diff
  (:use #:cl #:difflisp.tape)
  (:export #:grad #:backward))
  

(defpackage #:difflisp
  (:use #:cl #:difflisp.tape #:difflisp.ops #:difflisp.diff)
  (:shadowing-import-from #:difflisp.ops . #.*tensor-ops*))
