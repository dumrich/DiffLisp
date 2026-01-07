;;;; package.lisp

(defpackage #:difflisp.tape
  (:use #:cl)
  (:export #:number #:make-number #:number-value #:number-grad
           #:operation #:reset-tape #:push-to-tape))

(defpackage #:difflisp.ops
  (:use #:cl #:difflisp.tape)
  (:shadow #:* #:+ #:- #:/)
  (:export #:* #:+ #:- #:/))
  

(defpackage #:difflisp
    (:use #:cl #:difflisp.tape #:difflisp.ops))
