;;;; package.lisp

(defpackage #:difflisp.tape
  (:use #:cl)
  (:export #:dnumber #:make-dnumber #:dnumber-value #:dnumber-grad
           #:operation #:reset-tape #:push-to-tape #:pop-from-tape))

(defpackage #:difflisp.ops
  (:use #:cl #:difflisp.tape)
  (:shadow #:* #:+ #:- #:/ #:sin #:cos)
  (:export #:* #:+ #:- #:/ #:sin #:cos))
  
(defpackage #:difflisp.diff
  (:use #:cl #:difflisp.tape)
  (:export #:grad #:backward))
  

(defpackage #:difflisp
    (:use #:cl #:difflisp.tape #:difflisp.ops #:difflisp.diff))
