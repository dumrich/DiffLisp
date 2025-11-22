;;;; package.lisp

(defpackage #:difflisp
  (:use #:cl)
  (:export #:diff-lambda
           #:gradient
           #:diff
           #:make-variable
           #:value
           #:grad))
