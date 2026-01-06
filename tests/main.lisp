;;;; tests/main.lisp

(defpackage #:difflisp/tests
  (:use #:cl #:difflisp #:fiveam))

(in-package #:difflisp/tests)

(def-suite difflisp-tests
  :description "Test suite for DiffLisp")

(in-suite difflisp-tests)

;; Placeholder for tests

;; Test Value
;; Test Operation
;; Test Tape

;; Test Gradient (default first arg)
(defun f (x y)
  x + y)

(defun g (x y)
  (* (f x y) 3))

(test add-mul-grad
    """Test adding and multiplying w.r.t x"""
      (let (g-prime (grad #'g))
          (is (g-prime 1 2) 3)))

        
;; Test Gradient (custom first arg)
(test add-mul-grad-named
    """Test adding and multiplying w.r.t x (named)"""
      (let (g-prime (grad #'g :arg '(0 1)))
          (is (g-prime 1 2) '(3 3))))
