;;;; difflisp.asd

(asdf:defsystem #:difflisp
  :description "An automatically differentiable subset of Lisp in Common Lisp"
  :author "Abhinav Chavali <abhinavchavali12@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "tape")
                 (:file "operators")
                 (:file "diff")
                 (:file "core"))))
  :in-order-to ((test-op (test-op #:difflisp/tests))))

(asdf:defsystem #:difflisp/tests
  :description "Test suite for DiffLisp"
  :author "Abhinav Chavali <abhinavchavali12@gmail.com>"
  :license  "MIT"
  :depends-on (#:difflisp
               #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :difflisp-tests)))
