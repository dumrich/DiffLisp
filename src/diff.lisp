;;;; diff.lisp
;;;; Automatic differentiation core

(in-package #:difflisp.diff)

(defun backward ()
    (declare (optimize (debug 1))) ;; Low debug enables TCO
    (if (zerop (tape-size))
      nil
      (let ((op (pop-from-tape)))
        (funcall (operation-closure op))
        (backward))))
  
  
  
(defun grad (f)
  (lambda (&rest args)
    ;; Collect arguments into list of dnumbers
    (reset-tape)
    (let* ((nargs (mapcar #'(lambda (x)
                              (if (dnumber-p x)
                                  x
                                  (make-dnumber :value (coerce x 'double-float)))) args))
           (out (apply f nargs)))

        ;; Set gradient
      (if (dnumber-p out)
        (progn
            (setf (dnumber-grad out) 1.0d0)
            (backward)
            (mapcar #'(lambda (x) (dnumber-grad x)) nargs))
        (mapcar (constantly 0.0d0) args)))))
