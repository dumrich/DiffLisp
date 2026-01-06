;;;; operators.lisp
;;;; Differentiable operators

(in-package #:difflisp.ops)

;; Placeholder for differentiable operator definitions

;; Differentiable + operator operating on `dnumber` types
(defun + (&rest params)
  "Summation of a variable number of parameters with autograd"
  ;; Push operator struct to 
  (let ((sum (make-dnumber :value 0.0 :grad 0.0))
        (inputs '()))
    (loop for param in params
          do
             (if (dnumber-p param)
                 (push param inputs)
                 (push (make-dnumber :value param :grad 0.0) inputs))
          (setf (dnumber-value sum) (cl:+ (dnumber-value sum) (dnumber-value (car inputs)))))
    
    ;; Make operator
    (let ((op (make-operation :input inputs :output sum
                              :closure (lambda (parent inputs) (dnumber-grad parent)))))
        (push-to-tape op))
                    
    sum))
  
(defun * (&rest params)
  "Product of a variable number of parameters with autograd"
  (let ((product (make-dnumber :value 1.0 :grad 0.0))
        (inputs '()))
    (loop for param in params
          do
             (if (dnumber-p param)
                 (push param inputs)
                 (push (make-dnumber :value param :grad 0.0) inputs))
             (setf (dnumber-value product) (cl:* (dnumber-value product) (dnumber-value (car inputs)))))
          
    ;; Make operator
    (let ((op (make-operation :input inputs :output sum
                              :closure
                              (lambda (parent inputs)
                                (cl:* (apply #'cl:* inputs) (dnumber-grad parent))))))
      (push-to-tape op))
    product))
    

    

          
  

