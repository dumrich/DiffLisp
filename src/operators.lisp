;;;; operators.lisp
;;;; Differentiable operators

(in-package #:difflisp.ops)

;; Placeholder for differentiable operator definitions

;; Differentiable + operator operating on `dnumber` types
(defgeneric binary-add (a b)
  (:documentation "Summation of 2 params with autograd"))

(defmethod binary-add ((a dnumber) (b dnumber))
  (let* ((a-val (dnumber-value a))
         (b-val (dnumber-value b))
         (output (make-dnumber :value (cl:+ a-val b-val))))
    (let ((back-fn (lambda ()
                     (let ((out-grad (dnumber-grad output)))
                       (incf (dnumber-grad a) out-grad)
                       (incf (dnumber-grad b) out-grad)))))
      (push-to-tape (make-operation :closure back-fn)))
    output))
      
  
(defmethod binary-add ((a number) (b dnumber))
  (let ((a-dnum (make-dnumber :value (coerce a 'double-float))))
    (binary-add a-dnum b)))

(defmethod binary-add ((a dnumber) (b number))
  (let ((b-dnum (make-dnumber :value (coerce b 'double-float))))
    (binary-add a b-dnum)))

(defmethod binary-add ((a number) (b number))
  (cl:+ a b))

(defun + (&rest args)
  (if (null args)
      (make-dnumber :value 0.0d0)
      (reduce #'binary-add args)))
  
;; Multiplication
(defgeneric binary-mul (a b)
  (:documentation "Product of 2 params with autograd"))

(defmethod binary-mul ((a dnumber) (b dnumber))
  (let* ((a-val (dnumber-value a))
         (b-val (dnumber-value b))
         (output (make-dnumber :value (cl:* a-val b-val))))
    (let ((back-fn (lambda ()
                     (let ((out-grad (dnumber-grad output)))
                       (incf (dnumber-grad a) (cl:* out-grad b-val))
                       (incf (dnumber-grad b) (cl:* out-grad a-val))))))
      (push-to-tape (make-operation :closure back-fn)))
    output))

(defmethod binary-mul ((a number) (b dnumber))
  (let ((a-dnum (make-dnumber :value (coerce a 'double-float))))
    (binary-mul a-dnum b)))

(defmethod binary-mul ((a dnumber) (b number))
  (let ((b-dnum (make-dnumber :value (coerce b 'double-float))))
    (binary-mul a b-dnum)))

(defmethod binary-mul ((a number) (b number))
    (cl:* a b))

(defun * (&rest args)
  (if (null args)
      (make-dnumber :value 1.0d0)
      (reduce #'binary-mul args)))

;; Subtraction
(defgeneric binary-sub (a b)
  (:documentation "Difference of 2 params with autograd"))

(defmethod binary-sub ((a dnumber) (b dnumber))
  (let* ((a-val (dnumber-value a))
         (b-val (dnumber-value b))
         (output (make-dnumber :value (cl:- a-val b-val))))
    (let ((back-fn (lambda ()
                     (let ((out-grad (dnumber-grad output)))
                       (incf (dnumber-grad a) out-grad)
                       (incf (dnumber-grad b) (cl:* -1 out-grad))))))
      (push-to-tape (make-operation :closure back-fn)))
    output))

(defmethod binary-sub ((a number) (b dnumber))
  (binary-sub (make-dnumber :value (coerce a 'double-float)) b))

(defmethod binary-sub ((a dnumber) (b number))
  (binary-sub a (make-dnumber :value (coerce b 'double-float))))

(defmethod binary-sub ((a number) (b number))
  (cl:- a b))

(defun - (first &rest others)
  (if (null others)
      ;; Case: Unary Negation (- x) -> (0 - x)
      (binary-sub (make-dnumber :value 0.0d0) first)
      ;; Case: N-ary Subtraction
      (reduce #'binary-sub others :initial-value first)))
    
;; Division
;; Rename generic
(defgeneric binary-div (a b)
  (:documentation "Division of 2 params with autograd"))

(defmethod binary-div ((a dnumber) (b dnumber))
  (let* ((a-val (dnumber-value a))
         (b-val (dnumber-value b))
         (output (make-dnumber :value (cl:/ a-val b-val))))
    (let ((back-fn (lambda ()
                     (let ((out-grad (dnumber-grad output)))
                       ;; d(a/b)/da = 1/b
                       (incf (dnumber-grad a) (cl:* (cl:/ 1 b-val) out-grad))
                       ;; d(a/b)/db = -a/b^2
                       (let ((neg-a-over-b2 (cl:/ (cl:- a-val) 
                                                  (cl:* b-val b-val))))
                         (incf (dnumber-grad b) (cl:* neg-a-over-b2 out-grad)))))))
      (push-to-tape (make-operation :closure back-fn)))
    output))

(defmethod binary-div ((a number) (b dnumber))
  (binary-div (make-dnumber :value (coerce a 'double-float)) b))

(defmethod binary-div ((a dnumber) (b number))
  (binary-div a (make-dnumber :value (coerce b 'double-float))))

(defmethod binary-div ((a number) (b number))
  (cl:/ a b))

(defun / (first &rest others)
  (if (null others)
      ;; Case: Reciprocal (/ x) -> (1 / x)
      (binary-div (make-dnumber :value 1.0d0) first)
      ;; Case: N-ary Division
      (reduce #'binary-div others :initial-value first)))

;; Sin, Cos
(defgeneric sin (x)
  (:documentation "Compute the sin of a value in radians"))

(defmethod sin ((x dnumber))
  (let* ((x-val (dnumber-value x))
         (output (make-dnumber :value (cl:sin x-val))))
    (let ((back-fn (lambda ()
                     (let ((output-grad (dnumber-grad output)))
                       (incf (dnumber-grad x) (cl:* output-grad (cl:cos x-val)))))))
      (push-to-tape (make-operation :closure back-fn)))
    output))
                       
(defmethod sin ((x number))
  (cl:sin x))

(defgeneric cos (x)
  (:documentation "Compute the cos of a value in radians"))

(defmethod cos ((x dnumber))
  (let* ((x-val (dnumber-value x))
         (output (make-dnumber :value (cl:cos x-val))))
    (let ((back-fn (lambda ()
                     (let ((output-grad (dnumber-grad output)))
                       (incf (dnumber-grad x) (cl:* -1.0d0 output-grad (cl:sin x-val)))))))
      (push-to-tape (make-operation :closure back-fn)))
    output))
                       
(defmethod cos ((x number))
  (cl:cos x))

;; Tensor Operations

;; exp, log, max, min, sqrt
;; Sigmoid, Tahn, Relu
