;;;; operators.lisp
;;;; Differentiable operators

(in-package #:difflisp.ops)

;; Placeholder for differentiable operator definitions

;; Differentiable + operator operating on `dnumber` types
(defgeneric + (a b)
  (:documentation "Summation of 2 params with autograd"))

(defmethod + ((a dnumber) (b dnumber))
  (let* ((a-val (dnumber-value a))
         (b-val (dnumber-value b))
         (output (make-dnumber :value (cl:+ a-val b-val))))
    (let ((back-fn (lambda ()
                     (let ((out-grad (dnumber-grad output)))
                       (incf (dnumber-grad a) out-grad)
                       (incf (dnumber-grad b) out-grad)))))
      (push-to-tape (make-operation :closure back-fn)))
    output))
      
  
(defmethod + ((a number) (b dnumber))
  (let ((a-dnum (make-dnumber :value a)))
    (+ a-dnum b)))
  
(defmethod + ((a dnumber) (b number))
  (let ((b-dnum (make-dnumber :value b)))
    (+ a b-dnum)))

(defmethod + ((a number) (b number))
  (cl:+ a b))
  
;; Multiplication
(defgeneric * (a b)
  (:documentation "Product of 2 params with autograd"))

(defmethod * ((a dnumber) (b dnumber))
  (let* ((a-val (dnumber-value a))
         (b-val (dnumber-value b))
         (output (make-dnumber :value (cl:* a-val b-val))))
    (let ((back-fn (lambda ()
                     (let ((out-grad (dnumber-grad output)))
                       (incf (dnumber-grad a) (cl:* out-grad b-val))
                       (incf (dnumber-grad b) (cl:* out-grad a-val))))))
      (push-to-tape (make-operation :closure back-fn)))
    output))

(defmethod * ((a number) (b dnumber))
  (let ((a-dnum (make-dnumber :value a)))
    (* a-dnum b)))

(defmethod * ((a dnumber) (b number))
  (let ((b-dnum (make-dnumber :value b)))
    (* a b-dnum)))

(defmethod * ((a number) (b number))
    (cl:* a b))


;; Subtraction
(defgeneric - (a b)
  (:documentation "Difference of 2 params with autograd"))

(defmethod - ((a dnumber) (b dnumber))
  (let* ((a-val (dnumber-value a))
         (b-val (dnumber-value b))
         (output (make-dnumber :value (cl:- a-val b-val))))
    (let ((back-fn (lambda ()
                     (let ((out-grad (dnumber-grad output)))
                       (incf (dnumber-grad a) out-grad)
                       (incf (dnumber-grad b) (cl:* -1 out-grad))))))
      (push-to-tape (make-operation :closure back-fn)))
    output))
      
  
(defmethod - ((a number) (b dnumber))
  (let ((a-dnum (make-dnumber :value a)))
    (- a-dnum b)))
  
(defmethod - ((a dnumber) (b number))
  (let ((b-dnum (make-dnumber :value b)))
    (- a b-dnum)))

(defmethod - ((a number) (b number))
  (cl:- a b))
    
;; Division
(defgeneric / (a b)
  (:documentation "Division of 2 params with autograd"))

(defmethod / ((a dnumber) (b dnumber))
  (let* ((a-val (dnumber-value a))
         (b-val (dnumber-value b))
         (output (make-dnumber :value (cl:/ a-val b-val))))
    (let ((back-fn (lambda ()
                     (let ((out-grad (dnumber-grad output)))
                       (incf (dnumber-grad a) (cl:* (cl:/ 1 b-val) out-grad))

                       (let ((neg-a-over-b2 (cl:/ (cl:- a-val) 
                                                  (cl:* b-val b-val))))
                         (incf (dnumber-grad b) 
                               (cl:* neg-a-over-b2 out-grad)))))))
      (push-to-tape (make-operation :closure back-fn)))
    output))
      
  
(defmethod / ((a number) (b dnumber))
  (let ((a-dnum (make-dnumber :value a)))
    (/ a-dnum b)))
  
(defmethod / ((a dnumber) (b number))
  (let ((b-dnum (make-dnumber :value b)))
    (/ a b-dnum)))

(defmethod / ((a number) (b number))
  (cl:/ a b))
