#lang scheme


;;; Section 1.1
;;; -----------


;;; Exercise 1.1
;;; What are the results printed by the interpreter for each expression?

10
;; 10

(+ 5 3 4)
;; 12

(- 9 1)
;; 8

(/ 6 2)
;; 3

(+ (* 2 4) (- 4 6))
;; 6

(define a 3)

(define b (+ a 1))

(+ a b (* a b))
;; 19

(= a b)
;; #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; 16

(+ 2 (if (> b a) b a))
;; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; 16


;;; Exercise 1.2
;;; Convert equation to prefix form

;; infix: ((5 + 4 + (2 - ( 3 - (6 + (4/5))))) / (3 * (6 - 2) * (2 - 7))) = -37/150
;; prefix: (/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5)))))) (* 3 (* (- 6 2) (- 2 7))))
;;         = / + 5 + 4 - 2 - 3 + 6 / 4 5 * 3 * - 6 2 - 2 7

;; converting prefix to scheme will remove some of the repeated operators

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))
;; -37/150


;;; Exercise 1.3
;;; Define a procedure that takes three numbers and returns the sum of squares
;;; of largest two.

(define (sum-largest-two-squares x y z)
  (cond
   ((<= z x y) (+ (* x x) (* y y)))     ; if z is smallest return x^2 + y^2
   ((<= y x z) (+ (* x x) (* z z)))     ; if y is smallest return x^2 + z^2
   (else (+ (* y y) (* z z)))))         ; else x must be smallest so return y^2 + z^2


;;; Exercise 1.4
;;; Describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The if statement returns a procedure (either + or -) which will be used to
;; evaluate the following operands (a and b). If b is positive, the + operator
;; is returned and (+ a b) is evaluated, otherwise the - operator is returned
;; and (- a b) is evaluated. The result of that evaluation is returned from
;; the function.


;;; Exercise 1.5
;;; What behavior will be observed for the following procedures/evaluations if
;;; the interpreter uses applicative-order evaluation or normal-order
;;; evaluation?

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; If the interpreter uses applicative-order evaluation, this will result in
;; an infinite loop. (p) will be evaluated before (test 0 (p)), which will
;; call itself as (p) continually until the program is killed.

;; If the interpreter uses normal-order evaluation, then the arguments passed
;; to (test) won't be evaluated until the procedure has fully expanded. So
;; (test 0 (p)) will not evaluate (p) immediately, but will instead begin
;; executing the (test) procedure. Since x is 0, 0 is returned and (p) is never
;; evaluated.

;; This test determines the order evaluation of the interpreter.
