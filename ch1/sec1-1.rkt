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
