#lang scheme

;;; ===========
;;; Section 1.1
;;; ===========


;;; Exercise 1.1
;;; ------------
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
;;; ------------
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
;;; ------------
;;; Define a procedure that takes three numbers and returns the sum of squares
;;; of largest two.

(define (sum-largest-two-squares x y z)
  (cond
   ((<= z x y) (+ (* x x) (* y y)))     ; if z is smallest return x^2 + y^2
   ((<= y x z) (+ (* x x) (* z z)))     ; if y is smallest return x^2 + z^2
   (else (+ (* y y) (* z z)))))         ; else x must be smallest so return y^2 + z^2


;;; Exercise 1.4
;;; ------------
;;; Describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The if statement returns a procedure (either + or -) which will be used to
;; evaluate the following operands (a and b). If b is positive, the + operator
;; is returned and (+ a b) is evaluated, otherwise the - operator is returned
;; and (- a b) is evaluated. The result of that evaluation is returned from
;; the function.


;;; Exercise 1.5
;;; ------------
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


;;; Exercise 1.6
;;; ------------
;;; What happens using the code below to compute square roots?

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; old sqrt-iter
;; (define (sqrt-iter guess x)
;;   (if (good-enough? guess x)
;;       guess
;;       (sqrt-iter (improve guess x)
;;                  x)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

;; Because the interpreter uses applicative-order evaluation, every time
;; `new-if` is called, it will evaluate the arguments passed to it, then
;; evaluate the if statement. This results in an infinite loop because
;; `sqrt-iter` will be called before the contents of `new-if` execute. Resulting
;; in the if statement never being evaluated, since `sqrt-iter` will keep
;; calling itself.


;;; Exercise 1.7
;;; ------------
;;; Why is `good-enough?` not good enough when checking very small or large
;;; numbers? Explain with examples. Design a new procedure that checks the
;;; change in the guess from one iteration to the next.

;; `good-enough?` is testing whether the absolute value of the difference
;; between the square of our guess and the user's input is less then 0.001.
;; When dealing with a very small user input, the difference can be less than
;; 0.001 and result in a severe difference between the actual value
;; and our computed value. Take for instance the input of 0.0001. The actual
;; square root is 0.01, but the calculated result using the above procedures
;; results in a square root of ~0.03. That's 3x greater than the correct
;; answer. The square of 0.03 is 0.0009. The difference between the original
;; number (0.001) and 0.0009 is 0.0001, which fulfills the `good-enough?`
;; prodedure's requirements, but clearly is not good enough since the computed
;; square root is 3x greater than the actual square root.

;; For very large numbers, the guesses computed by `improve` won't be very
;; precise. Since the numbers are so large, it is possible for `improve`
;; procedure to reach a point where the output is always the same because the
;; float operations cannot make a more precise number. This in turn results
;; in an infinite loop as the most precise guess that is produced is never good
;; enough.

(sqrt 0.0001)
;; 0.03230844833048122

(sqrt 10000000000000000000000000000000)
;; ininite loop

(define (sqrt-improved x)
  (sqrt-iter-improved 1.0 x 2.0))

(define (sqrt-iter-improved guess x old-guess)
  (if (good-enough?-improved guess old-guess)
      guess
      (sqrt-iter-improved (improve guess x)
                          x
                          guess)))

(define (good-enough?-improved guess old-guess)
  (< (abs (- guess old-guess)) 0.001))

(sqrt-improved 0.0001)
;; 0.010000714038711746

(sqrt 10000000000000000000000000000000)
;; 3162277660168379.0

;; `sqrt-improved` makes much closer approximations.


;;; Exercise 1.8
;;; ------------
;;; Given the method for finding better approximations of cube roots is
;;; ((x/(y^2)) + 2y)/3, create a procedure to find the cube root analagous
;;; to the procedure to find the square root.

(define (cbrt x)
  (cbrt-iter 1.0 2.0 x))

(define (cbrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (cbrt-iter (improve guess x)
                 guess
                 x)))

(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) 0.001))

(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
     3))

(cbrt 9)
;; 2.0800838232385224

(cbrt 0.0001)
;; 0.046419202576589325

(cbrt 10000000000000000000000000000000)
;; 21544346900.318836
