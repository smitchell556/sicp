#lang scheme

;;; ===========
;;; Section 1.3
;;; ===========


;;; Exercise 1.29
;;; -------------
;;; Use Simpson's Rule to find integrals.

;; Given:
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))
;; ------

(define (integral-simpson f a b n)
  (define (h)
    (* (/ (- b a) n) 1.0))
  (define (real-f x)
    (* (if (even? x)
	   2
	   4)
       (f (+ a
	     (* x
		(h))))))
  (cond
   ((odd? n) #f)
   (else
    (* (/ (h)
	  3)
       (+ (f (+ a
		(* 0
		   (h))))
	  (sum real-f 1 inc (- n 1))
	  (f (+ a
		(* n
		   (h)))))))))

(integral-simpson cube 0 1 100)
;; 0.24999999999999992

(integral-simpson cube 0 1 1000)
;; 0.2500000000000003

;; The results are more precise than the ones determined by the initial integral
;; function in the text.


;;; Exercise 1.30
;;; -------------
;;; Create an iterative version of sum that fits the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if <??>
	<??>
	(iter <??> <??>)))
  (iter <??> <??>))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result a))))
  (iter a 0))


;;; Exercise 1.31
;;; -------------
;;; a. Define a procedure product. Define factorial using product and compute
;;;    the approximation of pi/4 = 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 ...

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial a b)
  (define (identity x)
    x)
  (define (inc x)
    (+ x 1))
  (product identity a inc b))

(define (pi-prod a b)
  (define (pi-func x)
    (* (/ x (+ x 1.0))
       (/ (+ x 2.0) (+ x 1.0))))
  (define (inc-2 x)
    (+ x 2))
  (cond
   ((< a 2) #f)
   ((odd? a) #f)
   (else
    (product pi-func a inc-2 b))))

;;; b. Define an iterative version of product.

(define (product term a next b)
  (define (product-iter x accumulator)
    (if (> x b)
	accumulator
	(product-iter (next x) (* accumulator
				  (term x)))))
  (product-iter a 1))


;;; Exercise 1.32
;;; -------------
;;; a. Show that sum and product are both special cases of a more general
;;;    notion called accumulate. Show how sum and product can be defined with
;;;    accumulate.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;; b. Write an iterative version of accumulate.

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter x accumulator)
    (if (> x b)
	accumulator
	(accumulate-iter (next x) (combiner accumulator (term x)))))
  (accumulate-iter a null-value))


;;; Exercise 1.33
;;; -------------
;;; Define a procedure, filtered-accumulate, which combines the terms within a
;;; specified range that satisfies a specified condition.

(define (filtered-accumulate combiner null-value term a next b filter)
  (cond
   ((> a b) null-value)
   ((filter a) (combiner (term a)
			 (filtered-accumulate combiner null-value term (next a) next b filter)))
   (else
    (filtered-accumulate combiner null-value term (next a) next b filter))))

;;; a. Sum of squares of prime numbers from a to b (assuming prime? already exists).

(define (sum-prime-squares a b)
  (define (square x)
    (* x x))
  (define (inc x)
    (+ x 1))
  (filtered-accumulate + 0 square a inc b prime?))

;;; b. Product of all positive integers less than n that are relatively prime.

(define (prod-rel-primes n)
  (define (identity x)
    x)
  (define (inc x)
    (+ x 1))
  (define (gcd a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))
  (define (rel-prime? x)
    (if (= (gcd n x) 1)
	#t
	#f))
  (if (< n 2)
      1
      (filtered-accumulate * 1 identity 1 inc n rel-prime?)))


;;; Exercise 1.34
;;; -------------
;;; Given:

(define (f g)
  (g 2))

;;; What would happen if we run (f f)?

;; Initial guess is that (f f) would evaluate to (f 2) which would evaluate to
;; (2 2) which would throw an error because 2 is an int not a procedure.

;; After evaluating in an interpreter this guess was correct. The interpreter
;; threw an error saying it expected a procedure but was given 2 instead.


;;; Exercise 1.35
;;; -------------
;;; Show the golden ratio is a fixed point of the transformation x -> 1 + 1/x
;;; and use this to compute it by the fixed-point procedure.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; The golden ratio = (1 + sqrt(5)) / 2

;; Change the transformation x -> 1 + 1/x to an equation x = 1 + 1/x and solve
;; x = 1 + 1/x
;; x - 1/x = 1
;; x * (x - 1/x) = 1 * x
;; x^2 - 1 = x
;; x^2 - x - 1 = 0
;; By the quadratic equation we get x = (1 +/- sqrt(5)) / 2
;; Therefore x = (1 + sqrt(5)) / 2 and x = 1 + 1/x.

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)


;;; Exercise 1.36
;;; -------------
;;; Modify fixed-point to print the sequence of approximations it generates.
;;; Then fin a solution to x^x = 1000 with the transformation
;;; x -> log(1000) / log(x).

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
;; 1.1
;; 72.47657378429035
;; 1.6127318474109593
;; 14.45350138636525
;; 2.5862669415385087
;; 7.269672273367045
;; 3.4822383620848467
;; 5.536500810236703
;; 4.036406406288111
;; 4.95053682041456
;; 4.318707390180805
;; 4.721778787145103
;; 4.450341068884912
;; 4.626821434106115
;; 4.509360945293209
;; 4.586349500915509
;; 4.535372639594589
;; 4.568901484845316
;; 4.546751100777536
;; 4.561341971741742
;; 4.551712230641226
;; 4.558059671677587
;; 4.55387226495538
;; 4.556633177654167
;; 4.554812144696459
;; 4.556012967736543
;; 4.555220997683307
;; 4.555743265552239
;; 4.555398830243649
;; 4.555625974816275
;; 4.555476175432173
;; 4.555574964557791
;; 4.555509814636753
;; 4.555552779647764
;; 4.555524444961165
;; 4.555543131130589
;; 4.555530807938518
;; 4.555538934848503


;;; Exercise 1.37
;;; -------------
;;; a. Define a procedure cont-frac that calculates a continued fraction
;;;    up to k terms.

(define (cont-frac n d k)
  (define (cont-frac-counter i)
    (if (= i k)
	(/ (n i) (d i))
	(+ (d (- i 1)) (/ (n i) (cont-frac-counter (+ i 1))))))
  (/ (n 1) (cont-frac-counter 2)))
