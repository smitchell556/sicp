#lang sicp

;;; ===========
;;; Section 2.1
;;; ===========


;;; Exercise 2.1
;;; ------------
;;; Define a version of make-rat that handles both positive and negative
;;; numbers.

;; Original:

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; Handles negatives:

(define (equalize-sign a b)
  ;; convert sign of b to be equal to sign of a.
  ;; returns: normalized b.
  (if (eq? (>= a 0) (>= b 0))
      b
      (* b -1)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (equalize-sign (/ n d) (/ n g)) (equalize-sign 1 (/ d g)))))


;;; Exercise 2.2
;;; ------------
;;; Represent lines using two points on a plane. Define make-segment which has
;;; a start-segment and end-segment. A point can be represented as an
;;; x-coordinate and y-coordinate. Define make-point and selectors x-point and
;;; y-point. Define a procedure midpoint-segment which takes a line segment and
;;; returns its midpoint (the average of the coordinates of the endpoints.

;; Supplied is a procedure to print points.

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

;; point structure API

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

;; line segment structure API

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;; midpoint definition
(define (midpoint-segment s)
  (define (avg-coord segment coord)
    (/ (+ (coord (start-segment segment)) (coord (end-segment segment)))
       2))
  (make-point (avg-coord s x-point) (avg-coord s y-point)))


;;; Exercise 2.3
;;; ------------
;;; Implement a representation for rectangles in a plane. Make procedures that
;;; compute the perimeter and the area of a given rectangle. Implement a
;;; different representation for rectangles that works with the same perimeter
;;; and area procedures.

(define (make-rect base height)
  ;; base: a line segment.
  ;; height: an int.
  (cons base height))

(define (distance segment)
  ;; finds the distance of a segment.
    (sqrt (+ (expt (- (x-point (start-segment segment))
		      (x-point (end-segment segment)))
		   2)
	     (expt (- (y-point (start-segment segment))
		      (y-point (end-segment segment)))
		   2))))

(define (base rect)
  (distance (car rect)))

(define (height rect)
  (cdr rect))

(define (perimeter rect)
  (+ (* (base rect)
	2)
     (* (height rect)
	2)))

(define (area rect)
  (* (base rect)
     (height rect)))

;; Alternate representations

(define (make-rect base height)
  ;; base: a line segment.
  ;; height: an int.
  (cons (distance base) height))

(define (base rect)
  (car rect))

;; height does not need to be redefined since it is a distance already.


;;; Exercise 2.4
;;; ------------
;;; Verify below representations of cons and car work. Write a definition for
;;; cdr that falls in line with these.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 1 2))
;; (car (lambda (m) (m 1 2)))
;; ((lambda (m) (m 1 2)) (lambda (p q) p))
;; ((lambda (p q) p) 1 2)
;; 1

(define (cdr z)
  (z (lambda (p q) q)))


;;; Exercise 2.5
;;; ------------
;;; Represent pairs of nonnegative integrs using only numbers and arithmetic
;;; operations if we represent the pair a and b as the integer that is the
;;; product 2^a * 3^b. Give definitions of cons, car, and cdr for this.

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car c)
  (define (iter x count)
    (if (not (= (remainder x 2) 0))
	count
	(iter (/ x 2) (inc count))))
  (iter c 0))

(define (cdr c)
  (define (iter x count)
    (if (not (= (remainder x 3) 0))
	count
	(iter (/ x 3) (inc count))))
  (iter c 0))


;;; Exercise 2.6
;;; ------------
;;; Define procedures one and two without using add-1 or zero. Hint: Use substitution
;;; to evaluate (add-1 zero). Give a direct definition of the addition procedure
;;; + (not in terms of repeated application of add-1).

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; Further evaluating even though this is where I think it would normally stop
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;; (lambda (f) (lambda (x) (f x))) -> this represents one
(add-1 one)
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; Further evaluating even though this is where I think it would normally stop
;; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;; (lambda (f) (lambda (x) (f (f x)))) -> this represents two.
;; The *Church numerals* look like a function f performed n times on some value
;; x where n would be the Church numeral.

;; Using add-1 and zero
(define one (add-1 zero))
(define two (add-1 one))

;; Without add-1 and zero
(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

;; An add procedure for Church numerals would perform a function f on a value
;; a + b times. So 2 + 2 would result in (f (f (f (f x)))). Basically the
;; function needs to be passed to both a and b to get the right number of
;; function calls, then x needs to be passed to the result of the combination
;; of b and the function and that is passed to the result of the combination
;; of a and the function.

(define (add m n)
  (lambda (f)
    (lambda (x) ((m f) ((n f) x)))))


;;; Exercise 2.7
;;; ------------
;;; Define selectors upper-bound and lower-bound to complete the interval
;;; implementation.

(define (upper-bound interval)
  (if (> (car interval) (cdr interval))
      (car interval)
      (cdr interval)))

(define (lower-bound interval)
  (if (< (car interval) (cdr interval))
      (car interval)
      (cdr interval)))


;;; Exercise 2.8
;;; ------------
;;; Define sub-interval which subtracts two intervals.

;; This implementation will subtract the smaller average resistance from the
;; larger average resistance.

(define (sub-interval x y)
  (define (avg interval)
    (/ (+ (upper-bound interval) (lower-bound interval))
       2))
  (let ((big (if (> (avg x) (avg y))
                 x
                 y))
        (small (if (< (avg x) (avg y))
                 x
                 y)))
    (make-interval (- (upper-bound big) (lower-bound small))
                   (- (lower-bound big) (upper-bound small)))))
