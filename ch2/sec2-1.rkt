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
