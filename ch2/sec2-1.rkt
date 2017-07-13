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
