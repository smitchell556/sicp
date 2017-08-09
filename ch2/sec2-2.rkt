#lang sicp

;;; ===========
;;; Section 2.2
;;; ===========


;;; Exercise 2.17
;;; -------------
;;; Define a procedure last-pair that returns the list that contains only the
;;; last element of a given nonempty list.

(define (last-pair items)
  (cond ((or (null? items) (null? (cdr items))) items)
	(else
	 (last-pair (cdr items)))))


;;; Exercise 2.18
;;; -------------
;;; Define a procedure reverse that takes a list as argument and returns a list
;;; of the same elements in reverse order.

(define (reverse items)
  (if (null? (cdr items))
      (car items)
      (cons (reverse (cdr items)) (car items))))


;;; Exercise 2.19
;;; -------------
;;; Define procedures first-denomination, except-first-denomination, and
;;; no-more? such that the cc procedure from 1.2.2 can work with any kind of
;;; coin denominations. Does the order of the list coin-values affect the
;;; answer produced by cc?

;; Given:

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
292

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; Answer:

(define (first-denomination values)
  (car values))

(define (except-first-denomination values)
  (cdr values))

(define (no-more? values)
  (if (null? values)
      #t
      #f))

;; No, the order doesn't matter because every possible coin combination is
;; still tried, just not in the same order.


;;; Exercise 2.20
;;; -------------
;;; Define a procedure same-parity that takes one or more integers and returns
;;; a list of all arguments that have same even-odd parity as the first
;;; argument. Use the . notation for multiple arguments.

(define (same-parity x . y)
  (define (get-parity-y y)
    (cond ((null? y) y)
	  ((= (remainder x 2) (remainder (car y) 2)) (cons (car y)
						     (get-parity-y (cdr y))))
	  (else
	   (get-parity-y (cdr y)))))
  (cons x (get-parity-y y)))
