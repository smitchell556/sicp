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


;;; Exercise 2.21
;;; -------------
;;; Complete both implementations of square-list.

(define (square-list items)
  (if (null? items)
      nil
      (cons <??> <??>)))
(define (square-list items)
  (map <??> <??>))

(define (square-list items)
  (if (null? items)
      items
      (cons (* (car items) (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map (lambda (x) (* x x)) items))


;;; Exercise 2.22
;;; -------------
;;; Explain why the following procedures create a reverse of the intended
;;; list.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))

;; The problem is the car of the list is being added in the wrong order.
;; The last element of things should be cons'd onto answer at each iteration.
;; The list is growing from the tail, not the head.

(square-list list(1 2))
;; (iter (1 . 2) '())
;; (iter (iter (2) (1 . ())))
;; (iter (iter (iter () (4 . (1 . ())))))
;; (iter (iter (4 . (1 . ()))))
;; (iter (4 . (1 . ())))
;; (4 . (1 . ()))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; This results in the right order, but car and cdr wont work as expected.
;; The cdr of a list produced by this procedure will be the last element of the
;; list, while the car of the list will be the list of all elements excluding
;; the last element. This also results in the first element of the list being
;; nil.

(square-list list(1 2))
;; (iter (1 . 2) '())
;; (iter (iter (2) (() . 1)))
;; (iter (iter (iter () ((() . 1) . 4))))
;; (iter (iter ((() . 1) . 4)))
;; (iter ((() . 1) . 4))
;; ((() . 1) . 4)

;; The list should look like (1 . (4 . ()))


;;; Exercise 2.23
;;; -------------
;;; Give an implementation of for-each.

(define (for-each f items)
  (cond ((null? items) items)
        (else
         (f (car items))
         (for-each f (cdr items)))))
