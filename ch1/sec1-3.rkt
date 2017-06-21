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
;;; Create an iterative version of sum that fits the following definition:
