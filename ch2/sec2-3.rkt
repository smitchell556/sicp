#lang sicp

;;; ===========
;;; Section 2.3
;;; ===========


;;; Exercise 2.53
;;; -------------
;;; What would the interpreter print in response to evaluating each of the
;;; following expressions?

(list 'a 'b 'c)
;; (a b c)

(list (list 'george))
;; ((george))

(cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

(pair? (car '(a short list)))
;; #f

(memq 'red '((red shoes) (blue socks)))
;; #f

(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)


;;; Exercise 2.54
;;; -------------
;;; Implement the procedure equal? using eq?

(define (equal? a b)
  (cond ((or (null? a) (null? b)) (eq? a b))
	((and (pair? (car a)) (pair? (car b)))
	 (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
	((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
	(else false)))


;;; Exercise 2.55
;;; -------------
;;; Explain why (car ''abracadabra) prints quote in the interpreter.

;; Remember '<item> is equivalent to (quote <item>), so (car ''abracadabra)
;; is the same as (car (quote (quote abracadabra))). The first call to quote
;; returns (quote abracadabra) as a list which is then passed as an argument
;; to car. car returns the first element of the list (quote abracadabra) which
;; is "quote".

;; (car ''abracadabra)
;; (car (quote (quote abracadabra)))
;; (car (quote abracadabra))
;; quote


;;; Exercise 2.56
;;; -------------
;;; Extend the basic differentiator to handle more kinds of expressions.
;;; Implement the differentiation rule:
;;; d(u^n)/dx = nu^(n-1)(du/dx)
;;; by adding a new clause to the deriv program and defining procedures
;;; exponentiation?, base, exponent, and make-exponentiation.
;;; Use the symbol ** to denote exponentiation. Build in the rules that
;;; anything raised to the power 0 is 1 and anything raised to the power 1 is
;;; itself.

;; Given:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;; Extended implementation:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (make-sum (exponent exp)
						       -1)))
	  (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list '** base exponent))))


;;; Exercise 2.57
;;; -------------
;;; Extend the differentiation program to handle sums and products of arbitrary
;;; numbers of (two or more) terms. Do not change deriv.

(define (augend s)
  (define (recursive-augend s)
    (cond ((null? s) '())
          (else (append (list (car s)) (recursive-augend (cdr s))))))
  (let ((aug (cddr s)))
    (if (null? (cdr aug))
        (car aug)
        (append '(+) (recursive-augend aug)))))

(define (multiplicand p)
  (define (recursive-multiplicand p)
    (cond ((null? p) '())
          (else (append (list (car p)) (recursive-multiplicand (cdr p))))))
  (let ((mult (cddr p)))
    (if (null? (cdr mult))
        (car mult)
        (append '(*) (recursive-multiplicand mult)))))
