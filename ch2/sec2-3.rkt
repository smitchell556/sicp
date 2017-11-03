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
