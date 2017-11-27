#lang sicp

;;; ===========
;;; Section 2.4
;;; ===========


;;; Exercise 2.73
;;; -------------
;;; Using the transformed version of deriv:

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;; compared to the original:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

;;; a) Explain what was done above. Why can't the predicates number? and
;;;    same-variable? be assimilated into the data-directed dispatch?

;; All symbols representing an action (addition, multiplication, etc.) are
;; handled using data-directed dispatching, but numbers and variables are not.
;; This is because data-directed dispatching would have to add the derivative
;; operation performed on numbers and variables to *every* possible number and
;; variable since the get procedure returns the appropriate derivative procedure
;; based on the symbol it's passed.

;;; b) Write the procedures for derivatives of sums and products, and the
;;;    auxiliary code required to install them in the table used by the program
;;;    above.

(define (install-sum-deriv-package)
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (addend s) (car s))
  (define (augend s)
    (let ((aug (cdr s)))
      (if (null? (cdr aug))
	  (car aug)
	  (cons '+ aug))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))
  (put 'deriv 'make-sum make-sum)
  (put 'deriv '+ (lambda (exp var)
		   (make-sum (deriv (addend exp) var)
			     (deriv (augend exp) var)))))

(define (install-mult-deriv-package)
  (define (multiplier p) (car p))
  (define (multiplicand p)
    (let ((mult (cdr p)))
      (if (null? (cdr mult))
	  (car mult)
	  (cons '* mult))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((and (number? m1) (number? m2)) (* m1 m2))
	  (else (list '* m1 m2))))
  (put 'deriv 'make-product make-product)
  (put 'deriv '* (lambda (exp var)
		   ((get 'deriv 'make-sum)
		    (make-product (multiplier exp)
				  (deriv (multiplicand exp) var))
		    (make-product (deriv (multiplier exp) var)
				  (multiplicand exp))))))

;;; c) Choose any additional differentiation rule that you like, such as the one
;;;    for exponents and install it in this data-directed system.

(define (install-expt-deriv-package)
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
	  ((=number? exponent 1) base)
	  ((and (number? base) (number? exponent)) (expt base exponent))
	  (else (list '** base exponent))))
  (put 'deriv 'make-exponentiation make-exponentiation)
  (put 'deriv '** (lambda (exp var)
		    ((get 'deriv 'make-product)
		     ((get 'deriv 'make-product)
		      (exponent exp)
		      (make-exponentiation
		       (base exp)
		       ((get 'deriv 'make-sum)
			(exponent exp)
			-1)))))))

;;; d) Suppose the procedures were indexed in the opposite way, such that
;;;    the dispatch line in deriv looked like:

;;;    ((get (operator exp) 'deriv) (operands exp) var)

;;;    What corresponding changes to the derivative system are required?

;; The operator and type are switched, so the put procedures would have
;; to swap <op> and <type>.


;;; Exercise 2.74
;;; -------------
;;; Create a data-driven dispatching scheme for Insatiable Enterprises, Inc.

;;; a) Implement for headquarters a get-record procedure that retrieves a
;;;    specified employee's record. The procedure should be applicable to any
;;;    division's file. Explain how the individual divisions' files should be
;;;    structured. In particular, what type information must be supplied?

(define (division file)
  "Gets the division from the file. No need to implement")

(define (get-record employee-name file)
  ((get (division file) 'get-record) employee-name))

;; Each division can continue to use their current scheme, they just need to
;; add their employee selector procedure to a table. The table has each division
;; on one axis and procedures names that are common to all divisions on the
;; other axis. The divisions' files can be implemented however they want, as
;; long as the selectors properly get the record of an employee. The only
;; requirement is that the file contains the division name as the first line.

;;; b) Implement for headquarters a get-salary procedure that returns the salary
;;;    information from a given employee's record from any division's personnel
;;;    file. How should the record be structured in order to make this operation
;;;    work?

(define (employee-division record)	; Unnecessary but nice to have.
  (car record))

(define (employee-record record)
  (cdr record))

(define (get-salary employee-name file)
  (let ((record (get-record employee-name file))
	(division (division file)))
    ((get division 'get-salary) (employee-record record))))

;; The record should have a tag indicating the division and the employee record
;; as a cons cell, like: (cons <division> <employee-record>).

;;; c) Implement for headquarters a find-employee-record procedure. This should
;;;    search all the divisions' files for the record of a given employee and
;;;    return the record. Assume that this procedure takes as arguments an
;;;    employee's name and a list of all the divisions' files.

(define (find-employee-record employee-name division-files)
  (let ((record (map (lambda (x) (get-record employee-name x))
		     division-files)))
    (if (null? record)
	record
	(car record))))

;;; d) When Insatiable takes over a new company, what changes must be made in
;;;    order to incorporate the new personnel information into the central
;;;    system?

;; The new company can keep their current scheme as long as they add the
;; appropriate selectors to the division/selector table and keep all records
;; in a single file. Other than that, their get-record selector must return
;; records structured like: (cons <division> <employee-info>).


;;; Exercise 2.75
;;; -------------
;;; Implement the constructor make-from-mag-ang in message-passing style.

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


;;; Exercise 2.76
;;; -------------
;;; As a large system with generic operations evolves, new types of data objects
;;; or new operations may be needed. For each of the three strategies -- generic
;;; operations with explicit dispatch, data-directed style, and
;;; message-passing-style -- describe the changes that must be made to a system
;;; in order to add new types or new operations. Which organization would be
;;; most appropriate for a system in which new types must often be added? Which
;;; would be most appropriate for a system in which new operations must often be
;;; added?

;; Generic operations with explicit dispatch:
;; Any changes or additions to this style requires the changes to be reflected
;; in the code that uses the API. The code is added or changed as is and must
;; be updated everywhere to reflect that.

;; Data-directed style:
;; Any changes or additions to this style doesn't require the changes to be
;; reflected in the code that uses the API. The interface allows for flexibility
;; with different types while not affecting the API.

;; Message-passing style:
;; Any changes or additions to this style doesn't require the changes to be
;; reflected in the code that uses the API. The interface allows for flexibility
;; with different data objects.

;; Both data-directed and message-passing styles are appropriate for systems
;; where new types are added often. The interface remains relatively static
;; which is desired in a system like this. For systems where new operations
;; are often added, any of the styles are appropriate. If you want to add
;; a new operation to all the current types, each type needs to have the
;; operation added regardless of style. A call through whatever interface
;; is being used must then be made to invoke that operation. In terms of
;; adding operations, the amount of work is the same, the difference is in how
;; the code is layed out.
