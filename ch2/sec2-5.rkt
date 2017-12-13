#lang sicp

;;; ===========
;;; Section 2.5
;;; ===========


;;; Exercise 2.77
;;; -------------
;;; Why does the selector magnitude for ('complex ('rectangular (3 4))) work
;;; for complex numbers after running the following put procedures.

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;; Given:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; Solution:

;; Calling magnitude will call apply-generic on the 'complex type, which will
;; get the same magnitude selector and apply it with the cdr of the input,
;; '(rectangular (3 4)). The selector will call apply-generic again but this
;; time it will get the magnitude selector for the 'rectangular type which
;; will compute the magnitude from the data (3 4) which represents 3+4i.

;; (magnitude z)
;; (magnitude '(complex (rectangular (3 4))))
;; (apply-generic 'magnitude '(complex (rectangular (3 4))))
;; type-tags = ('complex)
;; proc = magnitude    ; General selector
;; (apply magnitude '((rectangular (3 4))))
;; (magnitude '(rectangular (3 4)))
;; (apply-generic 'magnitude '(rectangular (3 4)))
;; type-tags = ('rectangular)
;; proc = magnitude    ; Rectangular selector
;; (apply magnitude '((3 4)))
;; (magnitude '(3 4))
;; (sqrt (+ (square 3)
;;          (square 4)))
;; 5


;;; Exercise 2.78
;;; -------------
;;; Modify the definitions of type-tag, contents, and attach-tag from section
;;; 2.42 so that our generic system takes advantage of Scheme's internal type
;;; system.

(define (attach-tag type-tag contents)
  (if (eq? 'scheme-number type-tag)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum -- CONTENTS" datum))))


;;; Exercise 2.79
;;; -------------
;;; Define a generic equality predicate equ? that tests the equality of two
;;; numbers, and install it in the generic arithmetic package. This operation
;;; should work for ordinary numbers, rational numbers, and complex numbers.

;; From sec 2.3:

(define (equal? a b)
  (cond ((or (null? a) (null? b)) (eq? a b))
	((and (pair? (car a)) (pair? (car b)))
	 (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
	((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
	(else false)))

(define (install-generic-equ-package)
  ;; Internal procedures
  (define (equ-ordinary-ordinary? x y) =)
  (define (equ-rational-rational? x y)
    (equal? x y))
  (define (equ-complex-complex? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  (define (equ-ordinary-rational? x y)
    (equ-rational-rational? (make-rational (contents x) 1) y))
  (define (equ-ordinary-complex? x y)
    (equ-complex-complex? (make-complex-from-real-imag (contents x) 0) y))
  (define (equ-rational-complex? x y)
    (if (= (imag-part y) 0)
	(equ-rational-rational? x (make-rational (real-part y) 1))
	false))
  ;; Interface to rest of system
  (put 'equ? '(scheme-number scheme-number) equ-ordinary-ordinary?)
  (put 'equ? '(rational rational) equ-rational-rational?)
  (put 'equ? '(complex complex) equ-complex-complex?)
  (put 'equ? '(scheme-number rational) equ-ordinary-rational?)
  (put 'equ? '(rational scheme-number) (lambda (x y) (equ-ordinary-rational? y x)))
  (put 'equ? '(scheme-number complex) equ-ordinary-complex?)
  (put 'equ? '(complex scheme-number) (lambda (x y) (equ-ordinary-complex? y x)))
  (put 'equ? '(rational complex) equ-rational-complex?)
  (put 'equ? '(complex rational) (lambda (x y) (equ-rational-complex? y x)))
  'done)

(define (equ? x y) (apply-generic 'equ? x y))

;; Note: Looking at the answer for this problem on the scheme wiki, it seems I
;;       misunderstood what was expected. The answers on the scheme wiki only
;;       concern comparing numbers with equivalent tags, so only comparing
;;       'scheme-number to 'scheme-number, 'rational to 'rational and 'complex
;;       to 'complex. I will leave my answer as is since it offers a greater
;;       breadth of equality checks while simultaneously finding a solution
;;       to the original problem without the use of the internal procedures
;;       used by the other packages.

;; Expected solution:

(define (install-scheme-number-package)
  ;; ...
  (define (equ? x y) =)
  ;; ...
  (put 'equ? '(scheme-number scheme-number) equ?)
  'done)

(define (install-rational-package)
  ;; ...
  (define (equ? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  ;; ...
  (put 'equ? '(rational rational) equ?)
  'done)

(define (install-complex-package)
  ;; ...
  (define (equ? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  ;; ...
  (put 'equ? '(complex complex) equ?)
  'done)

(define (equ? x y)
  (apply-generic 'equ? x y))


;;; Exercise 2.80
;;; -------------
;;; Define a generic predicate =zerio? that tests if its argument is zero, and
;;; install it in the generic arithmetic package. This operation should work
;;; for ordinary numbers, rational numbers, and complex numbers.

(define (install-scheme-number-package)
  ;; ...
  (define (=zero? x)
    (= x 0))
  ;; ...
  (put '=zero? 'scheme-number =zero?)
  'done)

(define (install-rational-package)
  ;; ...
  (define (=zero? x)
    (= (numer x) 0))
  ;; ...
  (put '=zero? 'rational =zero?)
  'done)

(define (install-complex-package)
  ;; ...
  (define (=zero? x)
    (= (real-part x) (imag-part x) 0))
  ;; ...
  (put '=zero? 'complex =zero?)
  'done)

(define (=zero? x) (apply-generic '=zero? x))
