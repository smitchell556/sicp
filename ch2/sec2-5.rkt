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
