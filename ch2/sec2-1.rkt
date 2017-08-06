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


;;; Exercise 2.3
;;; ------------
;;; Implement a representation for rectangles in a plane. Make procedures that
;;; compute the perimeter and the area of a given rectangle. Implement a
;;; different representation for rectangles that works with the same perimeter
;;; and area procedures.

(define (make-rect base height)
  ;; base: a line segment.
  ;; height: an int.
  (cons base height))

(define (distance segment)
  ;; finds the distance of a segment.
    (sqrt (+ (expt (- (x-point (start-segment segment))
		      (x-point (end-segment segment)))
		   2)
	     (expt (- (y-point (start-segment segment))
		      (y-point (end-segment segment)))
		   2))))

(define (base rect)
  (distance (car rect)))

(define (height rect)
  (cdr rect))

(define (perimeter rect)
  (+ (* (base rect)
	2)
     (* (height rect)
	2)))

(define (area rect)
  (* (base rect)
     (height rect)))

;; Alternate representations

(define (make-rect base height)
  ;; base: a line segment.
  ;; height: an int.
  (cons (distance base) height))

(define (base rect)
  (car rect))

;; height does not need to be redefined since it is a distance already.


;;; Exercise 2.4
;;; ------------
;;; Verify below representations of cons and car work. Write a definition for
;;; cdr that falls in line with these.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 1 2))
;; (car (lambda (m) (m 1 2)))
;; ((lambda (m) (m 1 2)) (lambda (p q) p))
;; ((lambda (p q) p) 1 2)
;; 1

(define (cdr z)
  (z (lambda (p q) q)))


;;; Exercise 2.5
;;; ------------
;;; Represent pairs of nonnegative integrs using only numbers and arithmetic
;;; operations if we represent the pair a and b as the integer that is the
;;; product 2^a * 3^b. Give definitions of cons, car, and cdr for this.

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car c)
  (define (iter x count)
    (if (not (= (remainder x 2) 0))
	count
	(iter (/ x 2) (inc count))))
  (iter c 0))

(define (cdr c)
  (define (iter x count)
    (if (not (= (remainder x 3) 0))
	count
	(iter (/ x 3) (inc count))))
  (iter c 0))


;;; Exercise 2.6
;;; ------------
;;; Define procedures one and two without using add-1 or zero. Hint: Use substitution
;;; to evaluate (add-1 zero). Give a direct definition of the addition procedure
;;; + (not in terms of repeated application of add-1).

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; Further evaluating even though this is where I think it would normally stop
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;; (lambda (f) (lambda (x) (f x))) -> this represents one
(add-1 one)
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; Further evaluating even though this is where I think it would normally stop
;; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;; (lambda (f) (lambda (x) (f (f x)))) -> this represents two.
;; The *Church numerals* look like a function f performed n times on some value
;; x where n would be the Church numeral.

;; Using add-1 and zero
(define one (add-1 zero))
(define two (add-1 one))

;; Without add-1 and zero
(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

;; An add procedure for Church numerals would perform a function f on a value
;; a + b times. So 2 + 2 would result in (f (f (f (f x)))). Basically the
;; function needs to be passed to both a and b to get the right number of
;; function calls, then x needs to be passed to the result of the combination
;; of b and the function and that is passed to the result of the combination
;; of a and the function.

(define (add m n)
  (lambda (f)
    (lambda (x) ((m f) ((n f) x)))))


;;; Exercise 2.7
;;; ------------
;;; Define selectors upper-bound and lower-bound to complete the interval
;;; implementation.

(define (upper-bound interval)
  (if (> (car interval) (cdr interval))
      (car interval)
      (cdr interval)))

(define (lower-bound interval)
  (if (< (car interval) (cdr interval))
      (car interval)
      (cdr interval)))


;;; Exercise 2.8
;;; ------------
;;; Define sub-interval which subtracts two intervals.

(define (sub-interval x y)
  (add-interval x
                (make-interval (* -1.0
                                  (lower-bound y))
                               (* -1.0
                                  (upper-bound y)))))


;;; Exercise 2.9
;;; ------------
;;; Show the sum (or difference) is a function of the combining of the width of
;;; the arguments and that for multiplication (or division) is is not a
;;; function of the combination of the width of the arguments.

;; a = 5 +- 1: upper-bound = 6, lower-bound = 4
;; b = 4 +-1: upper-bound = 5, lower-bound = 3
;; a + b = c: upper-bound = 11, lower-bound = 7
;; (11 + 7) / 2 = 9: c = 9 +- 2
;; c = a.value + b.value +- (a.width + b.width)

;; a * b = d: upper-bound = 30, lower-bound = 12
;; (30 + 12) / 2 = 21: d = 21 +- 9
;; 9 != 1 * 1


;;; Exercise 2.10
;;; -------------
;;; Modify the code to check for an interval that spans zero and raises an error
;;; if it occurs.

;; Original

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Modified

(define (div-interval x y)
  (cond ((and (> (upper-bound y) 0)
              (< (lower-bound y) 0))
         (error "interval spans 0"))
        (else
         (mul-interval x
                       (make-interval (/ 1.0 (upper-bound y))
                                      (/ 1.0 (lower-bound y)))))))


;;; Exercise 2.11
;;; -------------
;;; Change mul-interval to handle all possible cases of intervals.

;; Honestly this one just looks like busy work. Most of these "extra exercises"
;; feel like busy work, but this one more so than the others. Skipping.


;;; Exercise 2.12
;;; -------------
;;; Define a constructor make-center-percent that takes a center and percentage
;;; tolerance and produces the desired interval. Define a selector percent that
;;; produces the percentage tolerance for a given interval.

;; Given:

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; New:

(define (make-center-percent c p)
  (let ((w (* c (/ p 100.0))))
    (make-center-width c w)))

(define (percent i)
  (* (/ (width i)
        (center i))
     100))


;;; Exercise 2.13
;;; -------------
;;; Show there is a simple formula for the percentage tolerance of the product
;;; of two intervals in terms of the tolerances of the factors.

;; lower-bound = lowx * lowy
;; lower-bound using percent: low = center - width
;;                                = center - (center / percent)
;; lower-bound = (centerx - (centerx / percentx)) * (centery - (centery / percenty))
;;             = (centerx * centery) - ((centerx * centery) / percentx) - ((centerx * centery) / percenty) + ((centerx * centery) / (percentx * percenty))
;; upper-bound = (centerx + (centerx / percentx)) * (centery + (centery / percenty))
;;             = (centerx * centery) + ((centerx * centery) / percentx) + ((centerx * centery) / percenty) + ((centerx * centery) / (percentx * percenty))

;; center = (upper-bound + lower-bound) / 2
;;        = (((2 * centerx * centery) + (2 * ((centerx * centery) / (percentx * percenty)))) / 2
;;        = (centerx * centery) * (1 + (1 / (percentx * percenty)))

;; percent = ((center - lower-bound) / center) * 100
;;         = ((((centerx * centery) + ((centerx * centery) / (percentx * percenty))) - ((centerx * centery) - ((centerx * centery) / percentx) - ((centerx * centery) / percenty) + ((centerx * centery) / (percentx * percenty)))) / ((centerx * centery) * (1 + (1 / (percentx * percenty))))) * 100
;;         = ((centerx * centery) + ((centerx * centery) / (percentx * percenty)) - (centerx * centery) + ((centerx * centery) / percentx) + ((centerx * centery) / percenty) - ((centerx * centery) / (percentx * percenty)) / ((centerx * centery) * ((centerx * centery) / (percentx * percenty)))) * 100
;;         = ((((centerx * centery) / percentx) + ((centerx * centery) / percenty)) / ((centerx * centery) * ((centerx * centery) / (percentx * percenty)))) * 100
;;         = (((centerx * centery) * ((percentx + percenty) / (percentx * percenty))) / ((centerx * centery) * (1 + (1 / (percentx * percenty))))) * 100
;;         = (((percentx + percenty) / (percentx * percenty)) * ((percentx * percenty) / (1 + (percentx * percenty)))) * 100
;;         = ((percentx + percenty) / (1 + (percentx * percenty))) * 100
