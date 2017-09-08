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
      '()
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
;; '().

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


;;; Exercise 2.24
;;; -------------
;;; Give the results of (list 1 (list 2 (list 3 4))) from the intereter,
;;; the box and pointer structure, and the tree structure.

;; Interpreter:
;; (1 (2 (3 4)))

;; Box and pointer:
;; ## -> ## -> ## -> #X
;; |     |     |     |
;; 1     2     3     4

;; Tree:
;;               (1 (2 (3 4)))
;;                 /       \
;;                1      (2 (3 4))
;;                        /    \
;;                       2    (3 4)
;;                            /   \
;;                           3     4


;;; Exercise 2.25
;;; -------------
;;; Give combinations of cars and cdrs that will pick 7 from each of the
;;; following lists.

(define get-7 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr get-7)))))
;; 7

(define get-7 (list (list 7)))
(car (car get-7))
;; 7

(define get-7 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr get-7))))))))))))
;; 7


;;; Exercise 2.26
;;; -------------
;;; Define x and y to be two lists:
;;; (define x (list 1 2 3))
;;; (define y (list 4 5 6))
;;; What result is printed by the interpreter for the following expressions.

(append x y)
;; (1 2 3 4 5 6)


(cons x y)
;; ((1 2 3) 4 5 6)


(list x y)
;; ((1 2 3) (4 5 6))


;;; Exercise 2.27
;;; -------------
;;; Modify reverse to produce deep-reverse which reverses nested lists.

;; Original:
(define (reverse items)
  (if (null? (cdr items))
      (car items)
      (cons (reverse (cdr items)) (car items))))

;; Modified:
(define (deep-reverse items)
  (if (null? (cdr items))
      (if (pair? (car items))
          (deep-reverse (car items))
          (car items))
      (cons (deep-reverse (cdr items)) (if (pair? (car items))
                                           (deep-reverse (car items))
                                           (car items)))))


;;; Exercise 2.28
;;; -------------
;;; Write a procedure fringe that takes a tree as an argument and returns a list
;;; whose elements are all leaves of the tree arranged in left to right order.

(define (fringe tree)
  (let ((x (if (pair? (car tree))
               (fringe (car tree))
               (car tree))))
    (if (null? (cdr tree))
        x
        (cons x (fringe (cdr tree))))))


;;; Exercise 2.29
;;; -------------
;;; Binary mobile questions.

;; Given: 

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;; a. Write left-branch and right-branch procedures, which return the branches
;;;    of a mobile, and branch-length and branch-structure, which return the
;;;    components of a branch.

(define (left-branch mobile)
  (car mobile))
  
(define (right-branch mobile)
  (cadr mobile))
  
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))


;;; b. Define a procedure total-weight that returns the total weight of a
;;;    mobile.

(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))


;;; c. Define a procedure balanced that returns a boolean which determines if
;;;    the mobile has equal torque on both sides.

(define (balanced mobile)
  (define (inner mobile)
    (if (not (pair? mobile))
        mobile
        (let ((lw (inner (branch-structure (left-branch mobile))))
              (rw (inner (branch-structure (right-branch mobile)))))
          (cond
           ((or (eq? lw #f) (eq? rw #f)) #f)
           ((eq? (* (branch-length (left-branch mobile))
                    lw)
                 (* (branch-length (right-branch mobile))
                    rw))
            (+ lw rw))
           (else #f)))))
  (if (eq? (inner mobile) #f)
      #f
      #t))


;;; d. If make-mobile and make-branch are changed to use cons instead of list,
;;;    how much does the program need to be changed to fit the new
;;;    representation?

;; Any selecters that use cadr should be modified to cdr.

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))


;;; Exercise 2.30
;;; -------------
;;; Define procedure square-tree with and without using map.

;; Without map:
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;; With map:
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))


;;; Exercise 2.31
;;; -------------
;;; Abstract the answer from 2.30 to produce a procedure tree-map which
;;; square-tree could be defined as:
;;; (define (square-tree tree) (tree-map square tree))

(define (tree-map f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       tree))


;;; Exercise 2.32
;;; -------------
;;; Complete the definition of a procedure that generates the set of subsets
;;; of a set and give an explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map <??> rest)))))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
		(map (lambda (item) (list (car s) item))
		     rest)))))

;; For each item in the original list, all possible subsets either include the
;; item or they don't. The procedure gets all possible subsets without each
;; item, and then combines that list with a new list that includes the item
;; with each subset in that list.

;; For example, assume there are two lists; (1) and (1 2). For the first list,
;; (1) there are only two subsets. The empty set and (1), or (() (1)). If we look
;; at the second list, (1 2) we can create all subsets of this list by appending
;; all subsets of (1) with the combination of (2) and every subset of (1). This
;; gives (() (1) (2) (1 2)) (Note: the combination of the empty list with (2) is
;; just (2)). To create a list of all subsets of (1 2 3) would follow the same
;; process by using all subsets of (1 2) to get
;; (() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)). This process can be repeated
;; ad nauseum for each additional item in a list.


;;; Exercise 2.33
;;; -------------
;;; Fill in the missing expressions to complete the following definitions of
;;; some basic list-manipulation operations as accumulations.

(define (map p sequence)
  (accumulate (lambda (x y) <??>) '() sequence))
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons <??> <??>))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate <??> 0 sequence))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;;; Exercise 2.34
;;; -------------
;;; Fill in the following template to produce a procedure that evaluates a
;;; polynomial using Horner's rule. Assume the coefficients are arranged
;;; in a sequance from a_0 to a_n.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) <??>)
              0
              coefficient-sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff
                                                   (* x higher-terms)))
              0
              coefficient-sequence))


;;; Exercise 2.35
;;; -------------
;;; Redefine count-leaves as an accumulation.

(define (count-leaves t)
  (accumulate <??> <??> (map <??> <??>)))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))


;;; Exercise 2.36
;;; -------------
;;; Fill in the missing expressions in the following definition of accumulate-n.

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init <??>)
            (accumulate-n op init <??>))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;;; Exercise 2.37
;;; -------------
;;; Fill in the missing expressions in the following procedures for computing
;;; the matrix operations. (The procedure accumulate-n is defined in exercise
;;; 2.36.)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))         ;map here is equivalent to accumulate-n

(define (matrix-*-vector m v)
  (map <??> m))
(define (transpose mat)
  (accumulate-n <??> <??> mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <??> m)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))
(define (transpose mat)
  (accumulate-n <??> <??> mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <??> m)))
