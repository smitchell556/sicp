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
  (let ((aug (cddr s)))
    (if (null? (cdr aug))
        (car aug)
        (cons '+ aug))))

(define (multiplicand p)
  (let ((mult (cddr p)))
    (if (null? (cdr mult))
        (car mult)
        (cons '* mult))))


;;; Exercise 2.58
;;; -------------
;;; Modify the differentiation program to handle infix operators.

;;; a) Show how to do this with forms like:
;;;    (x + (3 * (x + (y + 2))))
;;;    Assume + and * always take two arguments and that expressions are
;;;    fully parenthesized.

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list  m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
  (let ((aug (cddr s)))
    (if (null? (cdr aug))
        (car aug)
        aug)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p)
  (let ((mult (cddr p)))
    (if (null? (cdr mult))
        (car mult)
        mult)))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list base '** exponent))))

;;; b) Show how to do this with forms like:
;;;    (x + 3 * (x + y + 2))
;;;    which drops unnecessary parentheses and assumes that multiplication
;;;    is done before addition.

;; I actually solved this in part a) unknowingly.


;;; Exercise 2.59
;;; -------------
;;; Implement the union-set operation for the unordered-list representation of
;;; sets.

;; Given:

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Solution:

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        (else
         (let ((s1 (adjoin-set (car set2) set1)))
           (union-set s1 (cdr set2))))))


;;; Exercise 2.60
;;; -------------
;;; Rewrite the above procedures while allowing duplicates.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (intersection-set set1 set2)
  (define (i-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)        
           (cons (car set1)
                 (i-set (cdr set1) set2)))
          (else (i-set (cdr set1) set2))))
  (append (i-set set1 set2) (i-set set2 set1)))

(define (union-set set1 set2)
  (append set1 set2))


;;; Exercise 2.61
;;; -------------
;;; Give an implementation of adjoin-set using the ordered representation.

;; Given:

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; Solution:

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))


;;; Exercise 2.62
;;; -------------
;;; Give a Theta(n) implementation of union-set for sets represented as
;;; ordered lists.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      (else (cons x2 (union-set set1 (cdr set2)))))))))


;;; Exercise 2.63
;;; -------------
;;; Each of the following two procedures converts a binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; Given:

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;; a) Do the two procedures produce the same results for every tree? If not,
;;;    how do they differ? What lists do the procedures produce for the trees
;;;    in figure 2.16?

;; I believe both would return an ordered list.

;;          7
;;        /   \
;;       3     9
;;      / \     \
;;     1   5     11

;; (tree->list-1 (7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (cons 7 (tree->list-1 (9 () (11 () ())))))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (cons 7 (append '() (cons 9 (tree->list-1 (11 () ()))))))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (cons 7 (append '() (cons 9 (append '() (cons 11 '()))))))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (cons 7 (append '() (cons 9 (append '() (11))))))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (cons 7 (append '() (cons 9 (11)))))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (cons 7 (append '() (9 11))))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (cons 7 (9 11)))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (7 9 11))
;; (append (tree->list-1 (3 (1 () ()) (5 () ()))) (7 9 11))
;; (append (append (tree->list-1 (1 () ())) (cons 3 (tree->list-1 (5 () ())))) (7 9 11))
;; (append (append (tree->list-1 (1 () ())) (cons 3 (append '() (cons 5 '())))) (7 9 11))
;; (append (append (tree->list-1 (1 () ())) (cons 3 (append '() (5)))) (7 9 11))
;; (append (append (tree->list-1 (1 () ())) (cons 3 (5))) (7 9 11))
;; (append (append (tree->list-1 (1 () ())) (3 5)) (7 9 11))
;; (append (append (append '() (cons 1 '())) (3 5)) (7 9 11))
;; (append (append (append '() (1)) (3 5)) (7 9 11))
;; (append (append (1) (3 5)) (7 9 11))
;; (append (1 3 5) (7 9 11))
;; (1 3 5 7 9 11)

;; (tree->list-2 (7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;; (copy-to-list (7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))) '())
;; (copy-to-list (3 (1 () ()) (5 () ())) (cons 7 (copy-to-list (9 () (11 () ())) '())))
;; (copy-to-list (3 (1 () ()) (5 () ())) (cons 7 (copy-to-list '() (cons 9 (copy-to-list (11 () ()) '())
;; (copy-to-list (3 (1 () ()) (5 () ())) (cons 7 (copy-to-list '() (cons 9 (copy-to-list '() (cons 11 (copy-to-list '() '())))))))
;; (copy-to-list (3 (1 () ()) (5 () ())) (cons 7 (copy-to-list '() (cons 9 (copy-to-list '() (cons 11 '()))))))
;; (copy-to-list (3 (1 () ()) (5 () ())) (cons 7 (copy-to-list '() (cons 9 (copy-to-list '() (11))))))
;; (copy-to-list (3 (1 () ()) (5 () ())) (cons 7 (copy-to-list '() (cons 9 (11)))))
;; (copy-to-list (3 (1 () ()) (5 () ())) (cons 7 (copy-to-list '() (9 11))))
;; (copy-to-list (3 (1 () ()) (5 () ())) (cons 7 (9 11)))
;; (copy-to-list (3 (1 () ()) (5 () ())) (7 9 11))
;; (copy-to-list (1 () ()) (cons 3 (copy-to-list (5 () ()) (7 9 11))))
;; (copy-to-list (1 () ()) (cons 3 (copy-to-list '() (cons 5 (copy-to-list '() (7 9 11))))))
;; (copy-to-list (1 () ()) (cons 3 (copy-to-list '() (cons 5 (7 9 11)))))
;; (copy-to-list (1 () ()) (cons 3 (copy-to-list '() (5 7 9 11))))
;; (copy-to-list (1 () ()) (cons 3 (5 7 9 11)))
;; (copy-to-list (1 () ()) (5 7 9 11))
;; (copy-to-list '() (cons 1 (copy-to-list '() (5 7 9 11))))
;; (copy-to-list '() (cons 1 (5 7 9 11)))
;; (copy-to-list '() (1 5 7 9 11))
;; (1 5 7 9 11)

;;     3
;;   /   \
;;  1     7
;;      /   \
;;     5     9
;;            \
;;             11

;; (3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))

;; (tree->list-1 (3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
;; (1 3 5 7 9 11)

;; (tree->list-2 (3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
;; (1 3 5 7 9 11)

;;           5
;;        /     \
;;       3       9
;;     /       /   \
;;    1       7    11

;; (5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))

;; (tree->list-1 (5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
;; (1 3 5 7 9 11)

;; (tree->list-2 (5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
;; (1 3 5 7 9 11)

;;; b) Do the two procedures have the same order of growth in the number of
;;;    steps required to convert a balanced tree with n elements to a list?
;;;    If not, which one grows more slowly?

;; They have a similar order of growth since each tree node is visited once, but
;; tree->list-1 uses append while tree->list-2 does not. Since append creates
;; new elements for all but the last list, and append is called on repeated
;; half increments of the tree, tree->list-1 is Theta(nlgn) while tree->list-2
;; is Theta(n) since it relies only on cons which is a constant time operation.


;;; Exercise 2.64
;;; -------------
;;; list->tree converts an ordered list to a balanced binary tree.

;; Given:

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;; a) Write a short paragraph explaining as clearly as you can how
;;;    partial-tree works. Draw the tree produced by list->tree for the list
;;;    (1 3 5 7 9 11).

;; partial-tree splits the elts list into three parts: 1) the first s elements
;; where s is the floor of (n-1)/2, 2) the s+1 element (which is equal to the
;; floor of n/2), and 3) the remaining elements through the end of the list.
;; The elements from 1) and 3) are passed to partial-tree with their
;; corresponding sizes. The return values are balanced trees of the elements of
;; those lists. A new tree is made where the element from 2) is the root of the
;; tree, and the trees made from 1) and 3) are the left and right branches
;; respectively. This tree is consed with whatever elements were not used to
;; create the tree and the cons cell is returned.

;; (1 3 5 7 9 11)

;; Step-by-step tree construction showing the cons cell returned at each step
;; as a tree and the remaining elements.

;;         3     (5 7 9 11)

;;         1     (5 7 9 11)
;;          \
;;           3

;;         7     (9 11)

;;         11    ()

;;         9     ()
;;        / \
;;       7   11

;;         5     ()
;;       /   \
;;      1     9
;;       \   / \
;;        3 7   11

;;; b) What is the order of growth in the number of steps required by list->tree
;;;    to convert a list of n elements?

;; list->tree calls make-tree once per each element and since partial-tree only
;; uses cons (not append), the construction of the cons cells is a constant-time
;; operation so the order of growth of Theta(n).


;;; Exercise 2.65
;;; -------------
;;; Give Theta(n) implementations of union-set and intersection-set for sets
;;; implemented as (balanced) binary trees.

;; Given:

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (intersection-set-ordered-list set1 set2)	; From section 2.3.3
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (union-set-ordered-list set1 set2) ; From ex 2.62
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      (else (cons x2 (union-set set1 (cdr set2)))))))))

;; Solution:

(define (set-operation fn set1 set2)
  (let ((list-set1 (tree->list-2 set1))
	(list-set2 (tree->list-2 set2)))
    (let ((single-list (fn list-set1 list-set2)))
      (list->tree single-list))))

(define (union-set set1 set2)
  (set-operation union-set-ordered-list set1 set2))

(define (intersection-set set1 set2)
  (set-operation intersection-set-ordered-list set1 set2))


;;; Exercise 2.66
;;; -------------
;;; Implement the lookup procedure for the case where the set of records is
;;; structured as a binary tree, ordered by the numerical values of the keys.

;; Given:

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; Solution:

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (entry set-of-records))) (entry set-of-records))
	((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
	(else (lookup given-key (right-branch set-of-records)))))


;;; Exercise 2.67
;;; -------------
;;; Define an encoding tree and a sample message:

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;; Solution:

(decode sample-message sample-tree)
;; (a d a b b c a)
