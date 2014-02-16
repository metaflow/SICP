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
          (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 3) '(2 3 4))

; excercise 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2)
        )
        (else (cons (car set1)
                    (union-set (cdr set1)
                               set2)))))

(union-set '() '())
(union-set '(1) '())
(union-set '(1) '(1))
(union-set '(1 2 4) '(2 3))

; excercise 2.60
; representation of element-of-set stays the same and require O(m) time
; where m - count of elements including duplicates. It's possible that m >> n
; same stands for intersect-set (O(m^2))
; Note that speed and result representation of operation
; now depends on order of arguments

(intersection-set '(1 2 3 3 6 6) '(2 2 2 4 4 6)) ; (2 6 6)
(intersection-set '(2 2 2 4 4 6) '(1 2 3 3 6 6)) ; (2 2 2 6)

; adjoin-set and union-set can be simplified to

(define (adjoin-set x set)
  (cons x set))

(adjoin-set 1 '(1 2))

(define (union-set set1 set2)
  (append set1 set2))

(union-set '(1 2 3) '(2 3 4))

; this approach is viable when you
; - have to frequently merge sets with low number of checks against result
; i.e. have to decide if specific value is present in group of sets
; or result of multiple adjoints
; - encounter sets which elements unique most of the time. i.e. you operate on
; several sets of size 100 of integers from range 1..10e6
; (with uniform probability)

; sorted sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              (else (intersection-set set1 (cdr set2)))))))

(intersection-set '(1 2 4 8) '(2 5 8 10))

; excercise 2.61

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(adjoin-set 1 '())
(adjoin-set 1 '(2))
(adjoin-set 1 '(-1))
(adjoin-set 1 '(-1 3))

; excercise 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
          (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1)
                                      (cdr set2))))
                ((< x1 x2)
                  (cons x1 (union-set (cdr set1)
                                      set2)))
                (else (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '() '())
(union-set '(1) '())
(union-set '() '(1))
(union-set '(1) '(1))
(union-set '(1 2 4 9) '(1 4 6))

; this union-set has time complexity of O(n)
; each time we decrease size of at least one element thus maximum number
; of recursion steps is sum of sizes of sets that is O(n)

; --- Sets as binary trees ---

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
          (element-of-set? x (left-branch set)))
        (else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x
                                (left-branch set))
                     (right-branch set)))
        (else
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x
                                (right-branch set))))))

(adjoin-set 1 (make-tree 4
                        (make-tree 2 '() '())
                        (make-tree 9 '() '())))

; excercise 2.63

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

; a. both procedures define in-order traversal - you get sorted list as result
; for example

(define (make-leaf v)
  (make-tree v '() '()))

(define tree-216-a
  (make-tree 7
    (make-tree 3
      (make-leaf 1)
      (make-leaf 5))
    (make-tree 9
      '()
      (make-leaf 11))))

(tree->list-1 tree-216-a)
(tree->list-2 tree-216-a)

; b. order of grown is the same - O(n) - every iteration you extract yet
; another element from tree

; excercise 2.64

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

; partial tree gets an list with N and returns pair
; (tree from first N elements; remaining elements)
; to construct a subtree from first N elements we first get a
; tree from from (N - 1)/2 elements plus remaining ones (this will be left branch)
; first element of remaining ones (middle of interval 1..N)
; tree from all other elements (will be right branch)

; in short we recoursively get middle of list as root of tree and construct
; branches from elements to the left and to the right
; Result tree is balanced by definition - size of left and right branch
; differ no more then 1




(list->tree (list 1 3 5 7 9 11))
; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;    5
;   / \
;  1   9
; /   / \
;3   7  11

