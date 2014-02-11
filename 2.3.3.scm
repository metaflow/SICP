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
; because each time we decrease size of at least one set thus maximum number
; of recursion steps is sum of sizes of sets that is O(m + n) <= O(2n) = O(n)
; having n > m

; TODO force bracket highlighter
