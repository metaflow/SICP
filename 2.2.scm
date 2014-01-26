(cons 1
  (cons 2
    (cons 3
      (cons 4 '())))) ; http://stackoverflow.com/a/9115801/167044

(define one-through-four
  (list 1 2 3 4))

(car one-through-four)
(cdr one-through-four)
(cons 5 one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(null? squares)
(null? '())

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length squares)

(define (length items)
  (define (lenght-iter a count)
    (if (null? a)
        count
        (lenght-iter (cdr a)
                     (+ count 1))))
  (lenght-iter items 0))

(length squares)

(define odds
  (list 1 3 5 7))

(append squares odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

(append odds squares)

; excercise 2.17

(define (last-pair items)
  (let ((a (car items))
        (tail (cdr items)))
    (if (null? tail)
        a
        (last-pair tail))))

(last-pair (list 23 72 149 34))

; excercise 2.18

(define (reverse items)
  (define (iter from to)
    (if (null? from)
        to
        (iter (cdr from)
              (cons (car from)
                    to))))
  (iter items '()))

(reverse squares)

; excercise 2.19

(define (cc amount coin-values)
  (cond ((= amount 0)
          1)
        ((or (< amount 0)
             (no-more? coin-values))
          0)
        (else
          (+ (cc amount (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values))
                 coin-values)))))

(define (no-more? coins)
  (null? coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define us-coins
  (list 50 25 10 5 1))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

; order of coins values does not affect answer as we eventually enumerate
; all possible ways to sum up to count. But order will affect structure of
; recursion tree and time to compute
; i.e. on my PC
;(runtime) ; 0.03 s
;(cc 100 uk-coins)
;(runtime) ; 15.24s -> descending list takes about 15s to compute
;(cc 100 (reverse uk-coins))
;(runtime) ; 73.5 -> takes about 60s to complete (x4 times slower)

;excercise 2.20

(define (same-parity . items)
  (define (iter is-odd from result)
    (define (add-parity x items)
      (if (equal? is-odd (odd? x))
        (cons x items)
        items))
    (if (null? from)
        result
        (iter is-odd (cdr from) (add-parity (car from) result))))
  (iter (odd? (car items))
        (reverse items)
        '()))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; ---------

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items)
               factor)
            (scale-list (cdr items)
                        factor))))

(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -1 0.5 -9.3 0))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 0 2 3.6) 10)

; excercise 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(square-list (list -1 6 9.2))

(define (square-list items)
  (map square items))

(square-list (list -1 6 9.2))

; excercise 2.22

; First implementation is how (reverse list) works beside the fact of squaring:
; if we move from one stack to another then order of elements will be reversed.
; Second will not work because pair of (list, value) is a tree but plain list
; he need to use (cons value list)

; excercise 2.23

; straightfoward way is to use existing 'map' procedure:

(define (for-each proc items)
  (map (lambda (x)
         (proc x)
         #t)
        items))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

; that interestingly enough prints list in a reverse order.
; To be explicit:

(define (for-each proc items)
  (cond ((null? items) #t)
        (else
          (proc (car items))
          (for-each proc (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

;

(cons (list 1 2) (list 3 4))
;for me there is more obvious way to write this:
(list (list 1 2) 3 4)

(define x
  (list (list 1 2) 3 4))

(length x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)

; excercise 2.24
; it will be (1 (2 (3 4))) (just remove 'list' word)
(list 1 (list 2 (list 3 4)))

; excercise 2.25

(define x
  (list 1 3 (list 5 7) 9))

(cdr (car (cdr (cdr x))))

(define x
  (list (list 7)))

(car (car x))

(define x
  (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;(cadr x) is (car (cdr x))

(cdr (cadr (cadr (cadr (cadr (cadr x))))))

; excercise 2.26

(define x
  (list 1 2 3))

(define y
  (list 4 5 6))

(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))

(define (deep-reverse items)
  (map reverse (reverse items)))

(define x (list (list 1 2) (list 3 4)))

(reverse x)
(deep-reverse x)

; better test

(define x (list (list 1 2) (list 3 4) 5))
; not working and we are to rewrite reverse to work with non-pairs too

(define (reverse x)
  (define (iter a result)
    (if (null? a)
        result
        (iter (cdr a)
              (cons (car a)
                    result))))
  (if (pair? x)
      (iter x '())
      x))

(reverse x)
(deep-reverse x)

; excercise 2.28

(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

(fringe x)
(fringe (list x x))

; excercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch lenght structure)
  (list lenght structure))

; a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define m (make-mobile (make-branch 1 2) (make-branch 3 4)))

(left-branch m)
(right-branch m)
(branch-length (right-branch m))
(branch-structure (right-branch m))

; b

(define (total-weight mobile)
  (if (pair? mobile)
    (+ (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))
    mobile))

(total-weight m)

; c

(define (balanced-mobile? mobile)
  (define (branch-torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (if (pair? mobile)
    (if (= (branch-torque (left-branch mobile))
           (branch-torque (right-branch mobile)))
        (and (balanced-mobile? (branch-structure (left-branch mobile)))
             (balanced-mobile? (branch-structure (right-branch mobile))))
        #f) ; no need to check balance futher
    #t)) ; simple weight is just balanced
;

(balanced-mobile? m)

(define m (make-mobile (make-branch 2 2) (make-branch 1 4)))

(balanced-mobile? m)

; d

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; we need to redefine just how we get right parts of our structures

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

(define m (make-mobile (make-branch 2 2) (make-branch 1 4)))

(total-weight m)
(balanced-mobile? m)

; excercise 2.30

(define (square-tree tree)
  (map (lambda (sub)
        (if (pair? sub)
            (square-tree sub)
            (square sub)))
       tree))

(square-tree (list 1 (list 2 3)))

(define (tree-map proc tree)
  (map (lambda (sub)
          (if (pair? sub)
              (tree-map proc sub)
              (proc sub)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(square-tree (list (list 1 2) (list 3 4)))

; excersise 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; the logic is that to get all subsets of set {a, b,...}
; we can join set of all sets with a and subsets {b, ...}
; with just subsets of {b, ...}
; simply: there are two sets of subsets in result - with and without element a

(subsets (list 1 2 3))


;
