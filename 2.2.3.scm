; from previous part
; I will use 'my-map' as we eventurally need standard Scheme map procedure

(define (my-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (my-map proc (cdr items)))))

(my-map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
          (cons (car sequence)
            (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))
(accumulate list '() (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1)
                                    high))))

(enumerate-interval 1 4)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((pair? tree) (append (enumerate-tree (car tree))
                              (enumerate-tree (cdr tree))))
        (else (list tree))))

(enumerate-tree (list 1 (list 2 3 4 (list 5))))

(define (sum-odd-square tree)
  (accumulate + 0
    (my-map square
        (filter odd?
                (enumerate-tree tree)))))

(sum-odd-square (list 1 2 (list 3 4 (list 5))))

(define (fib n)
  (define (iter a b n)
    (if (= 0 n)
        a
        (iter b
              (+ a b)
              (- n 1))))
  (iter 0 1 n))

(my-map fib (enumerate-interval 0 8))

(define (even-fibs n)
  (accumulate
    cons
    '()
    (filter even?
            (my-map fib
                 (enumerate-interval 0 n)))))

(even-fibs 15)

; actually we don't need accumulate cons on list
; (I guess authors use this for further generalization later)

(define (even-fibs n)
  (filter even?
          (my-map fib
               (enumerate-interval 0 n))))

(even-fibs 15)

; excercise 2.33

(define (my-map p sequence)
  (accumulate
    (lambda (x y)
      (cons (p x)
            y))
    '()
     sequence))

(my-map square (list 1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))
;

(define (length sequence)
  (accumulate
    (lambda (x y) (+ y 1))
    0
    sequence))

(length '())
(length (list 1))
(length (list 2 3 4))

; excercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* x higher-terms)))
    0
    coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))  ; = 79

; excercise 2.35

(define (count-leaves t)
  (accumulate
    +
    0
    (my-map (lambda (x)
      (cond ((null? x) 0)
            ((pair? x) (count-leaves x))
            (else 1)))
         t)))

(count-leaves (list 1 2 (list 3 4 (list 5 6) (list (list 7)) '())))

; excercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (my-map car seqs))
            (accumulate-n op init (my-map cdr seqs)))))

(accumulate-n + 0 (list
                    (list 1 2 3)
                    (list 4 5 6)
                    (list 7 8 9)
                    (list 10 11 12)))

; excercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 0.1 0.2 0.3))

(define (matrix-*-vector m v)
  (map (lambda (x)
        (dot-product v x))
    m))

(matrix-*-vector (list (list 1 2 3) (list -1 0 2)) (list 1 0.9 6))

(define (transponse m)
  (accumulate-n cons '() m))

(transponse (list (list 1 2) (list 3 4) (list 5 6)))

(define (matrix-*-matrix m n)
  (let ((cols (transponse n)))
    (map (lambda (x)
      (matrix-*-vector cols x))
     m)))

(matrix-*-matrix (list (list 1 2 3)
                       (list 0 1 0))
                 (list (list 0 1)
                       (list 3 0)
                       (list 0.5 1)))

; excercise 2.38

(define (my-fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

; the 'op' should be commutative to results of folding left or right be equal
; for example
; addition
(fold-left + 0 (list 1 2 3))
(fold-right + 0 (list 1 2 3))
; or conjuction
(define (conjuction a b)
  (and a b))
(fold-left conjuction #t (list #f #t))
(fold-right conjuction #t (list #f #t))
;

; excercise 2.39

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x))
             '()
             sequence))

(reverse (list 1 2 3 4))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x)))
              '()
              sequence))

(reverse (list 9 8 7 6))

; ----- Nested Mappings -----

(define (generate-pairs n)
  (accumulate append
  '()
  (map (lambda (i)
          (map (lambda (j) (list i j))
              (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n))))

(generate-pairs 5)

(define (flatmap op sequence)
  (accumulate append
              '()
              (map op sequence)))

(define (prime-sum? pair)
  (prime? (+ (car pair)
             (cadr pair))))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= n (smallest-divisor n)))

(prime-sum? (list 1 2))
(prime-sum? (list 2 2))

(define (make-pair-sum pair)
  (let ((x (car pair))
        (y (cadr pair)))
    (list x y (+ x y))))

(make-pair-sum (list 1 2))

(define (prime-sum-pairs n)
  (map make-pair-sum
    (filter prime-sum?
      (flatmap (lambda (i)
                 (map (lambda (j)
                        (list i j))
                      (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                  (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
               s)))

(define (remove x sequence)
  (filter (lambda (y) (not (= x y))) sequence))

(permutations (list 1 2 3))

; excercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
              (map (lambda (j)
                      (list i j))
                   (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 3)

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)

; excercise 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
            (map (lambda (p) (cons i p))
                 (unique-pairs (- i 1))))
    (enumerate-interval 1 n)))

(unique-triples 4)

(define (sequence-sum sequence)
  (accumulate + 0 sequence))

(define (exact-sum-triples n s)
  (filter (lambda (t) (= s (sequence-sum t)))
    (unique-triples n)))

(exact-sum-triples 10 12)

; excercise 2.42

(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
           (lambda (position)
             (safe? k position))
           (flatmap
            (lambda (rest-of-queens)
              (map
                (lambda (new-row)
                  (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
            (queens-cols (- k 1))))))
  (queens-cols board-size))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k position)
  (define (safe-part? col-distance x rows)
    (if (null? rows)
        #t
        (let ((y (car rows))
              (rest (cdr rows)))
          (cond ((= x y) #f)
                ((= col-distance (abs (- x y))) #f)
                (else (safe-part?
                        (+ col-distance 1)
                        x
                        rest))))))
  (if (null? position)
      #f
      (safe-part?
        1
        (car position)
        (cdr position))))

(queens 8)

; number of solutinos is 92
; http://en.wikipedia.org/wiki/Eight_queens_puzzle#Solutions)

(fold-right (lambda (x y) (+ y 1)) 0 (queens 8))

; excercise 2.43

; original implementation calls (queen-cols k) only once for every value of k
; Louis implementation do
; 8 calls of (queen-cols (- k 1)),
; 64 of (queen-cols (- k 2)) and so on.
; we will have recoursion tree with 8^8 leaves or 2^24 ~= 16M for 8 x 8 board
