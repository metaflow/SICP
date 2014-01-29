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

;
;
