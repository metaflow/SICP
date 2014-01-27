; from previous part

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(map square (list 1 2 3 4 5))

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
    (map square
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

(map fib (enumerate-interval 0 8))

(define (even-fibs n)
  (accumulate
    cons
    '()
    (filter even?
            (map fib
                 (enumerate-interval 0 n)))))

(even-fibs 15)

; actually we don't need accumulate cons on list
; (I guess authors use this for further generalization later)

(define (even-fibs n)
  (filter even?
          (map fib
               (enumerate-interval 0 n))))

(even-fibs 15)
