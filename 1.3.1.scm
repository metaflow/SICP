;(define (sum term a next b)
;    (if (> a b)
;        0
;        (+ (term a)
;           (sum term
;                (next a)
;                next
;                b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a))))
    )
  (iter a 0))

(define (inc a) (+ a 1))

(define (sum-cubes a b) (sum cube a inc b))

(define (cube n)
  (* n n n))

(define (identity x)
  x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
    (define (pi-term x)
      (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
      (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
    (define (add-dx x)
      (+ x dx))
    (* (sum f
            (+ a (* dx 0.5))
            add-dx
            b
        )
        dx))

 ; excercise 1.29

(define (s-integral f a b n)
    (define (s-dx-integral f a b dx)
      (define (s-add x)
        (+ x (* 2.0 dx)))
      (define (s-term x)
        (* (/ dx 3.0)
         (+ (f (- x dx))
            (* 4.0 (f x))
            (f (+ x dx)))))
      (sum s-term
           (+ a dx)
           s-add
           b))
    (s-dx-integral f a b (/ (- b a)
                         (* 1.0 n))))

(integral cube 0 1 0.0001)
(s-integral cube 0 1 1000)

; excercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a))))
    )
  (iter a 0))

; excercise 1.31
; a

(define (product term a next b)
    (define (iter a result)
      (if (> a b)
         result
         (iter (next a)
               (* result (term a)))))
    (iter a 1.0))

(define (factorial n)
  (product identity 1 inc n))

(factorial 3)

(define (pi-product n)
  (define (next x)
    (+ x 2))
  (define (term x)
    (* (/ (- x 1) x)
       (/ (+ x 1) x)))
  (* 4 (product term 3 next (+ 3 n)))
)

(pi-product 100000)

; b
(define (product-recursion term a next b)
  (if (> a b)
      1.0
      (* (term a)
         (product term
                  (next a)
                  next
                  b))))

;excercise 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1.0 term a next b))

(sum identity 1 inc 3)
(factorial 4)

;excercise 1.33

(define (filter-accumulate filter combiner null-value term a next b)
    (define (iter a result)
        (cond ((> a b)
                 result)
              ((filter a)
                (iter (next a)
                      (combiner result (term a))))
              (else (iter (next a)
                          result)))
    )
    (iter a null-value)
)

; a
(define (sum-of-squares-of-prime a b)
  (filter-accumulate prime? + 0 square a inc b))

;include from 1.2

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(sum-of-squares-of-prime 2 10) ; expected 87 = 2*2 + 3*3 + 5*5 + 7*7

(define (product-of-relative-primes-to n)
    (define (filter x)
      (= 1 (gcd x n)))
  (filter-accumulate filter * 1.0 identity 2 inc n))

(product-of-relative-primes-to 9) ; expected 2240 = 2 * 4 * 5 * 7 * 8
