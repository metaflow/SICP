-- 1.3.3 ---

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
      (if (close-enought? neg-point pos-point)
          midpoint
          (let ((v (f midpoint)))
              (cond ((positive? v) (search f neg-point midpoint))
                    ((negative? v) (search f midpoint pos-point))
                    (else midpoint))))))

(define (average x y)
  (/ (+ x y)
     2))

(define (close-enought? x y)
  (< (abs (- x y))
     0.001))

(search (lambda (x) (- x 1)) -1 3.5) ; expected 1.0

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
      (cond ((and (negative? a-value) (positive? b-value))
            (search f a b))
            ((and (positive? a-value) (negative? b-value))
            (search f b a))
            (else (error "Values are not of opposite sign" a-value b-value))))) ; yes I think that you are more interested in (f a) (f b)

(half-interval-method sin 2.0 4.0) ;Value: 3.14111328125
(half-interval-method sin 4.0 2.0) ;Value: 3.14111328125
; check for error (half-interval-method sin 1.0 2.0) ;Value: 3.14111328125

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0) ; 1.89306640625

(define tolerance 0.00001)

(define (close-enought? x y)
    (< (abs (- x y))
       tolerance)); will be used later

(define (fixed-point f first-guess)
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
        (if (close-enought? next guess)
            next
            (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 4.0)
(sqrt 2.0)

; 1.35

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
            1.0))

golden-ratio ;1.6180327868852458

; 1.36

(define right-side 1000)
(fixed-point (lambda (x) (/ (log right-side)
                            (log x)))
             2.0)

; 34 steps ~= 999.991
; mid-point
(fixed-point (lambda (x) (average (/ (log right-side)
                                     (log x))
                                  x))
             2.0)
;9 steps ~= 1000.004

; 1.37

(define (cont-fract n d k)
  (define (iter part left)
    (if (= left 0)
        part
        (iter (/ (n left)
                 (+ part (d left)))
              (- left 1))))
  (iter 0 k))

(define tolerance 0.00001)

(define (find-cont-fract-term f expected k)
  (if (close-enought? (f k)
                      expected)
      k
      (find-cont-fract-term f expected (+ k 1))))

(define (golden-ratio-approximation k)
  (/ 1.0 (cont-fract (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     k)))

(find-cont-fract-term (lambda (k) (golden-ratio-approximation k))
        1.61803398875
        0) ; 12

(golden-ratio-approximation 12) ; ~= 1.618

; excercise 1.38

(define (e-approximation k)
  (+ 2
    (cont-fract (lambda (i) 1.0)
                (lambda (i)
                  (let ((j (+ i 1)))
                      (if (= (remainder j 3) 0)
                          (* (/ j 3) 2.0)
                          1.0)))
                k)))

(e-approximation 5)


(find-cont-fract-term (lambda (k) (e-approximation k))
        2.7182818284
        0) ; 8

(e-approximation 8) ; ~= 2.7183

(define (tan-cf x k)
  (- (cont-fract (lambda (i) (- (expt x i)))
                 (lambda (i) (- (* i 2) 1))
                 k)))

(abs (- (tan 1.0)
        (tan-cf 1.0 10))) ; 2e-16

; --- 1.3.4 ---

(define (average-dump f)
  (lambda (x) (average x (f x))))

((average-dump square) 10)

(define (sqrt x)
  (fixed-point (average-dump (lambda (y) (/ x y)))
               1.0))

(sqrt 4)

(define (cube-root x)
  (fixed-point (average-dump (lambda (y) (/ x (square y))))
               1.0))

(cube-root 8)

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx))
          (g x)) dx)))

((deriv (lambda (x) (expt x 3))) 5)

(define (newtow-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtow-method g guess)
  (fixed-point (newtow-transform g) guess))

(define (sqrt x)
  (newtow-method (lambda (y) (- x (square y)))
                 1.0))

(sqrt 9)


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-dump
                            1.0))

(sqrt 25)

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- x (square y)))
                            newtow-transform
                            1.0))

(sqrt 25)

; 1.40

(define (cubic-polynome a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (cube x)
  (* x x x))

(newtow-method (cubic-polynome 1 1 6) 0.0) ; expected -2

; 1.41

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)

; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

; 1.43

; recursive

(define (repeated f n)
  (if (< 2 n)
      f
      (compose f (repeated f (- n 1)))))

((repeated inc 1) 3)
((repeated inc 5) 3)

; iterative

(define (repeated f n)
  (define (iter g f n)
    (cond ((< n 1) g) ; to handle float numbers too
          (else (iter(compose f g)
                               f
                               (- n 1)))))
 (iter (lambda (x) x) f n))

((repeated square 3) 4) ; 4^(2^3)

; 1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx))
       )
       3)))

(define dx 0.1)

((smooth abs) 0)
((smooth cos) 0)
((smooth abs) 1.0)

(((repeated smooth 3) abs) 0)
(((repeated smooth 3) cos) 0)
(((repeated smooth 3) abs) 1.0)

; 1.45

(define (cube-root x)
  (fixed-point-of-transform (lambda (y) (/ x (square y)))
                            average-dump
                            1.0))

(cube-root 27)

(define (n-th-root-experiment x n dumps)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-dump dumps)
                            1.0))

(define dx 0.1)

(n-th-root-experiment 4 2 1)
(n-th-root-experiment 27 3 1)
(n-th-root-experiment 16 4 2)
(n-th-root-experiment 16 4.5 2)
(n-th-root-experiment -32 5 2)
(n-th-root-experiment 64 6 2)
(n-th-root-experiment 128 7 2)
(n-th-root-experiment 256 8 3)

; so looks like we need to use log n base 2 as a count of average-dumps

(define (loge a b)
  (/ (log a)
     (log b)))

(loge 8 2)

(define (n-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-dump (loge n 2)) ; note: we need (repeated f n) to accept floats to use it like this
                            1.0))
(n-root 256 8)
(n-root 256 2)
(n-root 256 2.5)

; 1.46

(define (iterative-improve test improve)
  (lambda (guess)
    (define (iter x)
      (if (test x)
          x
          (iter (improve x))))
    (iter guess)))

((iterative-improve (lambda (x) (> x 5)) inc) 0)

(define (sqrt a)
  ((iterative-improve (lambda (x) (close-enought? (square x) a))
                      (lambda (x) (average x (/ a x))))
   1.0))

(sqrt 4)

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (x) (close-enought? x (f x)))
                      f)
   first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x)))
            1.0) ; golden-ratio
