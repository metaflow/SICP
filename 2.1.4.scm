(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-inverval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-inverval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (make-interval x y)
  (cons x y))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

; excercise 2.7

(define a
  (make-interval -2.0 2.0))

(define b
  (make-interval 6.5 11.0))

(add-interval a b)
(mul-inverval a b)
(div-interval a b)

; excercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(sub-interval a b)

; excercise 2.9
; let a = [la, ua] and b = [lb, ub]
; we can define them as a = [ca - wa, ca + wa] and [cb - wb, cb + wb]
; to calculate a sum: a + b = [ca - wa + cb - wb, ca + wa + cb + wb] =
; = [cs - wc, cs + wc], where wc = wa + wb
; subtract: a - b = [ca - wa - cb - wb, ca + wa - cb + wb] = [c`s - wc, c`s + wc] with very same width
; as example that width of multiplication is not defined only
; by widhts of initial intervals

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(width (mul-inverval (make-interval 1.0 2.0)
               (make-interval 1.0 2.0))) ; 1.5

(width (mul-inverval (make-interval -20.0 -19.0)
              (make-interval 1.0 2.0))) ; 10.5

; widths differ but widths of initial intervals equal (0.5)

; excercise 2.10

(define (div-interval x y)
  (define (spans-zero? i)
    (not (or (> (lower-bound i) 0)
             (< (upper-bound i) 0))))
  (if (spans-zero? y)
      (error "devisor interval spans the zero")
      (mul-inverval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

(div-interval a b)
; (div-interval b a)

(define (mul-interval x y)
  (define (sign a)
    (cond ((> 0 a) 1)
          ((< 0 a) -1)
          (else 0)))
  (define (interval-sign i)
    (+ (sign (upper-bound i))
       (sign (lower-bound i))))
  (let ((sing-x (interval-sign x))
        (sign-y (interval-sign y))
        (lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (= sign-x 2) (= sign-y 2))
            (make-interval (* lx ly) (* ux uy)))
          ((and (= sign-x -2) (= sign-y 2))
            (make-interval (* lx uy) (* ux ly)))
          ((and (= sign-x 2) (= sign-y -2))
              (make-interval (* ux ly) (* lx uy)))
          ((and (= sign-x -2) (= sign-y -2))
              (make-interval (* ux uy) (* lx ly)))
          ((= sign-x 2)
              (make-interval (* ux ly) (* ux uy)))
          ((= sign-x -2)
              (make-interval (* lx uy) (* lx ly)))
          ((= sign-y 2)
              (make-interval (* lx uy) (* ux uy)))
          ((= sign-y -2)
              (make-interval (* ux ly) (* lx ly)))
          (else (let ((p1 (* ux uy))
                      (p2 (* lx uy))
                      (p3 (* ux ly))
                      (p4 (* lx luy)))
                (make-interval (min p1 p2 p3 p4)
                               (max p1 p2 p3 p4)))))))

(mul-inverval a b)

; -----------------

(define (make-center-width c w)
  (make-interval (- c w)
                 (+ c w)))

(define (center i)
  (/ (+ (upper-bound i)
        (lower-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

; excercise 2.12

(define (as-percent x)
  (* x 0.01))

(define (to-percent x)
  (* x 100))

(define (make-center-percent c p)
  (make-center-width c (* (abs c)
                          (as-percent p))))

(define (percent i)
  (to-percent (/ (width i)
                 (abs (center i)))))

; let's test it out

(define z (mul-inverval (make-center-percent 4 5)
              (make-center-percent 6.9 10)))

(percent z) ; ~= 15% that give some guess about answer to

; excercise 2.13
; let first interval will be [a - pa * a, a + pa * a]
; and second [b - pb * b, b + pb * b]
; in assumtion that all of interval ends are positive multiplication will be
; [(a - pa * a) * (b - pb * b), (a + pa * a) * (b + pb * b)]
; [ a*b - a*b*pb - a*b*pa + D, a*b + a*pb*b + b*pa*a + D]
; where D is close to zero as it involves production pa * pb. so we get
; [ c - pc * c, c + pc * c ], where c = a * b and * pc = pa + pb *

(define (par1 r1 r2)
  (div-interval (mul-inverval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
; excercise 2.14

(define (test-methods)
  (let ((r1 (make-center-percent 5 3))
      (r2 (make-center-percent 9 2)))
  (let ((p1 (par1 r1 r2))
       (p2 (par2 r1 r2)))
        (newline)
        (display "first method ")
        (display (center p1))
        (display " +- ")
        (display (percent p1))
        (display "%")
        (newline)
        (display "second method ")
        (display (center p2))
        (display " +- ")
        (display (percent p2))
        (display "%")
        )))

(test-methods)

;first method 3.2 +- 7.4%
;second method 3.2 +- 2.6%

; now let's test straight devision

(let ((a (make-center-percent 5 2))
      (b (make-center-percent 9 3))
      (one (make-center-percent 1 0)))
  (newline)
  (display (percent (div-interval a b))) ; ~= 5%
  (newline)
  (display (percent (div-interval one a))) ; ~= 2%
  (newline)
  (display (percent (div-interval a a))) ; ~= 4% (but we would expect 2%)
)

; looks like by deviding intervals we get sum of relative variations

; excercise 2.15 and 2.16
; the reason is that we treat multiple instances of same variable
; as independent i.e. expression "A / B" is equvivalent
; to "A / A" if A and B intervals are the same
; to address this we should go away from combinations of
; atomic interval operations.
; For general case this task is very hard indeed
; but for parallel resistor problem we can go with following

(define (function-interval f a b)
  (let ((f1 (f (lower-bound a)
               (lower-bound b)))
        (f2 (f (upper-bound a)
               (lower-bound b)))
        (f3 (f (lower-bound a)
               (upper-bound b)))
        (f4 (f (upper-bound a)
               (upper-bound b))))
    (make-interval (min f1 f2 f3 f4)
                   (max f1 f2 f3 f4))))

(define (par1 r1 r2)
  (function-interval
    (lambda (a b) (/ (* a b)
                     (+ a b)))
    r1
    r2))

(define (par2 r1 r2)
  (function-interval
    (lambda (a b) (/ 1.0
                     (+ (/ 1.0 a)
                        (/ 1.0 b))))
    r1
    r2))

(test-methods)

; now we get same results of '3.2 +- 2.6%' like former 'par2' method
; moreover we can now reuse any existing functions like

(function-interval
    (lambda (a b) (sqrt (- (square a)
                           (log b))))
    (make-center-width 5 0.1)
    (make-center-width 3 0.1))

; as long as function at hand has no local min-/maximums
; inside area bounded by intervals
