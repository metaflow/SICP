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
(div-interval b a)

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
