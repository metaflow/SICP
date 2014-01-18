(define x (cons 1 2))
(car x)
(cdr x)


(define (make-rat n d)
  (cons n d))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  x)

(define one-half
  (make-rat 1 2))

(print-rat one-half)

(define one-third
  (make-rat 1 3))

(print-rat one-third)

(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b))
               (* (numer b) (denom a)))
            (* (denom a) (denom b))))

(print-rat (add-rat one-half one-third))

(define (mul-rat x y)
  (make-rat (* (numer x)
               (numer y))
            (* (denom x)
               (denom y))))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(define (make-rat x y)
  (let ((g (gcd x y)))
    (cons (/ x g)
          (/ y g))))

(print-rat (add-rat one-third one-third))

; excercise 2.1

(define (make-rat x y)
  (if (< y 0)
      (make-rat (- x) (- y))
      (let ((g (gcd x y)))
        (cons (/ x g)
              (/ y g)))))

(print-rat (make-rat 2 6))
(print-rat (make-rat -2 6))
(print-rat (make-rat 2 -6))
(print-rat (make-rat -2 -6))

; excercise 2.2

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-segment s)
  (newline)
  (display "[")
  (print-point (start-segment s))
  (display ", ")
  (print-point (end-segment s))
  (display "]"))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define sample-segment
  (make-segment (make-point 1.5 3.6)
                (make-point -0.9 9.9)))

(print-segment sample-segment)

(define (mid-point-segment s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

(define (average x y)
  (/ (+ x y)
     2))

(print-point (mid-point-segment sample-segment))
