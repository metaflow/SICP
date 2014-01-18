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
