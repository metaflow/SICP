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

; excercise 2.3
; simpliest representation as a pair of points with sides parallel to axes

(define (make-rectangle top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left rectangle)
  (car rectangle))

(define (bottom-right rectangle)
  (cdr rectangle))

(define (bottom-left rectangle)
  (make-point (x-point (top-left rectangle))
              (y-point (bottom-right rectangle))))

(define (width-rectangle rectangle)
  (segment-length (make-segment (bottom-left rectangle)
                                (bottom-right rectangle))))

(define (height-rectangle rectangle)
  (segment-length (make-segment (top-left rectangle)
                                (bottom-left rectangle))))

(define (segment-length s)
  (sqrt (+ (square (- (x-point (start-segment s))
                   (x-point (end-segment s))))
           (square (- (y-point (start-segment s))
                   (y-point (end-segment s)))))))

(define (perimeter-rectangle rectangle)
  (* 2 (+ (width-rectangle rectangle)
          (height-rectangle rectangle))))

(define (area-rectangle rectangle)
  (* (width-rectangle rectangle)
     (height-rectangle rectangle)))

(define sample-rectangle
  (make-rectangle (make-point 1 2)
                  (make-point 6 -1)))

(height-rectangle sample-rectangle)
(width-rectangle sample-rectangle)
(perimeter-rectangle sample-rectangle)
(area-rectangle sample-rectangle)

;now let's define rectangle as a diagonal vector (segment) and angle in rads to side in clockwise direction

(define (make-rectangle segment angle)
  (cons segment angle))

; now we are going to re-define width and height representation

(define (width-rectangle rectangle)
  (* (cos (cdr rectangle))
     (segment-length (car rectangle))))

(define (height-rectangle rectangle)
  (* (sin (cdr rectangle))
     (segment-length (car rectangle))))

(define pi 3.141592653589793)

(define sample-rectangle
  (make-rectangle (make-segment (make-point 1 1)
                                (make-point 4 4))
                  (/ pi 4)))

(height-rectangle sample-rectangle)
(width-rectangle sample-rectangle)
(perimeter-rectangle sample-rectangle)
(area-rectangle sample-rectangle)

; excercise 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define z (cons 1 2))
(car z)
(cdr z)

; excercise 2.5

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (loge a b)
  (/ (log a)
     (log b)))

; beware - this is not correct implementation
(define (car z)
  (round (loge (gcd z (expt 2 (truncate (loge z 2))))
               2)))

(define (cdr z)
  (round (loge (gcd z (expt 3 (truncate (loge z 3))))
               3)))

(define sample-pair (cons 2 3))
(car sample-pair)
(cdr sample-pair)

(define sample-pair (cons 26 43))

sample-pair
(car sample-pair)
(cdr sample-pair) ; 16 but 43

; let's search for error
(truncate (loge sample-pair 3)) ; correct
(expt 3 (truncate (loge sample-pair 3)))

; 1.4130386091738735e28 but 14130386091738734504764811067 is expected
; so 'expt' powering is not precise enough
; let's use direct integer power implementation

(define (power a b)
  (define (iter r a b)
    (cond ((= b 0) r)
          ((even? b) (iter r
                           (* a a)
                           (/ b 2)))
          (else (iter (* r a)
                      a
                      (- b 1)))))
  (iter 1 a b))

(power 3 59) ; looks good

(define (car z)
  (round (loge (gcd z (power 2 (truncate (loge z 2))))
               2)))

(define (cdr z)
  (round (loge (gcd z (power 3 (truncate (loge z 3))))
               3)))

(car sample-pair)
(cdr sample-pair) ; that's much better

; note that car O(log x) and cdr O(log y) (not as expected O(1) for previous pair operations)

; excercise

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f)
                    x)))))

; one is (add-1 zero) is
;(lambda (f) (lambda (x) (f x))) === (f x)
; two is (add-1 one) and is (f (f x))
(define one
  (lambda (f)
   (lambda (x) (f x))))

((one square) 2)

(define two
  (lambda (f)
   (lambda (x) (f (f x)))))

((two square) 2)

; thus addition is defined as sum up how many times we should apply arbitrary function to given argument
; to write 'a-times apply f' is simply as '((a f) x)'
; to apply f b more times to result of '((a f) x)' is ((b f) ((a f) x)))

(define (add a b)
  (lambda (f)
    (lambda (x)
     ((a f) ((b f) x)))))

(((add zero zero) square) 2)
(((add one zero) square) 2)
(((add one one) square) 2)
(((add two one) square) 2)
(((add two two) square) 2)

