; ----- 2.2.4 -----

;excercise 2.44

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let
        ((top (up-split painter (- n 1))))
        (below painter (beside top top)))))

(define (split f s)
  (define (step painter n)
    (if (= 0 n)
      painter
      (f (s (step painter (- n 1))))))
  (lambda (painter n) (step painter n)))

;(define right-split (split beside below))
;(define up-split (split beside below))

; --- Frames ---

(define (frame-coor-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcord-vect v)
                            (edge1-frame frame))
                (scale-vect (ycord-vect v)
                            (edge2-frame frame))))))

; excercise 2.46

(define (make-vect x y)
  (list x y))

(define (xcord-vect v)
  (car v))

(define (ycord-vect v)
  (cadr v))

(define (add-vect v u)
  (make-vect
    (+ (xcord-vect v)
       (xcord-vect u))
    (+ (ycord-vect v)
       (ycord-vect u))))

(define (scale-vect s v)
   (make-vect
    (* s (xcord-vect v))
    (* s (ycord-vect v))))

(define (negate-vect v)
  (make-vect
    (- (xcord-vect v))
    (- (ycord-vect v))))

(define (sub-vec v u)
  (add-vect v (negate-vect u)))


(define va (make-vect 1 2))
(define vb (make-vect 3 4))

(scale-vect va 0.5)
(add-vect va vb)
(sub-vec va vb)

; excercise 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cadr (cdr frame)))

(define vo (make-vect -1 -3))

(define test-frame
  (make-frame vo va vb))
(origin-frame test-frame)
(edge1-frame test-frame)
(edge2-frame test-frame)

; second implementation

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; origin and edge1 implementations are the same

(define (edge2-frame frame)
  (cdr (cdr frame)))


(define test-frame
  (make-frame vo va vb))
(origin-frame test-frame)
(edge1-frame test-frame)
(edge2-frame test-frame)

((frame-coor-map test-frame) (make-vect 0 0))
((frame-coor-map test-frame) (make-vect 1 1))
