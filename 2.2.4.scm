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

(define (frame-coord-map frame)
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
(xcord-vect va)
(ycord-vect va)

(scale-vect 0.5 va)
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

((frame-coord-map test-frame) (make-vect 0 0))
((frame-coord-map test-frame) (make-vect 1 1))

; --- Painters ---

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

; excercise 2.48

(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

(define test-segment (make-segment (make-vect 1 2) (make-vect 3.5 6.8)))

(start-segment test-segment)
(end-segment test-segment)



; excercise 2.49

; that will turn to be too verbose. Let's create version of make-segment
; that takes four coordinates instead of two vectors
; another nice option will be to be able to define polygonal lines only by
; specifying vertex points only

(define (make-segment-coord x-start y-start x-end y-end)
  (make-segment (make-vect x-start y-start)
                (make-vect x-end y-end)))

; for test purposes

(define (display-vector vector)
  (display "(")
  (display (xcord-vect vector))
  (display ", ")
  (display (ycord-vect vector))
  (display ")"))

(display-vector (make-vect 0.5 0.6))

(define (display-segment segment)
  (newline)
  (display "{")
  (display-vector (start-segment segment))
  (display "; ")
  (display-vector (end-segment segment))
  (display "}"))

(display-segment (make-segment-coord 1 2 3 4))

; this implementation of draw-line actually violates Liskov's substitution
; principle (see note 28 of this chapter). But as we cannot draw at canvas
; it's better to have something for test instead of hope

(define (draw-line start end)
  (display-segment (make-segment start end)))

; a.

(define outline->painter
  (segments->painter
    (list (make-segment-coord 0 0 0 1)
          (make-segment-coord 0 1 1 1)
          (make-segment-coord 1 1 1 0)
          (make-segment-coord 1 0 0 0))))

(define magnifying-frame
  (make-frame (make-vect 0 0)
              (make-vect 10 0)
              (make-vect 0 10)))

(outline->painter magnifying-frame)

; now with polygonal definition

(define (polygonal-line dots)
  (define (eq-vect? u v)
    (and (= (xcord-vect u)
            (xcord-vect v))
         (= (ycord-vect u)
            (ycord-vect v))))
  (define (remove-nil-segments segment-list)
    (filter
      (lambda (segment)
        (not (eq-vect? (start-segment segment)
                       (end-segment segment))))
      segment-list))

  (remove-nil-segments
    (fold-right
      (lambda (dot segments)
        (if (null? segments)
          (list (make-segment dot dot))
          (cons (make-segment dot (start-segment (car segments)))
                segments)))
    '()
    dots)))

(define outline->painter
  (segments->painter
    (polygonal-line
      (list (make-vect 0 0)
            (make-vect 0 1)
            (make-vect 1 1)
            (make-vect 1 0)
            (make-vect 0 0)))))

(outline->painter magnifying-frame)

;b.

(define diagonal->painter
  (segments->painter
    (list (make-segment-coord 0 0 1 1)
          (make-segment-coord 1 0 0 1))))

(diagonal->painter magnifying-frame)

;c.

(define diamond->painter
  (segments->painter
    (polygonal-line
      (list (make-vect 0.5 0)
            (make-vect 1 0.5)
            (make-vect 0.5 1)
            (make-vect 0 0.5)
            (make-vect 0.5 0)))))

(diamond->painter magnifying-frame)

;d.

; to ease our lives let's define painter combiner

(define (combine-painters painters)
  (lambda (frame)
    (for-each
      (lambda (p) (p frame))
      painters)))

((combine-painters (list outline->painter diagonal->painter)) magnifying-frame)

; and polygonal drawer

(define (polygonal->painter dots)
  (segments->painter (polygonal-line dots)))

((polygonal->painter (list (make-vect 1 2) (make-vect 3 4))) magnifying-frame)

; now bring it!

;d.

(define wave
  (combine-painters
   (list
    (polygonal->painter
      (list (make-vect 0 0.6)
            (make-vect 0.2 0.3)
            (make-vect 0.4 0.5)
            (make-vect 0.5 0.4)
            (make-vect 0.2 0)))
    (polygonal->painter
      (list (make-vect 0 0.8)
            (make-vect 0.2 0.5)
            (make-vect 0.4 0.6)
            (make-vect 0.3 0.8)
            (make-vect 0.4 1)))
    (polygonal->painter
      (list (make-vect 0.6 1)
            (make-vect 0.7 0.8)
            (make-vect 0.6 0.6)
            (make-vect 0.8 0.6)
            (make-vect 1 0.4)))
    (polygonal->painter
      (list (make-vect 0.4 0)
            (make-vect 0.5 0.3)
            (make-vect 0.6 0)))
    (polygonal->painter
      (list (make-vect 0.8 0)
            (make-vect 0.6 0.4)
            (make-vect 1 0.2))))))

(wave magnifying-frame)


