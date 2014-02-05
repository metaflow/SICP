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

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let
        ((top (up-split painter (- n 1))))
        (below painter (beside top top)))))

(define (split f s)
  (lambda (painter n)
    (define (step i)
      (if (= i 0)
        painter
        (let ((part (step (- i 1))))
             (f painter (s part part)))))
    (step n)))


; I had moved target definitions under 'beside' and 'below'

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

(define (sub-vect v u)
  (add-vect v (negate-vect u)))


(define va (make-vect 1 2))
(define vb (make-vect 3 4))
(xcord-vect va)
(ycord-vect va)

(scale-vect 0.5 va)
(add-vect va vb)
(sub-vect va vb)

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
; check http://jsfiddle.net/rHuEq/1 where you can paste output of
; multiple 'display-segment' and see the picture! (use ort-frame as a frame)

(define (display-vector vector)
  (display "{x: ")
  (display (xcord-vect vector))
  (display ", y:")
  (display (ycord-vect vector))
  (display "}"))

(display-vector (make-vect 0.5 0.6))

(define (display-segment segment)
  (newline)
  (display "{start: ")
  (display-vector (start-segment segment))
  (display ", end: ")
  (display-vector (end-segment segment))
  (display "},"))

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
; so all painters in the list share same frame

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

; Transforming and combining painters

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame
            (m origin)
            (sub-vect (m corner1)
                      new-origin)
            (sub-vect (m corner2)
                      new-origin)))))))

(define (flip-vert painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 1 1)
    (make-vect 0 0)))

(define test-painter
  (segments->painter (list (make-segment (make-vect 0 0.5)
                                         (make-vect 0.5 1)))))

(define ort-frame
  (make-frame (make-vect 0 0)
              (make-vect 1 0)
              (make-vect 0 1)))

(test-painter ort-frame)

((flip-vert test-painter) ort-frame)

(define (shrink-to-upper-right painter)
  (transform-painter painter
    (make-vect 0.5 0.5)
    (make-vect 1 0.5)
    (make-vect 0.5 1)))

((shrink-to-upper-right test-painter) ort-frame)

(define (rotate90 painter)
  (transform-painter painter
    (make-vect 1 0)
    (make-vect 1 1)
    (make-vect 0 0)))

(define (beside painter1 painter2)
  (let ((paint-left
          (transform-painter
            painter1
            (make-vect 0 0)
            (make-vect 0.5 0)
            (make-vect 0 1)))
        (paint-right
          (transform-painter
            painter2
            (make-vect 0.5 0)
            (make-vect 1 0)
            (make-vect 0.5 1))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

((beside test-painter diamond->painter) ort-frame)

; excercise 2.50

(define (flip-horiz painter)
  (transform-painter painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)))

((flip-horiz test-painter) ort-frame)

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

((rotate180 test-painter) ort-frame)

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

((rotate270 test-painter) ort-frame)

; excercise 2.51
; analogous to beside

(define (below painter1 painter2)
  (let ((paint-bottom
          (transform-painter
            painter1
            (make-vect 0 0)
            (make-vect 1 0)
            (make-vect 0 0.5)))
        (paint-top
          (transform-painter
            painter2
            (make-vect 0 0.5)
            (make-vect 1 0.5)
            (make-vect 0 1))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

((below test-painter diamond->painter) ort-frame)

; as beside with rotations

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))

((below test-painter diamond->painter) ort-frame)

; excercise 2.52
; a.
; I will not copy all 'wave' but rather combine existing one with new element

(define old-wave wave)
(define wave
  (combine-painters
    (list
      old-wave
      (polygonal->painter
        (list (make-vect 0.4 0.7)
              (make-vect 0.5 0.6)
              (make-vect 0.6 0.7))))))

(wave ort-frame)

; b. t

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner)))))

; from 2.44

(define right-split (split beside below))
(define up-split (split below beside))

((right-split diamond->painter 3) ort-frame)
((up-split diamond->painter 3) ort-frame)

; c. original square-limit:

; let's indeed make big mr. Rogers to be at corner of result square

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    ((square-of-four
      rotate270
      rotate180
      identity
      rotate90) quarter)))

(define (identity painter)
  painter)

((square-limit wave 2) ort-frame)
