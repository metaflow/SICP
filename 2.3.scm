(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; excercise 2.53

(list 'a 'b 'c) ; is (a b c)
(list (list 'george)) ; is ((george))
(cdr '((x1 x2) (y1 y2))) ; is ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; is (y1 y2)
(pair? (car '(a short list))) ; false
(memq 'red '((red shoes) (blue socks))) ; false
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

; excercise 2.54

(define (my-equal? a b)
  (cond ((and (null? a) (null? b)) true)
        ((or (null? a) (null? b)) false)
        ((and (pair? a) (pair? b))
          (and (my-equal? (car a)
                          (car b))
               (my-equal? (cdr a)
                          (cdr b))))
        ((or (pair? a) (pair? b)) false)
        (else (eq? a b))))

(my-equal? 1 2)
(my-equal? '() '())
(my-equal? '(1) '(1))
(my-equal? '() '(1))
(my-equal? '(1) '())
(my-equal? '(1 '(2)) '(1 '(2)))
(my-equal? '(1 '(2)) '('(2) 1))

; excercise 2.55

(car ''abracadabra)

; 'abracadabra is interpreted as (quote abracadabra)
(car '(qoute abracadabra))
; adding next qoute gives us a (list 'qoute 'abracadabra)
(car (list 'qoute 'abracadabra))
; first element of which is 'qoute' symbol

; ---- Symbolic Differentiation ----

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((product? exp)
          (make-sum (make-product
                      (multiplier exp)
                      (deriv (multiplicand exp) var))
                    (make-product
                      (multiplicand exp)
                      (deriv (multiplier exp) var))))
      (else (error "unknown expression type -- DERIV " exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp v)
  (and (number? exp)
       (= exp v)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2)))
        )


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; excercise 2.56

; first - let redefine deriv to accept exponents

(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-sum (make-product
                      (multiplier exp)
                      (deriv (multiplicand exp) var))
                    (make-product
                      (multiplicand exp)
                      (deriv (multiplier exp) var))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp) (deriv-sum exp var))
        ((product? exp) (deriv-product exp var))
        ((exponentiation? exp) (deriv-exp exp var))
      (else (error "unknown expression type -- DERIV " exp))))

(define (deriv-exp exp var)
  (make-product
    (make-product (exponent exp)
                  (make-exponentiation
                    (base exp)
                    (make-sum (exponent exp) -1)))
    (deriv (base exp) var)))

(define (make-exponentiation base exponent)
  (list '^ base exponent))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '^)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

; version of make-exponentiation with simplifications

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '^ base exponent))))

(deriv '(^ x 2) 'x)
(deriv '(^ (* x y) 3) 'x)

; excercise 2.57
; we need to redefine augend and multiplicand to solve this

(define (augend sum)
  (let ((rest (cddr sum)))
    (cond ((null? rest) 0)
          ((null? (cdr rest)) (car rest))
          (else (cons '+ rest)))))

(augend '(+ x))
(augend '(+ x y))
(augend '(+ x y z))

(define (multiplicand p)
  (let ((rest (cddr p)))
    (cond ((null? rest) 1)
          ((null? (cdr rest)) (car rest))
          (else (cons '* rest)))))

(deriv '(* x y (+ x 3)) 'x)

; excercise 2.58
; a
; we need to redefine our sum and product operators to accept
; list in form (a <operator> b)
; note that we don't need sum and product notations with arbitrary number of arguments


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(sum? (make-sum 'x 'y))
(addend (make-sum 'x 'y))
(augend (make-sum 'x 'y))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

(make-product 'x 'y)
(product? (make-product 'x 'y))
(multiplier (make-product 'x 'y))
(multiplicand (make-product 'x 'y))

(deriv '(x + (3 * (x + (y + 2)))) 'x)

; b
; to implement this we need a convinient way to extract parts of list
; before and after specific symbol (like memq does)

(define (before-symbol item x)
  (define (iter rest result)
    (cond ((null? rest) result)
          ((eq? item (car rest)) result)
          (else (iter (cdr rest)
                      (append result (list (car rest)))))))
  (iter x '()))

(before-symbol '+ '(x * e + y))
(before-symbol '* '(x * e + y))

(define (after-symbol item x)
  (cond ((null? x) '())
        ((eq? item (car x)) (cdr x))
        (else (after-symbol item (cdr x)))))

(after-symbol '+ '(x * e + y))
(after-symbol '* '(x * e + y))

(define (contains-symbol? item x)
  (not (null? (after-symbol item x))))

(contains-symbol? '* '(x * e + y))
(contains-symbol? '* '(x + e + y))

; now it's time to redefine sum and production procedures once again

(define (sum? x)
  (contains-symbol? '+ x))

(define (addend s)
  (before-symbol '+ s))

(define (augend s)
  (after-symbol '+ s))

(sum? (make-sum 'x 'y))
(addend (make-sum 'x 'y))
(augend (make-sum 'x 'y))
(addend '(x + y * 2 + z))
(augend '(x + y * 2 + z))

; note that we get (x) instead of just x. Let's simplify this

(define (simplify-single-element s)
  (if (or (null? s) (not (null? (cdr s))))
      s
      (car s)))

(simplify-single-element '())
(simplify-single-element '(x))
(simplify-single-element '(x + 3))

(define (addend s)
  (simplify-single-element (before-symbol '+ s)))

(define (augend s)
  (simplify-single-element (after-symbol '+ s)))

(addend '(x + y * 2 + z))
(augend '(x + y * 2 + z))

; even if sum? is applied in deriv in correct order it's better to enforce
; this here too

(define (product? x)
  (if (sum? x)
      false
      (contains-symbol? '* x)))

(define (multiplier s)
  (simplify-single-element (before-symbol '* s)))

(define (multiplicand s)
  (simplify-single-element (after-symbol '* s)))

(product? '(x + y * 2 + z))
(product? '(x * y * 2))
(multiplier '(x * y * 2))
(multiplicand '(x * y * 2))

(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + 3 * (x + y * x * x + 2)) 'x)
