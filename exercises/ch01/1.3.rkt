#lang sicp
; Messing with lambda
(define (sum-bak func from to)
  (if (> from to)
      0
      (+ (func from) (sum-bak func (+ 1 from) to))))

(define (square x) (* x x))

; (sum (lambda (x) (* x x)) 1 3)

(define (sum2 from to)
  (lambda (func)
      (if (> from to)
      0
      (+ (func from) (sum2 func (+ 1 from) to)))))

; ((sum2 1 3) square)
; ((sum2 1 ((sum2 1 3) square)) (lambda (x) x))
; (sum (lambda (x) x) 1 14)

; 1.3.1 Procs as Arguments

(define (sum f a b next)
  (if (> a b)
      0
      (+ (f a)
         (sum f (next a) b next))))

(define (inc x) (+ 1 x))
(define (identity x) x)

;(sum (lambda (x) (* x x x)) 1 10 inc)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a b pi-next))

; (* 8 (pi-sum 1 10000))

(define (def-integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) b add-dx)
     dx))
;(def-integral (lambda (x) (* x x x)) 0 1 0.00001)

; EXERCISE 1.29

(define (simps-rule f a b n)
  (define h (/ (- b a) n))
  (define (func x)
    (if (even? x)
        (* 2 (f (+ a (* x h))))
        (* 4 (f (+ a (* x h))))))
  (* (/ h 3) (+ (f a) (sum func 0 n inc))))

;(simps-rule (lambda (x) (* x x x )) 0 1 1000)

; Compared to the procedure above, this is much more accurate
; when n=100, it returns 19/75 (.25333...), when n=1000 751/3000 (.250333...)

; EXERCISE 1.30

(define (sum-it f a b next)
  (define (sum-helper cur total)
  (if (> cur b)
      total
      (sum-helper (next cur) (+ total (f cur)))))
  (sum-helper a 0))

;(sum-it (lambda (x) (* x x x)) 1 10 inc)

; EXERCISE 1.31

(define (product f a b next)
  (if (> a b)
      1
      (* (f a) (product f (next a) b next))))

(define (product-iter f a b next)
  (define (iter acc cur)
    (if (> cur  b)
        acc
        (iter (* acc (f cur)) (next cur))))
  (iter 1 a))

(define (factorial x)
  (product identity 1 x inc))

; (factorial 5)

(define (double-inc x) (+ x 2))
;(define (double x) ( * 2 x))

(define (pi-comp x)
(*  (/ (* 2 (product square 4 x double-inc))
     (* (+ 1 x) (product square 3 x double-inc)))
    4.0))

(pi-comp 9000)

; EXERCISE 1.32

(define (accumulate combiner null-value f a b next)
  (if (> a b)
      null-value
      (combiner (f a) (accumulate combiner null-value f (next a) b next) )))

(define (accumulate-iter combiner null-value f a b next)
  (define (iter acc cur)
    (if (> cur b)
        acc
        (iter (combiner acc (f cur)) (next cur))))
  (iter null-value a))

(define (sum-acc f a b next)
  (accumulate + 0 f a b next))

(define (sum-acc-iter f a b next)
  (accumulate-iter + 0 f a b next))

; (sum-acc-iter identity 1 5 inc)

; EXERCISE 1.33

(define (filtered-accumulate combiner null-value f a b next predicate?)
  (define (recurs-helper x)
    (cond ((> x b) null-value)
          ((predicate? x) (combiner (f x) (recurs-helper (next x))))
          (else (combiner null-value (recurs-helper (next x))))))
  (recurs-helper a))

; (filtered-accumulate + 0 square 1 6 inc prime?)

(define (sum-of-relative-primes-to x)
  (define (is-relative-prime y) (= 1 (gcd x y)))
  (filtered-accumulate + 0 identity 1 x inc is-relative-prime))

(sum-of-relative-primes-to 11)
(sum-acc identity 1 11 inc)

; EXERCISE 1.34

; My guess is that we'd get an error related to the fact that 2 is not a procedure, and we can't call it.

;(define (f g)
 ; (g 2))

;(f f) I was right!

; let vs lambda

(define (times-y x)
  (let ((y (+ 15 3)))
  (* x y)))

(define (times-y2 x)
   ((lambda (y)
    (* x y))
    (+ 15 3)))

(times-y 4)
(times-y2 4)

; 1.3.3 Procedures as General Methods

; Half-interval Method

(define (average a b) (/ (+ a b) 2))
(define (close-enough? a b) (< (abs (- a b)) 0.00001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "Values are not of opposite signs:" a b)))))

; (half-interval-method sin 2.0 4.0)

; (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)

; Fixed Points

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
    (if (close-enough? next guess)
        guess
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; NEWTON's METHOD

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/  (- (g (+ x dx)) (g x))
        dx)))

(define (cube x) (* x x x))

; ((deriv cube) 5)

; ((deriv (deriv cube)) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; EXERCISE 1.40

(define (cubic a b c)
  (lambda (x)
  (+ (* x x x) (* a (* x x)) (* b x) c)))

; EXERCISE 1.41

(define (double f)
  (lambda (x) (f (f x))))

; EXERCISE 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

; EXERCISE 1.43

(define (repeated f times)
  (if (< times 1)
      (lambda (x) x)
      (compose f (repeated f (- times 1)))))

((repeated square 2) 5)

; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter x)
      (if (good-enough? x)
          x
          (iter (improve x))))
    (iter guess)))

; ((iterative-improve (lambda (x) (= x 1)) (lambda (x) (- x 1))) 5)

(define (sqrt2 x)
  ((iterative-improve (lambda (guess)
                       (< (abs (- (square guess) x)) 0.00001))
                     (lambda (guess)
                       (average guess (/ x guess)))) (* x 1.0))) ; Force decimal number
(define (fixed-point-iter-improve f first-guess)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (f guess) guess)) 0.00001))
                      (lambda (guess) (f guess))) first-guess))

(fixed-point cos 1.0)
(fixed-point-iter-improve cos 1.0)