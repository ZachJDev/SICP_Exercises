#lang sicp

(define (square x)
  (* x x))

(define (cube x)
  (* x (square x)))

(define test  1448982799687472)
; A procedure to calculate the square root of x, given some initial guess.

(define (my-average x y)
  (/ (+ x y) 2.0))

(define (sqrt x guess)
  (my-average guess (/ x guess)))

; Exercise 1.8 -- cube roots.
(define (cbrt x guess)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3.0))

  (define (approx func x delta)
    (define (iter func x prev)
      (if (< (abs (- (func x prev) prev)) delta)
          prev
          (iter func x (func x prev))))
    (iter func x 1.0))

(define (approx2 func x delta)
  (define (iter func x prev)
    (if (< (abs (- 1 (/ prev (func x prev)))) delta)
        prev
        (iter func x (func x prev))))
  (iter func x 1))

 (approx sqrt test .0000000001)
 (approx2 sqrt test .0000000001)

(approx cbrt 190 .00000000001)


;; BOOK'S WORKING OUT:


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .0000001))
    

(define (sqrt2 x)
  (sqrt-iter 1.0 x))

; (sqrt2 test)

