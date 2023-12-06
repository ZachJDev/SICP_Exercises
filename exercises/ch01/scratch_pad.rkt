#lang sicp
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; (sqrt 25)

(define (factorial n)
  (if (<= n 1)
      n
      (* n (factorial (- n 1)))))

(factorial 6)

(define (factorial-iter n)
  (define (fact-iter acc current)
    (if (> current n)
        acc
        (fact-iter (* acc current)(+ 1 current))))
  (fact-iter 1 1))

(factorial-iter 6)

