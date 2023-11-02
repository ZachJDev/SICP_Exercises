#lang sicp
(define (sum func from to)
  (if (> from to)
      0
      (+ (func from) (sum func (+ 1 from) to))))

(define (square x) (* x x))

(sum (lambda (x) (* x x)) 1 3)

(define (sum2 from to)
  (lambda (func)
      (if (> from to)
      0
      (+ (func from) (sum func (+ 1 from) to)))))

((sum2 1 3) square)
((sum2 1 ((sum2 1 3) square)) (lambda (x) x))
(sum (lambda (x) x) 1 14)

