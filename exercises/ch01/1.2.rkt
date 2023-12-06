#lang sicp

; EXERCISE 1.16

(define (square x) (* x x))

(define (exp-iter x exp)
(define (iter b n a)
  (cond ((= n 0) a)
        ((even? n) (iter (square b) (/ n 2) a))
        (else (iter b (- n 1) (* a b)))))
 (iter x exp 1))

;(exp-iter 3 5)

