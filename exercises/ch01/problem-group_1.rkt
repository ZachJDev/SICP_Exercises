#lang sicp
; EXERCISE 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; EXERCISE 1.3
; Define a procedure that takes three numbers as arguments and returns the sum
; of the squares of the two larger numbers

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

(define (sum-of-squares-of-two-largest a b c)
  (cond
    ((and (<= a b) (<= a c)) (sum-of-squares b c))
    ((and (<= b a) (<= b c)) (sum-of-squares a c))
    ((and (<= c a) (<= c b)) (sum-of-squares a b))))

; EXERCISE 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Because we're not evaluting the + or - (because it's not the first item of the list), it's just the result of the compound expression.
; But because that result becomes the first item of the list, it's then treated as a procedure call with a and b.

; EXERCISE 1.5

; With applicative-order evaluation, we will evaluate (p) as soon as possible, and will be stuck in an inifite loop. With normal order, we would not evaluate (p) until it is needed
; which it never will be, because the (if) statement would never evaluate to false.

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;(test 0 (p))
