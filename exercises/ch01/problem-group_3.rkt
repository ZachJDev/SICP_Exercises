#lang sicp
; EXERCISE 1.9

; The first of the two definitions is recursive and the second is iterative. The first is recursive because
; the calls to (inc n) are deferred until (+ a b) returns a single value, which takes a calls to happen.
; The second is iterative because nothing is deferred waiting for a value -- what's recursively called
; is an other function call that does not rely on what was called in previous computation frames.

; EXERCISE 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A  (- x 1)
                  (A x (- y 1))))))
; (A 1 10)
; (A 2 4)

(define (f n) (A 0 n)) ; 2n
(define (g n) (A 1 n)) ; 2^n
(define (h n) (A 2 n)) ; 2^(2_1)^(2_2)...^(2_(n-1))

(define (^ a b)
  (if (= b 0)
      1
      (* a (^ a (- b 1)))))

; (f 5)
; (g 5)
; (h 1)
(= (h 5) (^ 2 (^ 16 4)))
(= (h 4) (^ 2 16))
(= (h 3) (^ 2 4))
(= (h 2) (^ 2 2))
(= (h 1) (^ 2 1))
(^ 2 (^ 2 (^ 2 2)))

