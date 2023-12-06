#lang sicp
; EXERCISE 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (p) (p))
(if (= 2 2) 0 (p))
; (new-if (= 2 2) 0 (p)) -- Program enters an infinite recursion here.

; This new-if doesn't work because of the order of operations -- scheme will attempt to evaluate
; both the then- and  else-clauses before getting to the cond expression. So this formulation is not
; acceptable as a check for a halting case in a recursive function because recursive call will always happen.

; EXERCISE 1.7

; The book's implementation will not be effective for finding small square roots because the precision needed may be
; smaller than the constant number that determines if a guess is good enough. Larger numbers will take a very long time to
; get to a point where that constant number is meaningful -- it is too precise. the redesigned version is much quicker for
; large numbers and much more precise for small numbers.


; EXERCISE 1.8

; Implemented in Newton_method.scm. Given the general approximation algorithm developed previously, I can plug
; in the cube root approximation with out much hassle.

