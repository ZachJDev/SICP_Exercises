#lang sicp


(define (my-reverse lst)
  (if (null? lst)
      '()
      (append (my-reverse (cdr lst)) (list (car lst)))))

(my-reverse '(1 2 3 4 5))

(define (list-ref items n)
  (if (= 0 n)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

; EXERCISE 2.17

(define (last-pair items)
  (if (null? (cdr (cdr items)))
      (cdr items)
      (last-pair (cdr items))))

; EXERCISE 2.20

(define (same-parity n . rest)
  (define parity-check (if (even? n) even? odd?))
  (define (helper list)
    (cond ((null? list) '())
          ((parity-check (car list))
           (cons (car list) (helper (cdr list))))
          (else (helper (cdr list)))))
  (helper rest))

; Mapping Over Lists