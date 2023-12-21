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

; EXERCISE 2.22
; This first  example is a good way to show the technique of building a list, then reversing it.
(define (square x) (* x x ))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
    answer
    (iter (cdr things)
          (cons (square (car things))
                answer)))) ; The first element will be the first thing added to answer. and the next will be cons'd to that, all the way to the front.
  (iter items nil))

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer ; The initial answer is nil, so that will be added to the second-to-last element of the list. Additionally,  our list will be mal-formed, as it won't end with a nil.
                    (square (car things))))))
  (iter items nil))

;The right way to define the recursive iter call in the first version would be:
;(iter (cdr things) (append answer (list (square (car things))))

;EXERCISE 2.23

(define (for-each proc list)
  (if (null? list)
      #t
      (and (for-each proc (cdr list)) (proc (car list)))))

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4 5))