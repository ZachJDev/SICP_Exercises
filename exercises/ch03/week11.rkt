#lang sicp
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
    (if (= n 0)
       (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
                (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each (lambda (x) (newline) (display x)) s))

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream)
        (cons-stream low (stream-enumerate-interval (+ low 1) high)))

(define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred (stream-car stream)) 
                (cons-stream (stream-car stream)
                    (stream-filter pred (stream-cdr stream))))
          (else (stream-filter pred (stream-cdr stream)))))



; TESTING FOR PRIMES

(define (square x) ( * x x))
(define (smallest-divisor n)
 (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))


; EXERCISE 3.50

; argstreams is a list of streams. This proc returns a single stream
(define (stream-map2 proc . argstreams)
    (if (null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map2
                (cons proc (map stream-cdr argstreams))))))