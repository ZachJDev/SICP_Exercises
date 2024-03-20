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
        the-empty-stream
        (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred (stream-car stream)) 
                (cons-stream (stream-car stream)
                    (stream-filter pred (stream-cdr stream))))
          (else (stream-filter pred (stream-cdr stream)))))


(define (display-line x)
 (newline)
 (display x))



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

; EXERCISE 3.51

(define (show x)
    (display-line x)
    x)
; (define x (stream-map show (stream-enumerate-interval 0 10)))
; evaluates to <#proc::x> or something, will also display 0.
; (stream-ref x 5)
; displays 1 -> 4, evaluates to 4
; (stream-ref x 7)
; displays 5 -> 6, evaluates to 6.
; A thought before testing these -- my reasoning around what the third call displays
; is, that while the value of each forced promise is cached, the side effects, i.e., (display x)
; are probably not repeated.
; Testing this, I was mostly right, except for an off-by-one error: (steram-ref x 5) actually displayed
; 1 2 3 4 5, and for 6 displayed 6 and 7.
; this is because (stream-ref) is 0 indexed and I was thinking it was 1-indexed, which was silly.

; EXERCISE 3.52

(define sum 0) ; sum == 0
 (define (accum x)
    (set! sum (+ x sum))
    sum) ; sum == 0
(define seq (stream-map accum (stream-enumerate-interval 1 20))) ; sum == 1
(define y (stream-filter even? seq)) ; sum == 1 XXX actually 6, see below
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq)) ; sum == 10

(stream-ref y 7) ; sum == 136, displays 136

; (display-stream z) ; sum == 210, displays 10,15,45,55,105,120,190,210

; Yes, if (delay <exp>) did not memoize, and we instead were calling (accum) each time through the stream,
; sum would be much larger, and we would get totally different results for subsequent calls to stream-ref
; and display-stream.  

; notes to the above -- stream-filter will go through the stream until it's predicate returns true
; before we get any calls to (cons-stream) -- meaning that sum will get to six, the first even number
; in the first stream-filter call. STREAM-FILTER DOES NOT FITLER THE ENTIRE STREAM, which is what I had wrong at first.
; it only evaluates until it reaches an item that returns true.


