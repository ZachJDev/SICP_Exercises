#lang simply-scheme
; EXERCISE 1.11
(define (f-recurse n)
  (if (< n 3)
      n
      (+ (f-recurse (- n 1)) (f-recurse (- n 3)) (f-recurse (- n 2)))))

(define (f-iterative n)
 ; We 'seed' f-iter as if it was starting at 3 --
 ; the first number where subsequent calls would all be above 0.
 ; Lower values are handled with the if statement prior to
 ; the f-iter call.
(define (f-iter total cur prev sec-prev third-prev)
  (if (> cur n)
      total
     (f-iter (+ prev sec-prev third-prev)
             (+ cur 1)
             (+ prev sec-prev third-prev)
             prev
             sec-prev)))
  (if (< n 3)
      n
  (f-iter 3 4 3 2 1)))


(define (f-both n)
  (list (f-iterative n) (f-recurse n)))

(f-iterative 10)

; EXERCISE 1.12

(define (pascal row pos)
 (cond ((= pos 0) 0)
       ((= pos 1) 1)
       ((= row pos) 1)
       (else (+
              (pascal (- row 1) pos)
              (pascal (- row 1) (- pos 1))))))

(pascal 2 2)

(define (pascal-row row-num)
  (define (helper current)
    (if (> current row-num)
        '()
        (cons (pascal row-num current) (helper (+ current 1)))))
  (helper 1))

(pascal-row 10)


; BETTER IMPLEMENTATION (cs61a)

(define (nth num list)
  (define (helper cur rest)
    (if (= cur num)
        (first rest)
        (helper (+ 1 cur) (bf rest))))
  (helper 0 list))

(define (new-pascal-row row-num)
  (define (iter in out)
    (if (empty? (bf in))
        out
        (iter (bf in) (se (+ (first in) (first (bf in))) out)) ))
  (define (next-row old-row num)
    (if (= num 0)
        old-row
        (next-row (se 1 (iter old-row '(1))) (- num 1)) ))
  (next-row '(1) row-num))

(define (new-pascal row col)
  (nth col (new-pascal-row row)))

(new-pascal 20 8)