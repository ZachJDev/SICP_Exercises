#lang sicp

(define (sum-of-factors n)
  (define (iter acc i)
    (cond ((> (* i 2) n) acc)
          ((= 0 (remainder n i))
           (iter (+ acc i) (+ i 1)))
          (else
           (iter acc (+ i 1)))))
  (iter 1 2))

(define (is-perfect? n)
  (= n (sum-of-factors n)))

(define (next-perf n)
  (if (is-perfect? n)
      n
      (next-perf (+ n 1))))

(next-perf 500)
    