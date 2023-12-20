#lang sicp


(define (make-interval a b) (cons a b))

; EXERCISE 2.7

(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (lower-bound y) (lower-bound x)))
        (p4 (* (lower-bound y) (upper-bound x))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (spans-zero? interval)
  (and (<= (lower-bound interval) 0) (>= (upper-bound interval) 0)))

(define (divide-interval  x y)
  (if (spans-zero? y)
      (error "cannot divide by an interval that spans zero")
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

; EXERCISE 2.8

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (upper-bound y))
                 (- (lower-bound x) (lower-bound y))))

; Center Width Implementation

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c perc)
  (let ((tolerance (* c (* .01 perc))))
    (make-interval (- c tolerance) (+ c tolerance))))

(define (percent i)
   (* 100 (- 1 (/ (lower-bound i) (center i)))))