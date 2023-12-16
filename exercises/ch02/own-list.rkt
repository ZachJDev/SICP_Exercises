#lang sicp

(define (own-list . args)
  (if (null? (car args))
      '()
      (cons (car args) (own-list (cdr args)))))