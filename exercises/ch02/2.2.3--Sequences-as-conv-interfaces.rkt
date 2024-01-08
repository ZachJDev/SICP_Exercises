#lang sicp

(define (square x) (* x x))

(define (filter predicate seq)
    (cond ((null? seq) '())
          ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
          (else 
            (filter predicate (cdr seq)))))

(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq) (accumulate op initial (cdr seq)))
        ))

(define (enumerate-interval low high)
    (if (> low high) '()
     (cons low (enumerate-interval (+ 1 low) high))))

(define (enumerate-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(define (sum-odd-squares tree)
   (accumulate + 0
    (map square (filter odd? (enumerate-tree tree)))))

; EXERCISE 2.36
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

; EXERCISE 2.37
(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (mi) (dot-product v mi)) m))
 
(define (transpose mat)
    (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))
    
; This is the decomposed version of the above. What I had originally been trying to do
; before realizing I just needed to multiply the cols matrix by the vectors of m.    

(define (MM m n)
(let ((cols (transpose n)))
    (map (lambda (x)
        (map (lambda (y)
                (accumulate + 0 (map * y x))) cols)) m)))
