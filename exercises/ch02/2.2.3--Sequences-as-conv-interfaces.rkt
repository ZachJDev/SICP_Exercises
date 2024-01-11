#lang sicp

; from https://stackoverflow.com/questions/13791047/scheme-prime-numbers#13793084
(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

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


; EXERCISE 2.37

; For my own edification -- even though accumulate and fold-left look very similar
; They are different because accumulate is recursive, and will await the result of the recursive call 
; (which is itself the result of combining the elements to the right). And it will combine `initial` with the final element.
; Because the function below iterates, we'll combine the elements from left to right, combining initial with
; First element.
; Note that fold-left could be written recursively and fold-right iteratively, I just think it's interesting
; to note how similar the two functions look when written out.

(define (fold-left op initial seq)
    (define (iter result rest)
        (if (null? rest) 
            result
            (iter (op result (car rest))
                (cdr rest))))
    (iter initial seq))

; (fold-right / 1 (list 1 2 3)) == 1 / ( 2 / 3) = 3/2
; (fold-left / 1 (list 1 2 3)) == (1/ 2) / 3 = 1/6
; (fold-right list nil (list 1 2 3)) == (list 1 (list 2 (list 3 nil))) == (1 2 3) ; This is incorrect, I was treating `list` like `cons`.
; (fold-left list nil (list 1 2 3)) == (list (list (list nil 1) 2) 3) == (((() 1) 2) 3)

; (display (accumulate append '() (map (lambda(x) 
;                 (map (lambda (y)
;                     (list x y))
;                     (enumerate-interval 1 (- x 1))))
;                 (enumerate-interval 1 3))))


; Nested loops like this will probably always? be nested map/filter/etc. functions

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))


(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(display (map make-pair-sum (filter prime-sum? (flatmap (lambda (x) (map (lambda (y)
                     (list x y))
                     (enumerate-interval 1 (- x 1)))) (enumerate-interval 1 3)))))

