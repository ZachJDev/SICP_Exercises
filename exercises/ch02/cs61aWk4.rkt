#lang simply-scheme

(define (substitute list old new)
  (cond ((null? list) '())
        ((list? (car list))
         (cons (substitute (car list) old new) (substitute (cdr list) old new)))       
        ((equal? (car list) old)
         (cons new (substitute (cdr list) old new)))
        (else (cons (car list) (substitute (cdr list) old new)))))

(substitute '((lead guitar) ((bass guitar)) (rhythm guitar) drums) 'guitar 'axe)

(define (substitute2 sent old-list new-list)
  (define (find-index list word idx)
    (cond ((null? list) -1)
          ((equal? (car list) word) idx)
          (else (find-index (cdr list) word (+ 1 idx)))))
  
  (define (get-nth-word list word idx)
    (cond ((equal? -1 idx) word)
          ((equal? 0 idx) (car list))
          (else (get-nth-word (cdr list) word (- idx  1)))))
  
  (define (sub-helper list)
    (cond ((null? list) '())
        ((list? (car list))
         (cons (sub-helper (car list))
               (sub-helper (cdr list))))       
        (else
         (cons (get-nth-word new-list
                             (car list)
                             (find-index old-list (car list) 0))
               (sub-helper (cdr list))))))
  
   (sub-helper sent))

(substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
             '(1 2 3 4) '(one two three four))

  (define (get-nth-word list word idx)
    (cond ((equal? -1 idx) word)
          ((equal? 0 idx) (car list))
          (else (get-nth-word (cdr list) word (- idx  1)))))

(define (find-index list word idx)
    (cond ((null? list) -1)
          ((equal? (car list) word) idx)
          (else (find-index (cdr list) word (+ 1 idx)))))