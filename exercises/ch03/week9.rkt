#lang sicp


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
         (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

; EXERCISE 3.21

(define (print-queue queue)
  (front-ptr queue))


; basic 1-D table
(define test-table '(*table* (a . 1) (b . 2) (c . 3)))

;(define (lookup key table)
;  (let ((record (assoc key (cdr table))))
;    (if record
;        (cdr record)
;        false)))



(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))




;(define (insert! key value table)
;  (let ((record (assoc key (cdr table))))
;    (if record
;        (set-cdr! record value)
;        (set-cdr! table
;                  (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

; 2-D table

(define (lookup key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                       (cons (cons key2 value) (cdr subtable))))      
        (set-cdr! table
                  (cons (list key1 (cons key2 value))
                        (cdr table)))))))


; "local" table

(define (make-table-loc)
  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
  (let ((subtable (assoc key1 (cdr local-table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))
  (define (insert! key1 key2 value)
  (let ((subtable (assoc key1 (cdr local-table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                       (cons (cons key2 value) (cdr subtable))))      
        (set-cdr! local-table
                  (cons (list key1 (cons key2 value))
                        (cdr local-table))))))
    'ok)
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          ((eq? m 'local-table) local-table)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch))

; EXERCISE 3.25

(define (table-lookup key records)
  (cond ((null? records) false)
        ((equal? key (car records)) (car records))
        (else (assoc key (cdr records)))))

(define (deep-assoc keys records)
  (let ((subtable (assoc (car keys) (cdr records))))
    (if (or (not subtable) (null? (cdr keys)))
        subtable
        (deep-assoc (cdr keys) subtable))))

(define (make-table-loc-2)
  (let ((local-table (list '*table*)))
    
  (define (lookup keys)
    (let ((record (deep-assoc keys local-table)))
          (if record
              (cdr record)
              false)))
    
    (define (insert-subtables-helper! keys parent)
       (if (null? keys)
           parent
           (let ((existing-table (table-lookup (car keys) parent)))
             (if existing-table
                 (insert-subtables-helper! (cdr keys) existing-table)
                 (begin
                   (set-cdr! parent (cons (list (car keys)) (cdr parent)))
                   (insert-subtables-helper! (cdr keys) (car (cdr parent))))))))
             
    (define (insert-subtables! keys)
     (insert-subtables-helper! keys local-table))
             
  (define (insert! keys value)
    (if (null? (cdr keys))
        #f
        (let ((key (insert-subtables! keys)))
               (set-cdr! key value)))
    'ok)
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch))

; Exercise 3.27

; (memoize fib) would not work because the recursive calls to (fib (- n 1)) etc. would not benefit from the memoziation table.
