#lang sicp

(define (count-leaves list)
  (cond  ((null? list) 0)
         ((list? list) (+
                       (count-leaves (car list))
                       (count-leaves (cdr list))))
        (else 1)))
;(define x (cons (list 1 2) (list  3 4)))
;(define x2 (list x x))
;(count-leaves x2)

; EXERCISE 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight structure)
  (cond ((number? structure) structure)
        (else
        (+ (total-weight (branch-structure (left-branch structure)))
           (total-weight (branch-structure (right-branch structure)))))))

(define (branch-torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (if (number? (branch-structure mobile))
      #t
   (and (= (branch-torque (left-branch mobile))
           (branch-torque (right-branch mobile)))
       (balanced? (left-branch mobile))
       (balanced? (right-branch mobile)))))
; I can see a strategy for an iterative solution that starts with the total weight, then subtracts weight as it moves down the tree.

(define branch1 (make-branch 1 2))
(balanced? (make-mobile branch1 branch1))
(balanced? (make-mobile branch1 (make-branch 1 (make-mobile branch1 branch1))))      