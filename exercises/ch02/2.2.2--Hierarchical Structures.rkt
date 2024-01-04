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
; (balanced? (make-mobile branch1 branch1))
; (balanced? (make-mobile branch1 (make-branch 1 (make-mobile branch1 branch1))))

;I'm having a realization that a cdr of a well-formed list will never be an element of the list! I feel like that's so obvious, but not something I grasped.
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
  tree))
; EXERCISE 2.30 & 2.31
(define (map-tree tree func)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (func tree))
        (else (cons (map-tree (car tree) func)
                    (map-tree (cdr tree) func)))))
(define (square-tree tree)
  (map-tree tree (lambda (x) (* x x))))
; (square-tree '(1 2 (3 4) ((5 6)) (((7 8))) (9) 10 11 12))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (* sub-tree sub-tree))) tree))

;(display (square-tree2 '(1 2 (3 4) ((5 6)) (((7 8))) (9) 10 11 12)))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (x)
                       (cons (car s) x))
                      rest)))))

;(subsets '(1 2 3 4))
; The key to understanding this is to note that `rest` will always
; be the subsets of s without the first element. With that knowledge
; We can see that taking those subsets and adding the first element to them
; will generate the subsets of s. The `append` call takes all of the subsets without
; the first element and appends a list of each of those lists with the first element attached.
; all of the subsets are determined before any appending or mapping happens: we get to the
; Bottom of the callstack, returning the empty listas `rest`, at which point `append` the empty set
; to the map, which just conses the last element to the empty set, all of which will return up a level
; to where s is the final 2 elements of the set, at which point we append everything from the previous
; level to a map over those elements where we cons 3 to them.  at each recurse, we double the number
; of elements, because we have rest=<prevous elements> + map_call=<cons new_elements previous_elements>.
; The map call returns a list that is the same size as rest.