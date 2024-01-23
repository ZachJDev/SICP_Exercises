#lang sicp
(#%require sicp-pict)
(define wave-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0.4) ;;; leg triangle
                  (make-vect 0.6 0))
    (make-segment (make-vect 0.5 0.4)
                  (make-vect 0.4 0))
    (make-segment (make-vect 0.3 0)
                  (make-vect 0.35 0.4))
    (make-segment (make-vect 0.35 0.4)
                  (make-vect 0.3 0.7))
    (make-segment (make-vect 0.3 0.7)
                  (make-vect 0.2 0.6))
    (make-segment (make-vect 0.2 0.6)
                  (make-vect 0 0.8))
    (make-segment (make-vect 0 0.9)
                  (make-vect 0.2 0.7))
    (make-segment (make-vect 0.2 0.7)
                  (make-vect 0.3 0.75))
    (make-segment (make-vect 0.3 0.75)
                  (make-vect 0.4 0.75))
    (make-segment (make-vect 0.4 0.75)
                  (make-vect 0.35 0.9))
    (make-segment (make-vect 0.35 0.9)
                  (make-vect 0.4 1))
    (make-segment (make-vect 0.5 1)
                  (make-vect 0.55 0.9))
    (make-segment (make-vect 0.55 0.9)
                  (make-vect 0.5 0.75))
    (make-segment (make-vect 0.5 0.75)
                  (make-vect 0.6 0.75))
    (make-segment (make-vect 0.6 0.75)
                  (make-vect 1 0.45))
    (make-segment (make-vect 1 0.3)
                  (make-vect 0.6 0.5))
    (make-segment (make-vect 0.6 0.5)
                  (make-vect 0.7 0)))))

(define wave wave-painter)

(define wave2 (beside wave (flip-vert wave)))
(define wave3 (below wave2 wave2))
(define (flipped-pairs painter)
  (let  ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define (right-split painter n)
  (if (= 0 n)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
; Exercise 2.44
(define (up-split painter n)
  (if (= 0 n)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (my-corner-split painter n)
  (if (= n 0)
      painter
  (let ((half (right-split painter 1)))
    (my-corner-split (below half (flip-horiz half)) (- n 1)))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((top-half (beside(flip-horiz quarter)  quarter )))
      (below (flip-vert top-half) top-half))))

; (paint(square-limit wave 1))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; EXERCISE 2.45
(define (split d1 d2)
  (define (split-helper painter n)
    (if (= n 0)
        painter
         (d1 painter (split-helper (d2 painter painter) (- n 1)))))
  (lambda (painter n)
    (split-helper painter n)))

; EXERCISE 2.46

(define (make-vect2 x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (x-vect fn)
 (lambda (v1 v2)
   (make-vect2 (fn (xcor-vect v1)
                 (xcor-vect v2))
              (fn (ycor-vect v1)
                 (ycor-vect v2)))))
(define add-vect (x-vect +))
(define sub-vect (x-vect -))
(define (scale-vect vect s)
  (make-vect (* (xcor-vect vect) s)
             (* (ycor-vect vect) s)))
        
; Exercise 2.47

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin1 frame)
  (car frame))
(define (edges1 frame)
  (cdr frame))
(define (edge1-1 frame)
  (car (edges1 frame)))
(define (edge2-1 frame)
  (cadr (edges1 frame)))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin2 frame)
  (car frame))
(define (edges2 frame)
  (cdr frame))
(define (edge1-2 frame)
  (car (edges2 frame)))
(define (edge2-2 frame)
  (cdr (edges2 frame)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
    (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                          (edge1-frame frame))
             (scale-vect (ycor-vect v)
                         (edge2-frame frame))))))