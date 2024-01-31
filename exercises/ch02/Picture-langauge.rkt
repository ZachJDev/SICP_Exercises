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
                  (make-vect 0.7 0))
    (make-segment (make-vect 0.4 .83)
                  (make-vect .45 .80))
    (make-segment (make-vect 0.5 .83)
                  (make-vect .45 .80)))))

(define wave wave-painter)

(paint wave)

(define (draw-line segment1 segment2)
  (display segment1)
  (display segment2))

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
        (let ((top-left up)
              (bottom-right right)
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
    (let ((top (beside  (br painter)  (bl painter)))
          (bottom (beside  (tr painter)  (tl painter) )))
      (below bottom top))))

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit wave 3))
(paint (square-limit2 wave 3))

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
(define (scale-vect s vect)
  (make-vect2 (* (xcor-vect vect) s)
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
  (cdr (edges1 frame)))

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
    (origin1 frame)
     (add-vect (scale-vect (xcor-vect v)
                          (edge1-1 frame))
             (scale-vect (ycor-vect v)
                         (edge2-1 frame))))))

(define (segments->>painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; EXERCISE 2.48
(define (make-segment2 point1 point2)
  (list point1 point2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))

; EXERCISE 2.49
; implementign with the sicp-pict library-defined calls allows us to use the built in painter.
(define outline-painter
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1))
                           (make-segment (make-vect 1 1) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x-painter
  (segments->painter (list (make-segment (make-vect 1 0) (make-vect 0 1))
                             (make-segment (make-vect 0 0) (make-vect 1 1)))))
(define diamond-painter
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                             (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                             (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                             (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(define (transform-painter2 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame2 new-origin
                              (sub-vect (m corner1) new-origin)
                              (sub-vect (m corner2) new-origin)))))))

(define (flip-vert2 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (shrink-to-upper-right painter)
  (transform-painter2 painter
                      (make-vect2 0.5 0.5)
                      (make-vect2 0.5 1.0)
                      (make-vect2 1.0 0.5)))
;Exercise 2.50
(define (beside2 painter1 painter2)
 (let ((split-point (make-vect 0.5 0.0)))
   (let ((paint-left
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             split-point
                             (make-vect 0.0 1.0)))
         (paint-right
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.0)
                             (make-vect 0.5 1.0))))
     (lambda (frame)
       (paint-left frame)
       (paint-right frame)))))

(define (flip-horiz2 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
(define (flip90cc painter)
  (transform-painter painter
                    (make-vect 1 0)
                    (make-vect 1 1)
                    (make-vect 0 0)))
(define (flip180cc painter)
  (flip90cc (flip90cc painter)))
(define (flip270cc painter)
  (flip90cc (flip180cc painter)))
                   
;Exercise 2.51
(define (below2 painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-top
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1 0)
                              split-point))
          (paint-bottom
               (transform-painter painter2
                        split-point
                        (make-vect 0 1)
                        (make-vect 1 0.5))))
  (lambda (frame)
    (paint-top frame)
    (paint-bottom frame)))))

(define (below3 painter1 painter2)
  (flip90cc (beside2 (flip270cc painter1)
           (flip270cc painter2))))







  