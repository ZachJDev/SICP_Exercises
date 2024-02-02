#lang sicp
;(define (square x) (* x x))
;(define (make-from-real-imag x y)
;  (cons (sqrt (+ (square x) (square y)))
;        (atan y x)))
;(define (make-from-mag-ang x y) (cons x y))
;
;(define (real-part z)
;(* (magnitude z) (cos (angle z))))
;(define (imag-part z)
;  (* magnitude z) (sin (angle z)))
;(define (magnitude z) (car z))
;(define (angle z) (cdr z))
;
;
;(define (add-compex z1 z2)
;  (make-from-real-imag (+ (real-part z1)
;                          (real-part z2))
;                       (+ (imag-part z1)
;                          (imag-part z2))))
;(define (sub-compex z1 z2)
;  (make-from-real-imag (- (real-part z1)
;                          (real-part z2))
;                       (- (imag-part z1)
;                          (imag-part z2))))
;(define (mul-complex z1 z2)
;  (make-from-mag-ang (* (magnitude z1)
;                        (magnitude z2))
;                     (+ (angle z1)
;                        (angle z2))))
;(define (div-complex z1 z2)
;  (make-from-mag-ang (/ (magnitude z1)
;                        (magnitude z2))
;                     (- (angle z1)
;                        (angle z2))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "BAD tagged datum -- CONTENTS" datum)))

;EXERCISE 2.75

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
    (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

; EXERCISES 2.79/80

; (define (equ? x y) (apply-generic 'equ? x y))
; (define (=zero? x) (apply-generic '=zero? x))

; in scheme-number:

; (put 'equ '(scheme-number scheme-number)
;    (lambda (x y) (eq? x y)))
; (put '=zero? '(scheme-number)
;   (lambda (x) (= 0 x)))

; in rational-number:

; 
; (put 'equ '(rational-number rational-number)
;    (lambda (x y) (and (eq? (numer x) (numer y))
;                       (eq? (denom x) (denom y))))) ; Assuming they're in simplest form, which it looks like make-rat does.
; (put '=zero? '(rational-number)
;   (lambda (x) (and (= 0 (numer x)) (not (= 0 (denom x))))))

; in complex-number:

; (put 'equ '(complex-number complex-number)
;    (lambda (x y) (and (eq? (real-part x) (real-part y))
;                        (eq? (imag-part x) (imag-part y)))))
; (put '=zero? '(complex-number)
;   (lambda (x) (= 0 (real-part x) (imag-part x))))

