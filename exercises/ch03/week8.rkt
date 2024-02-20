#lang sicp
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "UNKNOWN Request -- MAKE-ACCOUNT" m))))
  dispatch)

; EXERCISE 3.3/4/7
(define (call-the-cops) (lambda args "Dialing 911 now..."))

(define (make-pw-protected obj pw)
  (define incorrect-pw-attempts 0)
  (define (dispatch pw-attempt method)
        (cond ((> incorrect-pw-attempts 6) (call-the-cops))
          ((not (eq? pw-attempt pw))
           (begin
            (set! incorrect-pw-attempts (+ incorrect-pw-attempts 1))
            (lambda args "Incorrect Password")))
          (else
           (begin
             (set! incorrect-pw-attempts 0)
             (obj method)))))
  dispatch)

(define (make-password-account pw balance)
  (make-pw-protected (make-account balance) pw))

; EXERCISE 3.7

(define (make-joint account old-pw new-pw)
  (make-pw-protected (lambda (method)(account old-pw method)) new-pw))

; EXERCISE 3.8

(define f
  (let ((prev-arg -1))
  (lambda (arg)
    (begin
      (define res (+ arg prev-arg))
      (set! prev-arg arg)
      res))))


