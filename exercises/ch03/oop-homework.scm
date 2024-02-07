(load "obj.scm")

(define random random-integer)

(define-class (random-generator size)
  (instance-vars (count 0))
   (method (number)
           (begin
             (set! count (+ 1 count))
             (random size))))

(define r10 (instantiate random-generator 10))

;(print (ask r10 'number)) ; 6
;(print (ask r10 'number)) ; 1
;(print (ask r10 'number)) ; 6
;(print (ask r10 'count))  ; 3
 
(define-class (coke-machine can-limit price)
  (instance-vars (deposited-money 0)
                 (cans-in-machine 0))
  (method (fill count)
            (let ((cans-to-fill (min (- can-limit cans-in-machine) count)))
                (set! cans-in-machine (+ cans-in-machine cans-to-fill))))
  (method (deposit cents)
          (set! deposited-money (+ deposited-money cents)))
  (method (coke)
          (if (>= deposited-money price)
              (if (> cans-in-machine 0)
                  (let ((change (- deposited-money price)))
                  (begin
                   (set! deposited-money 0)
                   (set! cans-in-machine (- cans-in-machine 1))
                   change))
                  "Machine Empty")
              "Not Enough Money")))
  
(define my-machine (instantiate coke-machine 80 50))
; (ask my-machine 'fill 60)
; (ask my-machine 'fill 60)
; (ask my-machine 'deposit 70)
; (print (ask my-machine 'coke)) ; 20
; (print (ask my-machine 'cans-in-machine)) ; 79


; (ask my-machine 'fill 1)
; (ask my-machine 'deposit 50)
; (print (ask my-machine 'coke)) ; 0
; (ask my-machine 'deposit 70)
; (print (ask my-machine 'coke)) ; Machine Empty

(define ordered-deck '(AH 2H AC 2C AD 2D AS 2S))

(define (shuffle deck)
  (if (null? deck)
      '()
      (let ((card (nth (random (length deck)) deck)))
        (cons card (shuffle (remove card deck))))))
            

(define-class (deck)
  (instance-vars (stack (shuffle ordered-deck)))
  (method (deal)
          (if (ask self 'empty?) '()
            (let ((top-card (car stack)))
              (begin
               (set! stack (cdr stack))
               top-card))))
  (method (empty?)
          (null? stack)))

(define-class (miss-manners obj)
  (method (please meth arg)
          (ask obj meth arg)))

(define fussy-machine (instantiate miss-manners my-machine))
;(print (ask fussy-machine 'deposit 10))
(print (ask fussy-machine 'please 'deposit 10))
