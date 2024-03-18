#lang sicp

; EXERCISE 3.38

; 1. Peter -> Paul -> Mary ((100 + 10) - 20) / 2 = 45
; 2. Paul -> Peter -> Mary ((100 - 20) + 10) / 2 = 45
; 3. Peter -> Mary -> Paul ((100 + 10) / 2) - 20 = 35
; 4. Paul -> Mary -> Peter ((100 - 20) / 2) + 10 = 50
; 5. Mary -> Peter -> Paul ((100 / 2) + 10) - 20 = 40
; 6. Mary -> Paul -> Peter ((100 / 2) - 20) + 10 = 40

; Other Options:
; They are all interspersed: which ever happed last would be the final balance
; so, 110, 80, or 50. And Mary would always have 50 dollars.
; Mary and Peter interspersed, Mary:sets balance 100/2, then Peter sets balance 100 + 10
;   finally Paul sets balance 110 - 20 = 80.

; EXERCISE 3.39
; the final execution, where X ends up 100. 

; EXERCISE 3.40
; I think the key here is figuring out all the combinations of the two 
; If x is set between multiplications
; 100^3 -- executes "in order"
; 1000^2 -- executes "backwards"
; 100 -- p1 accesses, p2 sets, p1 sets
; 1000 -- p2 accesses, p1 sets, p2 sets
; 10,000 -- p1 accesses x once, p2 sets x to 1000, p1 sets x OR
;           p2 accesses x twice, p1 sets x to 100, p2 sets x to (* 10 10 100)
; 100,000 -- p2 accesses x once, p1 sets x to 100, p2 sets x to (* 10 100 100)

; Exercise 3.41
; No, I don't think I agree, because reading a value and not doing anything with it is "timeless",
; there's no point at which reading a value could potentially change a value that another thread
; Relies on. 
; There is a possibility of getting back stale information, where one reads the value as it's
; being set by another process, so the balance returned may not match the actual balance. But 
; I don't think that's a real concern, as a "balance checker" could never be certain there wasn't a
; change a split second _after_ they get a value back. Because there's no practical difference between
; Concurrent execution and "very soon after" execution, I still don't think it's a problem.

; Exercise 3.42

; I'm leaning towards "it's not the same", but I can't really explain why in detail -- the clear difference
; is that two users calling the same procedure would no longer be executing different procedures.
; Depending on how make-serializer is implemnted, that may be a big problem. My guess is that 
; Make-serializer would not return the exact same procedure for two subsequent calls with the same arguments
; And if that's the case, this change would not work if the serializer relied on the previous differences
; in order to actually serialize multiple calss.

; EXERCISE 3.44
; I think I may be missing something here, but it seems like one difference is that 
; Ben is not using the exported serializer like it was explained that users would need to do.
; But if we assume the procedures _are_ serialized, I don't see anything wrong with this solution.
; The essential difference is that this procedure is not calculating a value based on the balance,
; As was teh case in the exchange proc. Without that, there's no need to serialize the whole thing.

; EXERCISE 3.48
; The deadlock problem is avoided because, in the example given, both users would attempt
; to acquire the mutex for the same account first, instead, they've ended up locking one without knowing 
; if they have access to the other. This is avoided if there is an order to accounts and a defined
; process for always locking mutices in a specific order. My implementation below would be improved by
; serialzing account-number creation.

(define next-account-number 0)

(define (make-account-and-serializer balance)
    (define account-numer next-account-number)
    (set! next-account-number (+ 1 next-account-number))
    ;...
    (define (dispatch m)
        (cond ((eq? m 'account-number) account-numer))
)
dispatch)

(define (exchange) 'exchanging...)

(define (serialzed-exchange account1 account2)
(let ((accounts (if (< (account1 'account-number) (account2 'account-number))
                    (cons account1 account2)
                    (cons account2 account1))))
    (let ((serializer1 ((car accounts 'serializer)))
          (serializer2 ((cdr accounts 'serializer))))
          ((serializer1 (serializer2 exchange))
          (car accounts)
          (cdr accounts)))))
                    