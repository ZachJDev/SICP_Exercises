#lang sicp

; (define DEBUG #t)
; (define (debug-message  messages) 
;     (cond ((eq? #f DEBUG) '()) 
;          ((null? messages)
;         )
;         (else (begin
;             (display (car messages))
;             (newline)
;             (debug-message (cdr messages))))))

; SYNTACTIC DEFINTIONS:

(define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
   (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (quoted? exp)
    (tagged-list? exp 'quote))

(define (text-of-quotation exp)
    (cadr exp))

(define (assignment? exp)
    (tagged-list? exp 'set!))

(define (assignment-variable exp)
    (cadr exp))

(define (assignment-value exp)
    (caddr exp))

(define (definition? exp)
    (tagged-list? exp 'define))

(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)  ; Formal Params
                     (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-params exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp) 
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp )(cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                            clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))

; EXERCISE 4.6

(define (let? exp) (tagged-list? exp 'let))

(define (let-definitions exp) (cadr exp))

(define (let-body exp) (caddr exp))

(define (let-params definitions)
    (if (null? definitions)
        '()
        (cons (caar definitions) (let-params (cdr definitions)))))

(define (let-values definitions)
    (if (null? definitions)
        '()
        (cons (cadar definitions) (let-params (cdr definitions)))))

(define (let->combination exp)
    (make-lambda (let-params (let-definitions exp)) (list (let-body exp))))

; EXERCISE 4.7
; I think it is sufficent to use derived expressions, 
; because the eval <-> apply loop will, on the next go-around
; turn the let into a lambda, and so on for each of the subsequent lets.

(define (let*? exp) (tagged-list? exp 'let*?))

(define (make-let definitions body) (list 'let definitions body))

(define (let*->nested-lets exp)
    (let ((body (let-body exp)))
        (define (let-helper definitions)
            (if (null? (cdr definitions))
                (make-let (list (car definitions)) body)
                (make-let (list (car definitions)) (let-helper (cdr definitions)))))
        (let-helper (let-definitions exp))))
    
(define (list-of-values exp env)
    (if (no-operands? exp)
        '()
        (cons (eval (first-operand exp) env)
                (list-of-values (rest-operands exp) env))))

; DATA STRUCTURES

(define (true? x)
    (not (eq? x false)))

(define (false? x)
(eq? x false))

; PROC DSs

(define (make-procedure parameters body env)
    (list 'procedure parameters body env))

(define (compound-procedure? p)
    (tagged-list? p 'procedure))

(define (procedure-paramaters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

; ENVIRONMENT

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
    (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too manyy arguments supplied!" vars vals)
            (error "Too few arguments supplied!" vars vals))))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                        (env-loop (enclosing-environment env))) ; co-recursive
                 ((eq? var (car vars)) 
                    (car vals))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error " Unbound Variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                    (frame-values frame)))))
    (env-loop env))


(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) 
                        (env-loop (enclosing-environment env)))
                 ((eq? var (car vars))
                    (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
             (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
(env-loop env))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
         (define (scan vars vals)
                (cond ((null? vars)
                        (add-binding-to-frame! var val frame))
                    ((eq? var (car vars))
                        (set-car! vals val))
                    (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
                (frame-values frame))))

; Eval:

(define (eval-if exp env)
    (if (true? (actual-value (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
     (else (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)

(define (eval-definition exp env)
    (define-variable!    (definition-variable exp)
                         (eval (definition-value exp) env)
                         env)
    'ok)    


(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
            (make-procedure (lambda-params exp)
                            (lambda-body exp)
                            env))
        ((begin? exp)
            (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) (let-values (let-definitions exp)) env))
        ((application? exp)
            (apply1 (actual-value (operator exp) env)
                     (operands exp) env))
        (else 
        (error "UNKNOWN expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

; Apply:

(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
        (primitive-implementiation proc) args))

(define (apply1 procedure arguments env)
    (cond ((primitive-procedure? procedure) 
            (apply-primitive-procedure procedure (list-of-arg-vals arguments env))) ; If it is a primitive procedure, we want the actual values, so we can apply it.
            ((compound-procedure? procedure) 
              (eval-sequence
                (procedure-body procedure)
                (extend-environment
                    (procedure-paramaters procedure)
                   (list-of-delayed-args arguments env) ; If it is a compound procedure, we create a list of thunks.
                 (procedure-environment procedure))))
    (else 
        (error 
        "UNKOWN procedure type -- APPLY" procedure))))

(define (list-of-arg-vals exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-vals (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


(define (force-it obj)
  (cond ((thunk? obj)
      (let ((result
      (actual-value (thunk-exp obj) (thunk-env obj))))
        (set-car! obj 'evaluated-thunk)
        (set-car! (cdr obj) result)
        (set-cdr! (cdr obj) '())
        result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp obj)
  (cadr obj))
(define (thunk-env obj) (caddr obj))

(define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
          (list '+ +)
          (list '/ /)
          (list '- -)
          (list '* *)
          (list '= =)
          ))


(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (primitive-procedure-names)
    (map car primitive-procedures))

(define (setup-environment)
    (let ((initial-env
            (extend-environment (primitive-procedure-names)
                                (primitive-procedure-objects)
                                the-empty-environment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))

(define (primitive-implementiation proc) (cadr proc))

; REPL

(define input-prompt ";;; L-EVAL input:")
(define output-prompt ";;; L-EVAL value:")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (actual-value input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
    (driver-loop))

(define (prompt-for-input string)
    (newline) (newline)
    (display string) (newline))

(define (announce-output string)
    (newline) (display string) (newline))

(define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                        (procedure-paramaters object)
                        (procedure-body object)
                        '<procedure-env>))
        (display object)))

(driver-loop)

; Exercise 4.27

; count -> 0 ; Nothing has changed it (id has not been called because w is currently ('thunk...)),
             ; and it is a definition, no application is happening.


; count above was incorrect according to my tests -- it was actually 1. Why?
  ; What I missed was that the outside call's to  (id ...) was computing as part of (eval-sequence) inside of
  ; (eval-definition). So count is incremented as part of that outside call, and, until we call w,
  ; w's value is ('thunk (id 10) <some-env>). only when we call w does (driver-loop) ask for it's (actual-value),
  ; and we finally evaluate the initial call to (id 10).
  ; we can test this by instead defining w as (id1 (id2 10)), where id1 and increment count by 1 and 2, respectively.
  ; after defining w, count is 1, and after calling it, count is 3.


; w -> 10 ; (id 10) will return 10, so the outer call to (id (id 10)) will reduce to (id 10).

; count -> 2 ; a thunk is not memoizing a proc call with specific arguments, but rather a specific call. So
             ; if two calls to the same proc are made with the same arguments, it will still be treated as two
             ; separate objects.
