#lang sicp

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
    (make-lambda (let-params (let-definitions exp)) (let-body exp)))

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


(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
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

; Eval:

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp))
        ((lambda? exp)
            (make-procedure (lambda-params exp)
                            (lambda-body exp)
                            env))
        ((begin? exp)
            (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) (let-values (let-definitions exp)) env))
        ((application? exp)
            (apply (eval (operator exp) env)
                    (list-of-values (operands exp) env)))
        (else 
        (error "UNKNOWN expression type -- EVAL" exp))))

; EXERCISE 4.3 Data-directed Eval:

(define (install-eval-package)
    (put ('eval 'self-evaluating) (lambda (exp _env) exp))
    (put ('eval 'variable) lookup-variable-value)
    (put ('eval 'quoted) (lambda (exp _env) (text-of-quotation exp)))
    (put ('eval 'assignment) eval-assignment)
    (put ('eval 'definition) eval-definition)
    (put ('eval 'if) eval-if)
    (put ('eval 'lambda) (lambda (exp env) (make-procedure
                                                (lambda-params exp)
                                                (lambda-body exp)
                                                env)))
    (put ('eval 'begin) (lambda (exp env) (eval-sequence (begin-actions exp) env)))
    (put ('eval 'cond) (lambda (exp env) (data-directed-eval (cond->if exp) env)))
    (put ('eval 'call) (lambda (exp env) (apply (data-directed-eval (operator exp) env)
                                                      (list-of-values (operands exp) env)))))

(define (get-type exp)
    (cond ((pair? exp) (car exp))
          ((or (boolean? exp) (number? exp) (string? exp)) 'self-evaluating)
          ((symbol? exp) 'variable)
          (else (error "UNKNOWN type!"))))                                                      

(define (data-directed-eval exp env)
    (let ((proc (get 'eval (get-type exp))))
        (if proc
        (proc exp env)
        ((get 'eval 'call) exp env))))

; The other considersations not implemented above are:
; 1. any calls to eval in e.g. eval-assignment would still go to normal eval.
; 2. This assumes that there's some mechanism for tagging the data, which I
; Can only assume moves the `cond` clause somewhere else, unless the language
; being interpreted somehow includes those type tags when written.
; help for addressing these concerns thanks to https://mk12.github.io/sicp/index.html

; Apply:

(define (apply procedure arguments)
    (cond ((primitive-procedure? procedure) 
            (apply-primitive-procedure procedure arguments))
            ((compound-procedure? procedure) 
            (eval-sequence
                (procedure-body procedure)
                (extend-environment
                    (procedure-paramaters procedure)
                    arguments
                 (procedure-environment procedure))))
    (else 
        (error 
        "UNKOWN procedure type -- APPLY" procedure))))

; Exercise 4.10
; I may not go to the trouble of actually implementing it, 
; but I think an easy example would be changing the order of body and params
; for a lambda expression -- just switch the two selectors and rearrange 
; how `make-lambda` handles the params and body. Everything else should work fine.