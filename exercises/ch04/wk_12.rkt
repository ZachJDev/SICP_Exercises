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
    (display "looking up ") (display var)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                        (env-loop (enclosing-environment env))) ; co-recursive
                 ((eq? var (car vars)) 
                    (display " found ") (display (car vals)) (newline)
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


; Exercise 4.11

(define (extend-environment2 vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame2 vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too manyy arguments supplied!" vars vals)
            (error "Too few arguments supplied!" vars vals))))

(define (make-frame2 vars vals)
    (if (null? vars)
        '()
     (cons (cons (car vars) (car vals)) (make-frame2 (cdr vars) (cdr vals)))))

(define (frame-variables2 frame)
    (if (null? frame)
        '()
        (cons (caar frame) (frame-variables2 (cdr frame)))))


(define (frame-values2 frame)
    (if (null? frame)
        '()
        (cons (cdar frame) (frame-values2 (cdr frame)))))

(define (add-binding-to-frame!2 var val frame)
    (set-cdr! frame (cons (cons var val) (cdr frame))))

; Lookup continues to work, because of how it splits the variables and values.

(define (set-variable-value!2 var val env)
    (define (env-loop env)
        (define (scan var-vals)
            (cond ((null? var-vals) 
                        (env-loop (enclosing-environment env)))
                 ((eq? var (caar var-vals))
                    (set-cdr! (car var-vals) val))
                (else (scan (cdr var-vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
             (let ((frame (first-frame env)))
                (scan frame))))
(env-loop env))

(define (define-variable!2 var val env)
    (let ((frame (first-frame env)))
         (define (scan var-vals)
                (cond ((null? var-vals)
                        (add-binding-to-frame!2 var val frame))
                    ((eq? var (caar var-vals))
                        (set-cdr! (car var-vals) val))
                    (else (scan (cdr var-vals)))))
        (scan frame)))

; EXERCISE 4.13

(define (make-unbound! var env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond ((null? vars) (error "UNBOUND variable -- unbind" var))
                  ((eq? var (car vars))
                    (set-car! vars (cadr vars))
                    (set-car! vals (cadr vals))
                    (set-cdr! vars (cddr vars))
                    (set-cdr! vals (cddr vals))
                    )
                (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

; I would argue that this should only remove the binding from the top level --
; Any environment could have multiple frames extending it, and the call context
; of `make-unbound!` won't have the context of other frames who may be relying
; on those values.

; Eval:

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


(define (old-eval exp env)
    (display "EVALUATING: ") (display exp) (newline)
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
        ((cond? exp) (old-eval (cond->if exp) env))
        ((let? exp) (old-eval (let->combination exp) (let-values (let-definitions exp)) env))
        ((application? exp)
            (apply1 (old-eval (operator exp) env)
                    (list-of-values (operands exp) env)))
        (else 
        (error "UNKNOWN expression type -- EVAL" exp))))

(define (analyze-self-evaluating exp)
    (lambda (env) exp))
(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env) qval)))

(define (analyze-variable exp)
    (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
          (lambda (env) 
            (set-variable-value! var (vproc env) env)
            'ok)))

(define (analyze-definition exp)

    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
        (lambda (env) 
            (define-variable! var (vproc env) env)
            'ok)))

(define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp))))
        (lambda (env) 
            (if (true? (pproc env))
              (cproc env)
              (aproc env)))))

(define (analyze-sequence exps)
    (define (sequentially proc1 proc2)
    (display "sequentiallizing...") (newline)
        (lambda (env) (proc1 env) (proc2 env))) ; this will aways reduce to two procedures at the top level -- even if 'proc1' is itself the result of a call to sequentially.
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs)) (cdr rest-procs))))
    (let ((procs (map analyze exps)))
        (if (null? procs)
            (error  "EMPTY SEQUENCE! -- ANALYZE"))
            (loop (car procs) (cdr procs))))


; EXERCISE 4.23

(define (analyze-sequence2 exp)
    (define (execute-sequence procs env)
        (display "Executing sequence...") (newline)
        (cond ((null? (cdr procs)) ((car procs) env))
            (else ((car procs) env)
                    (execute-sequence (cdr procs) env))))
    (let ((procs (map analyze exp)))
        (if (null? procs)
            (error "EMPTY SEQUENCE! -- ANALYZE"))
            (lambda (env) (execute-sequence procs env))))

; (map analyze exp) will return an analyzed version of what was passed in. let's compare 
; how both versions will handle the expression (begin proc1)
; the second version will reduce to something like (lambda (env) (proc1 env))
; the first will reduce to (proc1 env). Proc1 will already be in the form (lambda (env) (foo env))
; given that that is result of (map analyze exp).
; How about something like (begin proc1 proc2 proc3)?
; the first version will be (lambda (env) ((lambda (env) (proc1 env) (proc2 env)) env) (proc3 env))
; the second: (lambda (env) (proc1 env) (proc2 env) (proc3 env))
; I think makes it pretty clear where the "the expressions in the sequence have been analyzed, but
; the sequence itself has not been" comes from. But I think I'm missing what the difference is.
; For more expressions, the calls to "execute-sequence" would be defered until the previous procedure
; has finished. In the first version, all the calls are ready to go after it's gone through analyze-sequence.


(define (analyze-lambda exp)
    (let ((vars (lambda-params exp))
          (bproc (analyze-sequence (lambda-body exp))))
      (lambda (env) (make-procedure vars bproc env))))

(define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp))))
        (lambda (env)
            (execute-application (fproc env)
                                    (map (lambda (aproc) (aproc env)) aprocs)))))

(define (execute-application proc args)
(display "EXECUTING PROC: ") (display (car proc)) (newline)
    (cond ((primitive-procedure? proc)
           (apply-primitive-procedure proc args))
          ((compound-procedure? proc)
          ((procedure-body proc)
            (extend-environment (procedure-paramaters proc)
                                args
                                (procedure-environment proc))))
        (else (error "UNKNOWN proc type -- EXECUTE-APPLICATION" proc))))

; EXERCISE 4.22

 (define (analyze-let exp)
    (let ((lproc (analyze-lambda (let->combination exp)))
          (letvals (map analyze (let-values (let-definitions exp)))))
        (lambda (env)
            (execute-application (lproc env) (map (lambda (val) (val env)) letvals)))))


(define (analyze exp)
    (display "ANALYZING: ") (display exp) (newline)
     (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((quoted? exp) (analyze-quoted exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp)
            (analyze-sequence2 (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze-let exp))
        ((application? exp) (analyze-application exp))
        (else 
        (error "UNKNOWN expression type -- EVAL" exp))))

(define (eval exp env)
    (display "Evaluating ") (display exp) (newline)
    ((analyze exp) env))

; (define eval old-eval)



; EXERCISE 4.3 Data-directed Eval:

; (define (install-eval-package)
;     (put ('eval 'self-evaluating) (lambda (exp _env) exp))
;     (put ('eval 'variable) lookup-variable-value)
;     (put ('eval 'quoted) (lambda (exp _env) (text-of-quotation exp)))
;     (put ('eval 'assignment) eval-assignment)
;     (put ('eval 'definition) eval-definition)
;     (put ('eval 'if) eval-if)
;     (put ('eval 'lambda) (lambda (exp env) (make-procedure
;                                                 (lambda-params exp)
;                                                 (lambda-body exp)
;                                                 env)))
;     (put ('eval 'begin) (lambda (exp env) (eval-sequence (begin-actions exp) env)))
;     (put ('eval 'cond) (lambda (exp env) (data-directed-eval (cond->if exp) env)))
;     (put ('eval 'call) (lambda (exp env) (apply (data-directed-eval (operator exp) env)
;                                                       (list-of-values (operands exp) env)))))

; (define (get-type exp)
;     (cond ((pair? exp) (car exp))
;           ((or (boolean? exp) (number? exp) (string? exp)) 'self-evaluating)
;           ((symbol? exp) 'variable)
;           (else (error "UNKNOWN type!"))))                                                      

; (define (data-directed-eval exp env)
;     (let ((proc (get 'eval (get-type exp))))
;         (if proc
;         (proc exp env)
;         ((get 'eval 'call) exp env))))

; The other considersations not implemented above are:
; 1. any calls to eval in e.g. eval-assignment would still go to normal eval.
; 2. This assumes that there's some mechanism for tagging the data, which I
; Can only assume moves the `cond` clause somewhere else, unless the language
; being interpreted somehow includes those type tags when written.
; help for addressing these concerns thanks to https://mk12.github.io/sicp/index.html

; Apply:

(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
        (primitive-implementiation proc) args))

(define (apply1 procedure arguments)
(display "EXECUTING PROC: ") (display (car procedure)) (newline)
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

(define input-prompt ";;; M-EVAL input:")
(define output-prompt ";;; M-EVAL value:")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (eval input the-global-environment)))
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

; Exercise 4.14

; I believe the answer is because of how lambda works -- the primitive map
; expects a primitive implementation of lambda, but that's not how the lambda expression
; is evaluated in the metacircular evaluator.

; Exercise 4.15
; The key here is to notice that (try try) is exactly what `halts?` is testing.
; So, regardless of what 'halts?` may theorteically return, the call to (try try)
; will do the opposite, which shows that `halts?` was incorrect.

