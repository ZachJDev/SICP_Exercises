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

; Eval:
   
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
            (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze-let exp))
        ((application? exp) (analyze-application exp))
        (else 
        (error "UNKNOWN expression type -- EVAL" exp))))

(define (eval exp env)
    (display "Evaluating ") (display exp) (newline)
    ((analyze exp) env))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
        (primitive-implementiation proc) args))

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
