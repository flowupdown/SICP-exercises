#lang sicp

;the core of the evaluetor

;eval

;(define (eval exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((quoted? exp) (text-of-quotation exp))
;        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
;        ((if? exp) (eval-if exp env))
;        ((lambda? exp) (make-procedure (lambda-parameters exp)
;                                       (lambda-body exp)
;                                       env))
;        ((begin? exp)
;         (eval-sequence (begin-actions exp) env))
;        ((cond? exp) (eval (cond->if exp) env))
;        ((let? exp) (eval (let->combination exp) env))
;        ((let*? exp) (eval (let*->nested-lets exp)))
;        ((application? exp)
;         (meta-apply (eval (operator exp) env)
;                     (list-of-values (operands exp) env)))
;        (else
;         (error "Unknown expression type: EVAL" exp))))

;apply

;(define (meta-apply procedure arguments)
;  (cond ((primitive-procedure? procedure)
;         (apply-primitive-procedure procedure arguments))
;        ((compound-procedure? procedure)
;         (eval-sequence
;          (procedure-body procedure)
;          (extend-environment
;           (procedure-parameters procedure)
;           arguments
;           (procedure-environment procedure))))
;        (else
;         (error
;          ("Unknown procedure type: APPLY" procedure)))))

;procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;conditionals

;(define (eval-if exp env)
;  (if (true? (eval (if-predicate exp) env))
;      (eval (if-consequent exp) env)
;      (eval (if-alternative exp) env)))

;sequences

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;assignments and definitions

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;Exercise 4.1

(define (list-of-values-left-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (eval (first-operand exps) env)))
        (cons first-eval
              (list-of-values-left-right (rest-operands exps) env)))))

(define (list-of-values-right-left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-eval (eval (rest-operands exps) env))
            (first-eval (eval (first-operand exps) env)))
        (cons first-eval
              rest-eval))))

;representing expressions

;numbers and strings

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;variables

(define (variable? exp) (symbol? exp))

;quotations

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;assignments

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;definitions

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;lambda expressions

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;conditionals

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;begin

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

;application

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;derived expressions

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;Exercise 4.4

;and

(define (and? exp) (tagged-list? exp 'and))
(define (and-sequence exp) (cdr exp))
(define (and-first seq) (car seq))
(define (and-rest seq) (cdr seq))
(define (eval-and seq env)
  (let ((first (eval (and-first seq) env)))
    (if (null? first)
        true
        (if (null? (and-rest seq))
            first
            (if first
                (eval-and (and-rest seq))
                false)))))

;or

(define (or? exp) (tagged-list? exp 'or))
(define (or-sequence exp) (cdr exp))
(define (or-first seq) (car seq))
(define (or-rest seq) (cdr seq))
(define (or-no-exp? exp) (null? (or-sequence exp)))
(define (eval-or seq env)
  (cond ((or-no-exp? seq) false)
        ((eval (or-first seq) env) true)
        (else (eval-or (or-rest seq) env))))

;derived exps

(define (and-clauses exp) (cdr exp))
(define (and-first-clause clauses) (car clauses))
(define (and-rest-clauses clauses) (cdr clauses))
(define (and->or exp) (expand-and-clauses (and-clauses exp)))

;(define (expand-and-clauses clauses)
;  (if (null? clauses)
;      true
;      (let ((first (or (and-first-clause clauses))))
;        (if first
;            (expand-and-clauses (and-rest-clauses clauses))
;            false))))
            
(define (expand-and-clauses clauses)
  (if (null? clauses)
      true
      (make-if (and-first-clause clauses)
               (expand-and-clauses (and-rest-clauses clauses))
               false)))

;Exercise 4.6

(define (let? exp) (tagged-list? exp 'let))
(define (let-var-exp exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let-var var-exp)
  (map (lambda (x) (car x)) var-exp))
(define (let-exp var-exp)
  (map (lambda (x) (cadr x)) var-exp))
(define (let->combination exp)
  (let ((var-exp (let-var-exp exp)))
    (expand-let (let-var var-exp)
                (let-exp var-exp)
                (let-body exp))))
(define (expand-let vars exps body)
  (cons (make-lambda vars (list body))
        exps))
                           
;Exercise 4.7

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-vars exp) (cadr exp))
(define (let*-body exp) (caddr exp))
(define (make-let* vars body)
  (list 'let* vars body))
(define (make-end-let exp body)
  (list 'let (list exp) body))
(define (make-pre-let exp next)
  (list 'let (list exp next)))
  
(define (let*->nested-lets exp)
  (let ((first (car (let*-vars exp)))
        (rest (cdr (let*-vars exp)))
        (body (let*-body exp)))
    (if (null? rest)
        (make-end-let first
                      (let*-body exp))
        (make-pre-let first
                      (let*->nested-lets (make-let* rest body))))))

;Exercise 4.8

(define (expand-define-let var vars body)
  (list 'define (cons var vars) body))
;(define (let->combination exp)
;  (let ((var-exp (let-var-exp exp)))
;    (if (pair? var-exp)
;        (expand-let (let-var var-exp)
;                    (let-exp var-exp)
;                    (let-body exp))
;        (let ((new-exp (cdr exp)))
;          (let ((new-var-exp (let-var-exp new-exp)))
;            (begin
;              (expand-define-let var-exp
;                                (let-var new-var-exp)
;                                (let-body new-exp))
;              (cons var-exp (let-exp new-var-exp))))))))

;Exercise 4.9

;do

(define (do? exp) (tagged-list? exp 'do))
(define (do-init exp) (cadr exp))
(define (do-stop exp) (caddr exp))
(define (do-command exp) (car (cdddr exp)))
(define (do-make-iter inits)
  (cons 'iter
        (do-make-var inits)))
(define (do-make-var inits)
  (let ((first (car inits))
        (rest (cdr inits)))
    (if (null? rest)
        (list (car first))
        (cons (car first)
              (do-make-var rest)))))
(define (do-make-step inits)
  (let ((first (car inits))
        (rest (cdr inits)))
    (if (null? rest)
        (let ((init (cdddr first)))
          (if (null? init)
              (list (car first))
              (list (caddr first))))
        (cons (caddr first)
              (do-make-step rest)))))
(define (do-make-init inits)
  (let ((first (car inits))
        (rest (cdr inits)))
    (if (null? rest)
        (list (cadr first))
        (cons (cadr first)
              (do-make-init rest)))))
(define (do->definition exp)
  (make-begin
   (list (list 'define
               (cons 'iter (do-make-var (do-init exp)))
               (make-if (car (do-stop exp))
                        (cadr (do-stop exp))
                        (make-begin
                         (list
                         (do-command exp)
                         (append '(iter) (do-make-step (do-init exp)))))))
         (append '(iter) (do-make-init (do-init exp))))))

;evaluator data structures

;resting of predicates

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;representing procedures

(define (make-procedure parameters body env)
  (if (got-defines? body)
      (let ((new-body (scan-out-defines body)))
        (make-procedure parameters new-body env))
      (list 'procedure parameters body env)))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;operations on environments

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
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable:" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        'error
;        (error "Unbound variable")
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;Exercise 4.11

(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))
;(define (make-binding variable value)
;  (cons variable value))
;(define (binding-variable binding)
;  (car binding))
;(define (binding-value binding)
;  (cadr binding))
(define (make-frame-bs variables values)
  (cond ((and (null? (cdr variables)) (null? (cdr values)))
         (list (make-binding (car variables) (car values))))
        ((null? (cdr variables))
         (error "Too many arguments supplied" variables values))
        ((null? (cdr values))
         (error "Too few arguments supplied" variables values))
        (else (cons (make-binding (car variables) (car values))
                    (make-frame-bs (cdr variables) (cdr values))))))
(define (add-binding-to-frame-bs var val frame)
  (cons (make-binding var val) frame))
(define (extend-environment-bs vars vals base-env)
  (cons (make-frame-bs vars vals) base-env))
(define (lookup-variable-value-bs var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (binding-variable (first-binding frame)))
             (binding-value (first-binding frame)))
            (else (scan (rest-bindings frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))
(define (set-variable-value-bs! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (binding-variable (first-binding frame)))
             (set-cdr! (first-binding frame) val))
            (else (scan (rest-bindings frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (first-frame env))))
  (env-loop env))
(define (define-variable-bs! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
             (add-binding-to-frame-bs var val frame))
            ((eq? var (binding-variable (first-binding frame)))
             (set-cdr! (first-binding frame) val))
            (else (scan (rest-bindings frame)))))
    (scan frame)))

;Exercise 4.12

(define (scan-frame frame var)
  (let ((vars (frame-variables frame))
        (vals (frame-values frame)))
    (define (scan vars vals)
      (cond ((null? vars) nil)
            ((eq? var (car vars)) (cons (car vars) (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (scan vars vals)))

(define (env-loop env var)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (let ((cell (scan-frame frame var)))
          (if (null? cell)
              (env-loop (enclosing-environment env))
              cell)))))

(define (lookup-variable-value-common var env)
  (let ((cell (env-loop env var)))
    (cdr cell)))
(define (set-variable-value-common! var val env)
  (let ((cell (env-loop env var)))
    (set-cdr! cell val)))
(define (define-variable-common! var val env)
  (let ((cell (scan-frame (first-frame env) var)))
    (if (null? cell)
        (add-binding-to-frame! var val (first-frame env))
        (set-cdr! cell val))))

;Exercise 4.13

(define (make-unbound-common! var env)
  (let ((cell (scan-frame (first-frame env) var)))
    (if (null? cell)
        (error "There isn't variable to unbound")
        (set-cdr! cell '()))))

;running the evaluator as a program

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'pair? pair?)
        (list 'eq? eq?)
        (list '+ +)
        (list '* *)
        (list '= =)
        (list '- -)
        (list '/ /)
        (list '> >)
        (list 'not not)
        (list 'abs abs)
        (list 'true? true?)
        (list 'true true)
        (list 'false false)
        (list 'member member)
;        (list 'or or)
;        (list 'and and)
        (list 'not not)
        (list 'runtime runtime)
        (list 'list list)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
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
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define apply-in-underlying-scheme apply)
;(define input-prompt ";;; M-Eval input:")
;(define output-prompt ";;; M-Eval value:")
;(define (driver-loop)
;  (prompt-for-input input-prompt)
;  (let ((input (read)))
;    (let ((output (eval input the-global-environment)))
;      (announce-output output-prompt)
;      (user-print output)))
;  (driver-loop))
(define (analyze-driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (analyze-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (analyze-driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;(driver-loop)

;Exercise 4.14

;(define (square x) (* x x))

;Exercise 4.16

;b

(define (got-defines? exp)
  (if (or (null? exp) (procedure? exp))
      false
      (let ((first (car exp)))
        (if (definition? first)
            true
            (got-defines? (cdr exp))))))

(define (scan-out-defines exp)
  (let ((variables-list '()))
    (let ((let-list (list 'let '())))
      (define (scan exp)
        (if (null? exp)
            let-list
            (let ((first (car exp)))
              (if (definition? first)
                  (begin
                    (set! variables-list
                          (cons (list (definition-variable first) '*unassigned*)
                                variables-list))
                    (set! let-list
                          (append
                           (append (list 'let variables-list)
                                   (list (list 'set!
                                               (definition-variable first)
                                               (definition-value first))))
                           (cddr let-list)))
                    (scan (cdr exp)))
                  (begin
                    (set! let-list
                          (append let-list
                                  (list first)))
                    (scan (cdr exp)))))))
      (scan exp))))

;Exercise 4.20

;a

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-definitions exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (letrec-variable def) (car def))
(define (letrec-expression def) (cadr def))
(define (letrec->let exp)
  (let ((letrec-def (letrec-definitions exp))
        (letrec-body (letrec-body exp)))
    (let ((scanned (scan-letrec letrec-def)))
      (append scanned
              letrec-body))))
(define (scan-letrec exp)
  (let ((variables-list '()))
    (let ((let-list (list 'let '())))
      (define (scan exp)
        (if (null? exp)
            let-list
            (let ((first (car exp)))
              (begin
                (set! variables-list
                      (cons (list (car first) '*unassigned*)
                            variables-list))
                (set! let-list
                      (append
                       (append (list 'let variables-list)
                               (list (list 'set!
                                           (car first)
                                           (cadr first))))
                       (cddr let-list)))
                (scan (cdr exp))))))
      (scan exp))))

;'(letrec
;     ((even? (lambda (n)
;               (if (= n 0) true (odd? (- n 1)))))
;      (odd? (lambda (n)
;              (if (= n 0) false (even? (- n 1))))))
;   (+ n 2))
                    
;Exercise 4.21

(define 10-factorial
  (let ((start-time (runtime)))
    (begin
  ((lambda (n)
     ((lambda (fact) (fact fact n))
      (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
   10000)
  (- (runtime) start-time))))
    
        
(define (proc ft k)
  (if (= k 1) 1 (* k (proc ft (- k 1)))))

;(define (fib n)
;  (cond ((= n 0) 0)
;        ((= n 1) 1)
;        (else (+ (fib (- n 1))
;                 (fib (- n 2))))))

;a

(define 10-fibonacci
  ((lambda (n)
     ((lambda (fib) (fib fib n))
      (lambda (fb k) (cond ((= k 0) 0)
                           ((= k 1) 1)
                           (else (+ (fb fb (- k 1))
                                    (fb fb (- k 2))))))))
   10))

;b

(define (f x)
  (define (even? n)
    (if (= n 0) true (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) false (even? (- n 1))))
  (even? x))

(define (f-alt x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

;separating syntactic analysis from execution

(define (analyze-eval exp env) ((analyze exp) env))
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((require? exp) (analyze-require exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((unless? exp) (analyze-unless->if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze-let->combination exp))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))
;(define (analyze-self-evaluating exp)
;  (lambda (env) exp))
;(define (analyze-quoted exp)
;  (let ((qval (text-of-quotation exp)))
;    (lambda (env) qval)))
;(define (analyze-variable exp)
;  (lambda (env) (lookup-variable-value exp env)))
;(define (analyze-assignment exp)
;  (let ((var (assignment-variable exp))
;        (vproc (analyze (assignment-value exp))))
;    (lambda (env)
;      (set-variable-value! var (vproc env) env)
;      'ok)))
;(define (analyze-definition exp)
;  (let ((var (definition-variable exp))
;        (vproc (analyze (definition-value exp))))
;    (lambda (env)
;      (define-variable! var (vproc env) env)
;      'ok)))
;(define (analyze-if exp)
;  (let ((pproc (analyze (if-predicate exp)))
;        (cproc (analyze (if-consequent exp)))
;        (aproc (analyze (if-alternative exp))))
;    (lambda (env) (if (true? (pproc env))
;                      (cproc env)
;                      (aproc env)))))
;(define (analyze-lambda exp)
;  (let ((vars (lambda-parameters exp))
;        (bproc (analyze-sequence (lambda-body exp))))
;    (lambda (env) (make-procedure vars bproc env))))
;(define (analyze-sequence exps)
;  (define (sequentially proc1 proc2)
;    (lambda (env) (proc1 env) (proc2 env)))
;  (define (loop first-proc rest-procs)
;    (if (null? rest-procs)
;        first-proc
;        (loop (sequentially first-proc (car rest-procs))
;              (cdr rest-procs))))
;  (let ((procs (map analyze exps)))
;    (if (null? procs) (error "Empty sequence: ANALYZE"))
;    (loop (car procs) (cdr procs))))
;(define (analyze-application exp)
;  (let ((fproc (analyze (operator exp)))
;        (aprocs (map analyze (operands exp))))
;    (lambda (env)
;      (execute-application
;       (fproc env)
;       (map (lambda (aproc) (aproc env))
;            aprocs)))))
;(define (execute-application proc args)
;  (cond ((primitive-procedure? proc)
;         (apply-primitive-procedure proc args))
;        ((compound-procedure? proc)
;         ((procedure-body proc)
;          (extend-environment
;           (procedure-parameters proc)
;           args
;           (procedure-environment proc))))
;        (else
;         (error "Unknown procedure type: EXECUTE-APPLICATION"
;                proc))))

;Exercise 4.22

(define (analyze-let->combination exp)
  (let ((var-exp (let-var-exp exp)))
    (let ((let-vars (let-var var-exp))
          (let-exps (let-exp var-exp))
          (body (let-body exp)))
      (analyze-expand-let let-vars
                          let-exps
                          body))))
(define (analyze-expand-let vars exps body)
  (let ((lambda
            (cons (make-lambda vars (list body))
                  exps)))
    (analyze lambda)))
                 
;Exercise 4.26

(define (unless? exp) (tagged-list? exp 'unless))
(define (analyze-unless->if exp)
  (let ((condition (if-predicate exp))
        (exceptional-value (if-alternative exp))
        (usual-value (if-consequent exp)))
    (let ((if-expression (make-if condition
                                  exceptional-value
                                  usual-value)))
      (analyze-if if-expression))))
      
;(define (factorial n)
;  (unless (= n 1)
;          (* n (factorial (- n 1)))
;          1))
  

;an interpreter with lazy avaluation

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
;        ((quoted? exp) (eval (eval-quoted exp) env))
;        ((quoted? exp) (eval-quoted exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp)))
        ((application? exp)
         (meta-apply (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))
(define (meta-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))    ;changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)   ;changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY"
                     procedure))))
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps)
                      env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
;(define input-prompt ";;; L-Eval input:")
;(define output-prompt ";;; L-Eval value:")
;(define (driver-loop)
;  (prompt-for-input input-prompt)
;  (let ((input (read)))
;    (let ((output
;           (actual-value
;            input the-global-environment)))
;      (announce-output output-prompt)
;      (user-print output)))
;  (driver-loop))

;representing thunks

;(define (force-it obj)
;  (if (thunk? obj)
;      (actual-value (thunk-exp obj) (thunk-env obj))
;      obj))
(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj)
                     result)       ;replace exp with its value
           (set-cdr! (cdr obj)
                     '())          ;forget unneeded env
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

;Exercise 4.31

;(define (meta-apply procedure arguments env)
;  (cond ((primitive-procedure? procedure)
;         (apply-primitive-procedure
;          procedure
;          (list-of-arg-values arguments env)))    ;changed
;        ((compound-procedure? procedure)
;         (let ((parameters (procedure-parameters procedure)))
;           (eval-sequence
;            (procedure-body procedure)
;            (extend-environment
;             parameters
;             (list-of-delayed-args parameters arguments env)   ;changed
;             (procedure-environment procedure)))))
;        (else (error "Unknown procedure type: APPLY"
;                     procedure))))
;(define (list-of-delayed-args pars exps env)
;  (cond ((no-operands? pars) '())
;        ((pair? (first-operand pars))
;                (cons (delay-it (first-operand exps)
;                                env)
;                      (list-of-delayed-args (rest-operands pars)
;                                            (rest-operands exps)
;                                            env)))
;        (else (cons (eval (first-operand exps) env)
;                    (list-of-delayed-args (rest-operands pars)
;                                          (rest-operands exps)
;                                          env)))))
;(define (try a (b lazy)) (if (= a 0) 1 b))
;(try 0 (/ 1 0))

;streams as lazy lists
;(define (my-cons x y) (lambda (m) (m x y)))

;(define (list-ref items n)
;  (if (= n 0)
;      (car items)
;      (list-ref (cdr items) (- n 1))))

;(define (scale-list items factor)
;  (map (lambda (x) (* x factor)) items))
;(define (add-lists list1 list2)
;  (cond ((null? list1) list2)
;        ((null? list2) list1)
;        (else (cons (+ (car list1) (car list2))
;                    (add-lists (cdr list1) (cdr list2))))))
;(define ones (my-cons 1 ones))
;(define integers (cons 1 (add-lists ones integers)))

;Exercise 4.33

;(define (eval-quoted exp)
;  (let ((quoted-list (text-of-quotation exp)))
;    (define (iter ql)
;      (if (null? (cdr ql))
;          (car ql)
;          (make-lambda '(m) (list (cons 'm
;                                        (cons (car ql)
;                                              (list (iter (cdr ql)))))))))
;    (iter quoted-list)))
      
(define (eval-quoted exp env)
  (let ((quoted-list (text-of-quotation exp)))
    (define (iter ql)
      (if (null? (cdr ql))
          (car ql)
          (make-lambda '(m) (list (cons 'm
                                        (cons (list 'quote (car ql))
                                              (list (iter (cdr ql)))))))))
    (if (pair? quoted-list)
        (eval (iter quoted-list) env)
        quoted-list)))

;Exercise 4.34

;(define (driver-loop)
;  (prompt-for-input input-prompt)
;  (let ((input (read)))
;    (let ((output
;           (actual-value
;            input the-global-environment)))
;      (announce-output output-prompt)
;      (if (tagged-list? output 'procedure)
;;          (let ((variable (caddr (car (caddr output)))))
;;            (let ((look (lookup-variable-value variable the-global-environment)))
;;              (if (eq? look 'error)
;                  (let ((out (procedure->list output)))
;                    (let ((out-print (actual-value out the-global-environment)))
;                      (user-print out-print)))
;;                  (user-print 'infinite))))
;          (user-print output))))
;  (driver-loop))

(define (procedure->list exp)
  (list 'my-map '(lambda (x) x) (list 'quote exp) 10))
;(define (my-lookup var env)
;  (let ((look (lookup-variable-value var env)))
;    (if (eq? look 'error)
;        false
;        true)))

;(my-cons 1 (my-cons 2 '()))

(begin 
(define (my-cons x y) (lambda (m) (m x y)))
(define (my-car z) (z (lambda (p q) p)))
(define (my-cdr z) (z (lambda (p q) q)))
(define (my-map proc items n)
  (if (null? items)
      '()
      (if (= n 0)
          (cons 'infinite
                '())
          (cons (proc (my-car items))
                (my-map proc (my-cdr items) (- n 1)))))))
;(define (my-map proc items)
;  (if (null? items)
;      '()
;      (cons (proc (my-car items)) (my-map proc (my-cdr items)))))

;variations on a scheme - nondeterministic computing

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))
(define (require p) (if (not p) (amb)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (prime? n)
        (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;Exercise 4.35

(define (an-integer-between low high)
  (let ((integers (simple-integers low high))
        (medium (/ (- high low) 2)))
    (let ((int (an-element-of integers)))
      (require (medium? int medium))
      int)))
(define (medium? i m) (>= i m))
(define (simple-integers low high)
  (if (> low high)
      '()
      (cons low
            (simple-integers (+ low 1) high))))
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;Exercise 4.36

(define (pythagorian-triples-from n)
  (let ((i (an-integer-starting-from n)))
    (let ((j (an-integer-between n i)))
      (let ((k (an-integer-between n j)))
        (require (= (+ (* k k) (* j j)) (* i i)))
        (list i j k)))))

;Exercise 4.37

(define (a-pythagorean-triple-between-new low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

;logic puzzles

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= cooper 1)))
    (require (not (= baker 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;Exercise 4.38

(define (how-many-solutions? proc)
  (let ((counter 0)
        (start-time (runtime))
        (time 0))
    (proc)
    (newline)
    (set! counter (+ counter 1))
    (set! time (- (runtime) start-time))
;    (set! time (runtime))
    (display counter)
    (newline)
    (display time)
    (amb)))

(define (probe)
  (let ((time (runtime))
        (pro (an-integer-starting-from 1)))
    (require (= pro 10000))
    (- (runtime) time)))

;Exercise 4.39

(define (expert-dwelling-time) (how-many-solutions? multiple-dwelling))

;Exercise 4.40

(define (effective-multiple-dwelling)
  (let ((cooper (amb 2 3 4 5))
          (miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4 5)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require
            (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;Exercise 4.41

(define (pick-pair list1 n)
  (cond ((> n (length list1))
         (error "value out of region"))
        ((= n 1) (car list1))
        (else
         (pick-pair (cdr list1) (- n 1)))))
(define (invert l) (append (cdr l) (cons (car l) '())))
(define (one-lap l)
  (let ((first (car l)))
    (define (iter ll)
      (let ((inverted (invert ll)))
        (if (equal? first (car inverted))
            (list inverted)
            (cons inverted
                  (iter inverted)))))
    (iter l)))
(define (insert-item i l) (one-lap (cons i l)))
(define (smoove l)
  (if (null? l)
      '()
      (append (car l)
              (smoove (cdr l)))))
(define (five-combination)
  (let ((first (insert-item 2 '(1))))
    (define (iter result item)
      (if (= item 6)
          result
          (iter (smoove (map (lambda (x) (insert-item item x)) result)) (+ item 1))))
    (iter first 3)))
(define (my-cieve condit l)
  (cond ((null? l) '())
        ((true? (condit (car l))) (cons (car l) (my-cieve condit (cdr l))))
        (else (my-cieve condit (cdr l)))))
(define (my-dwelling)
  (define (baker l) (car l))
  (define (cooper l) (cadr l))
  (define (fletcher l) (caddr l))
  (define (miller l) (cadddr l))
  (define (smith l) (cadddr (cdr l)))
  (let ((comb (five-combination)))
    (let ((first (my-cieve (lambda (x) (not (= (baker x) 5))) comb)))
      (let ((second (my-cieve (lambda (x) (not (= (cooper x) 1))) first)))
        (let ((third (my-cieve (lambda (x) (not (= (fletcher x) 1))) second)))
          (let ((fourth (my-cieve (lambda (x) (> (miller x) (cooper x))) third)))
            (let ((fifth (my-cieve (lambda (x) (not (= (abs (- (smith x) (fletcher x))) 1))) fourth)))
              (let ((sixth (my-cieve (lambda (x) (not (= (abs (- (fletcher x) (cooper x))) 1))) fifth)))
                (map (lambda (x) (list (list 'baker (baker x))
                                       (list 'cooper (cooper x))
                                       (list 'fletcher (fletcher x))
                                       (list 'miller (miller x))
                                       (list 'smith (smith x))))
                     sixth)))))))))
     
;Exercise 4.42

(define (liars-puzzle)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
      (distinct? (list betty ethel joan kitty mary)))
    (require (or (and (= kitty 2) (not (= betty 3)))
                 (and (not (= kitty 2)) (= betty 3))))
    (require (or (and (= ethel 1) (not (= joan 2)))
                 (and (not (= ethel 1)) (= joan 2))))
    (require (or (and (= joan 3) (not (= ethel 5)))
                 (and (not (= joan 3)) (= ethel 5))))
    (require (or (and (= kitty 2) (not (= mary 4)))
                 (and (not (= kitty 2)) (= mary 4))))
    (require (or (and (= mary 4) (not (= betty 1)))
                 (and (not (= mary 4)) (= betty 1))))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

;Exercise 4.43

;friends--------yachts--------daughters
;moore----------lorna---------mary
;colonel--------melissa----------------
;hall-----------rosalind---------------
;barnacle-------gabrielle------melissa
;parker---------mary-------------------
    
(define (old-friends-puzzle)
  (let ((hall (amb 'lorna 'rosalind 'gabrielle)))
    (require (not (eq? hall 'rosalind)))
    (let ((parker (amb 'lorna 'rosalind 'gabrielle)))
      (require (not (eq? parker 'gabrielle)))
      (let ((colonel (amb 'lorna 'rosalind 'gabrielle)))
        (require (distinct?
                  (list colonel hall parker)))
        (let ((my-list (list (list 'moore 'mary)
                             (list 'colonel colonel)
                             (list 'hall hall)
                             (list 'barnacle 'melissa)
                             (list 'parker parker))))
          (require (eq? (search-list my-list 'gabrielle) (cadr (cadddr (cdr my-list)))))
          my-list)))))
(define (search-list l n)
  (if (eq? (cadr (car l)) n)
      (yacht (caar l))
      (search-list (cdr l) n)))
(define (yacht n)
  (cond ((eq? n 'moore) 'lorna)
        ((eq? n 'hall) 'rosalind)
        ((eq? n 'barnacle) 'gabrielle)))

;Exercise 4.44

(define (position n) (list n (amb 1 2 3 4 5 6 7 8)))

(define (queens)
  (let ((first (position 1))
        (second (position 2)))
    (require (safe? second (list first)))
    (let ((third (position 3)))
      (require (safe? third (list second first)))
      (let ((fourth (position 4)))
        (require (safe? fourth (list third second first)))
        (let ((fifth (position 5)))
          (require (safe? fifth (list fourth third second first)))
          (let ((sixth (position 6)))
            (require (safe? sixth (list fifth fourth third second first)))
            (let ((seventh (position 7)))
              (require (safe? seventh (list sixth fifth fourth third second first)))
              (let ((eighth (position 8)))
                (require (safe? eighth (list seventh sixth fifth fourth third second first)))
                (list first second third fourth fifth sixth seventh eighth)))))))))
        
(define (safe-row? i l)
  (cond ((null? l) true)
        ((not (= (car i) (caar l))) (safe-row? i (cdr l)))
        (else false)))
(define (safe-column? i l)
  (cond ((null? l) true)
        ((not (= (cadr i) (cadr (car l)))) (safe-column? i (cdr l)))
        (else false)))
(define (safe-diagonal? i l)
  (define (iter left right lis)
    (cond ((null? lis) true)
          ((and (not (equal? (car lis) (list (- (car left) 1) (- (cadr left) 1))))
                (not (equal? (car lis) (list (- (car right) 1) (+ (cadr right) 1)))))
           (iter (list (- (car left) 1) (- (cadr left) 1))
                 (list (- (car right) 1) (+ (cadr right) 1))
                 (cdr lis)))
          (else false)))
  (iter i i l))
(define (safe? i l)
  (if (true? (and (safe-row? i l)
                  (safe-column? i l)
                  (safe-diagonal? i l)))
      true
      false))

;parsing natural language

;(define nouns '(noun student professor cat class))
;(define verbs '(verb studies lectures eats sleeps))
;(define articles '(article the a))
;(define prepositions '(prep for to in by with))
;(define (parse-sentence)
;  (list 'sentence
;        (parse-noun-phrase)
;        (parse-word verbs)))
;(define (parse-noun-phrase)
;  (list 'noun-phrase
;        (parse-word articles)
;        (parse-word nouns)))
;(define (parse-word word-list)
;  (require (not (null? *unparsed*)))
;  (require (memq (car *unparsed*) (cdr word-list)))
;  (let ((found-word (car *unparsed*)))
;    (set! *unparsed* (cdr *unparsed*))
;    (list (car word-list) found-word)))
;(define (parse-simple-noun-phrase)
;  (list 'simple-noun-phrase
;        (parse-word articles)
;        (parse-word nouns)))

(define (parse input)
  (let ((nouns (amb 'student 'professor 'cat 'class))
        (verbs (amb 'studies 'lectures 'eats 'sleeps))
        (articles (amb 'the 'a))
        (prepositions (amb 'for 'to 'in 'by 'with)))
(define *unparsed* '())  
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))
(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
(define adjectives '(adjective pretty sunny funny))
(define (parse-simple-noun-phrase)
  (amb (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word nouns))
       (list 'simple-adjective-noun-phrase
             (parse-word articles)
             (parse-word adjectives)
             (parse-word nouns))))
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (set! *unparsed* (cdr *unparsed*))
  word-list)
(set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent)))

;implementing the amb evaluator

;simple expressions

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))
    
;conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;success continuation for evaluating the predicate
             ;to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;failure continuation for evaluating the predicate
             fail))))
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

;definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)           ;*1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()       ;*2*
                            (set-variable-value!
                             var old-value env)
                            (fail2)))))
             fail))))

;procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       ;succeed continuation for this aproc
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          ;success continuation for
          ;recurcive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
       fail)))
(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

;evaluating amb expressions

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

;driver-loop

;(define input-prompt ";;; Amb-Eval input:")
;(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline) (display ";;; Starting a new problem ")
            (ambeval
             input
             the-global-environment
             ;amb-eval success
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             ;ambeval failure
             (lambda ()
               (announce-output
                ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (driver-loop))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
(define (or x1 x2)
  (cond ((true? x1) 'true)
        ((true? x2) 'true)
        (else 'false)))
(define (and x1 x2)
  (if (true? x1)
      (if (true? x2)
          'true
          'false)
      'false))

;Exercise 4.50

(define (get-new-list l)
  (if (null? l)
      '()
      (let ((length-l (length l)))
        (let ((rand-l (+ (random length-l) 1)))
          (define (iter head tail counter)
            (if (= counter 1)
                (cons (car tail)
                      (append head (cdr tail)))
                (iter (append head (list (car tail)))
                      (cdr tail)
                      (- counter 1))))
          (iter '() l rand-l)))))
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (let ((new-choices (get-new-list choices)))
          (if (null? new-choices)
              (fail)
              ((car new-choices)
               env
               succeed
               (lambda () (try-next (cdr new-choices)))))))
      (try-next cprocs))))

(define (probe111 x)
  (let ((count (amb 1 2 3 4 5)))
    (require (> x count))
    x))

;Exercise 4.51

(define (analyze-permanent-set exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
                 (succeed 'ok fail2))
             fail))))

;Exercise 4.54

(define (require? exp)
  (tagged-list? exp 'require))
(define (require-predicate exp)
  (cadr exp))
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
               
;logic programming

;implementing the query system
;the driver loop and instantiation

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                 q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;the evaluator

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;simple queries

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;compound queries

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)

;filters

(define (negate oprands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null?
          (qeval (negated-query operands)
                 (singleton-stream frame)))
         (singleton-stream frame)
         the-enpty-stream))
   frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
            call
            frame
            (lambda (v f)
              (error "Unknown pat var: LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))
(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

;finding assertions by pattrn matching

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))
(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-enpty-stream
        (singleton-stream match-result))))
(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;rules and unification

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))
(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let (unify-result (unify-match query-pattern
                                    (conclusion clean-rule)
                                    queryp-frame)))
    (if (eq? unify-result 'failed)
        the-empty-stream
        (qeval (rule-body clean-rule)
               (singleton-stream unify-result)))))
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))   ;***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))
(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ((var? val)           ;***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)     ;***
           'failed)
          (else (extend var val frame)))))
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;maintainig the data base

(define THE-ASSERTIONS the-enpty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))
(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))
(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream
                assertion
                current-assertion-stream))))))
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))
(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))
(define (use-index? pat) (constant-symbol? (car pat)))

;stream oprations

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
        (stream-cdr s1)
        delayed-s2))))
(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
        (force delayed-s2)
        (delay (stream-cdr s1))))))
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))
(define (singleton-stream x)
  (cons-stream x the-empty-stream))

;query syntax procedures

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Undnown expression CONTENTS" exp)))
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))
(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))
(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))
(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))
(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (cadr (variable))
                  (string-append (symbol->string (caddr variable))
                                 "-"
                                 (number->string (cadr variable)))
                  (symbol->string (cadr variable)))))))

;frames and bindings

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assroc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))


          


