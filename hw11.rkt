#lang racket

(provide (all-defined-out))

;; A Variable (Var) is a Symbol
;; - vari (Symbol)

;; var=? : Any -> Boolean
;; Checks if a value is a symbol
(define (var=? x)
 (symbol? x)) 
;; EXAMPLES:
;; (var=? 'x)
;; (var=? 5)   

;; An Env (Environment) is one of:
;; - 'empty
;; - (cons (list Var Result) Env)
(struct empty-env [] #:transparent)
(define/contract (make-empty-env) 
 (-> empty-env?) 
  (empty-env)) 
;; Example:
;; (define my-env (make-empty-env))

;; env? : Any -> Boolean
;; Checks if a value is a valid Env
(define (env? env)
  (list? env))
;; EXAMPLES:
;; (env? (make-empty-env))
;; (env? '(('x 3))) 

;; Exception for syntax errors in CS450Lang
(struct exn:fail:syntax:cs450 [msg mrks] #:transparent)
;; exn:fail:syntax:cs450? : Any -> exn:fail:syntax:cs450
;; Checks if a value is a exn:fail:syntax:cs450
;; (define (exn:fail:syntax:cs450? e)
;;   (is-a? e exn:fail:syntax:cs450))
;; EXAMPLES:
;; (raise (exn:fail:syntax:cs450 "Invalid bind syntax" (current-continuation-marks)))
;; (raise (exn:fail:syntax:cs450 "Invalid syntax" (current-continuation-marks)))

;; An 450LangAST (abbreviated AST) is one of:
;; - (num Number)
;; - (vari Symbol)
;; - (bind Symbol AST AST)
;; - (call AST (listof AST))
(struct num [val] #:transparent)
(struct vari [name] #:transparent) 
(struct bind [var expr body] #:transparent) 
(struct call [fn args] #:transparent)

;; ast? : Any -> Boolean
;; Checks if a value is an 450LangAST
(define (ast? val)
  (and (list? val)
       (match val
         [`(num ,n) (number? n)]
         [`(vari ,sym) (symbol? sym)]
         [`(bind ,sym ,ast1 ,ast2) 
          (and (symbol? sym) 
               (ast? ast1) 
               (ast? ast2))]
         [`(call ,ast1 ,asts) 
          (and (ast? ast1) 
               (andmap ast? asts))]
         [_ #f])))
;; EXAMPLES:
;; (ast? (num 3))
;; (ast? (bind 'x (num 5) (num 20))) 
;; (ast? (call (vari 'add) (list (num 3) (num 4)))) 

(define/contract (make-num val)
  (-> number? num?)
  (num val))
(define/contract (make-bind vari expr body)
  (-> symbol? ast? ast? bind?)
  (bind vari expr body))
(define/contract (make-call fn args)
  (-> ast? (listof ast?) call?)
  (call fn args))
;; EXAMPLES:
;; (make-num 7)
;; (make-bind 'x (num 5) (num 10))  
;; (make-call (vari 'add) (list (num 4) (num 7)))

;; NaN? : Any -> Boolean
;; Checks if the result is NaN
(define/contract (NaN? result)
 (-> any/c boolean?)
  (nan? result))
;; EXAMPLES:
;; (NaN? (nan)) 
;; (NaN? 5) 

;; An ErrorResult is one of:
;; - UNDEFINED-ERROR
;; - NOT-FN-ERROR
(define UNDEFINED-ERROR 'UNDEFINED-ERROR)
(define NOT-FN-ERROR 'NOT-FN-ERROR)

;; UNDEFINED-ERROR? : Any -> Boolean
;; Checks if the result is UNDEFINED-ERROR
(define/contract (UNDEFINED-ERROR? val)
 (-> any/c boolean?)
  (equal? UNDEFINED-ERROR val))
;; EXAMPLES:
;;(UNDEFINED-ERROR? UNDEFINED-ERROR)
;;(UNDEFINED-ERROR? NOT-FN-ERROR)

;; NOT-FN-ERROR? : Any -> Boolean
;; Checks if the result is NOT-FN-ERROR
(define/contract (NOT-FN-ERROR? val)
 (-> any/c boolean?)
  (equal? NOT-FN-ERROR val))
;; EXAMPLES:
;; (NOT-FN-ERROR? NOT-FN-ERROR)
;; (NOT-FN-ERROR? UNDEFINED-ERROR)

;; Initial Environment
(define INIT-ENV
  (list
   (list '+ (lambda (x y) (if (and (number? x) (number? y)) (+ x y) (NaN))))
   (list '- (lambda (x y) (if (and (number? x) (number? y)) (- x y) (NaN))))))

;; A 450LangResult (abbreviated Result) is one of:
;; - Number
;; - (Racket) function
;; - NaN
;; - ErrorResult
(struct nan [] #:transparent)
(define/contract (NaN)
  (-> nan?)
  (nan))
;; Examples:
;; (NaN)

;; result? : Any -> Boolean
;; Checks if a value is a 450LangResult
(define (result? val)
  (or (number? val) 
      (nan? val)
      (procedure? val)
      (UNDEFINED-ERROR? val)
      (NOT-FN-ERROR? val)))    
;; EXAMPLES:
;; (result? NaN) 
;; (result? (num 10))  
;; (result? UNDEFINED-ERROR)

;; An 450LangExpr (abbreviated Expr) is one of:
;; - Number
;; - Variable
;; - (list ‘bind [Var Expr] Expr)
;; - (cons Expr . List<Expr>)

;; expr? : Any -> Boolean
;; Checks if a val is valid 450LangExpr
(define (expr? val)
  (or (number? val)
      (symbol? val)
      (and (list? val)
           (cond
             ;; Check for bind expression
             [(and (equal? (first val) 'bind)
                   (list? (second val))
                   (let ([bind-vars (second val)])
                     (and (pair? bind-vars)                ; Ensure it has at least two elements
                          (symbol? (first bind-vars))      ; First is a symbol
                          (pair? (rest bind-vars))         ; Rest exists for second element
                          (null? (rest (rest bind-vars)))  ; Ensure exactly two elements
                          (expr? (second bind-vars))       ; Second element is an expression
                          (expr? (third val)))))           ; Body is an expression
              #t]
             ;; Check for function call
             [(and (expr? (first val))
                   (andmap expr? (rest val)))
              #t]
             [else #f]))))
;; EXAMPLES:
;; (expr? 42)
;; (expr? 'x) 
;; (expr? '(bind (x 10) (+ x 5)))

;; env-add : Var Result Env -> Env
;; Adds a new variable and its associated value to the environment
(define/contract (env-add var val env)
 (-> symbol? result? env? env?)  
    (cons (list var val) env))
;; EXAMPLES:
;; (env-add 'x (num 5) (make-empty-env)) 
;; (env-add 'y (num 10) '((x (num 5))))

;; env-lookup : Env Var -> Result
;; Searches the environment for the specified ariable
(define/contract (env-lookup vari env)
  (-> symbol? env? result?)
  (cond
    [(empty-env? env) UNDEFINED-ERROR]
    [(null? env) UNDEFINED-ERROR]
    [(equal? (first (first env)) vari) (second (first env))]
    [else (env-lookup vari (rest env))]))
;; EXAMPLES:
;; (env-lookup 'x '((x (num 5))))
;; (env-lookup 'y '((x (num 5))))

;; Helper functions for parse
;; parse-call : Expr -> AST
;; Transforms a function call expression into its corresponding AST representation
(define/contract (parse-call expr)
 (-> expr? ast?)
  (list 'call 
        (parse (car expr)) 
        (if (null? (cdr expr))
            '()
            (map parse (cdr expr)))))
;; EXAMPLES:
;; (parse-call '(add 3 4))
;;(parse-call '(multiply x y))

;; parse-binding : Expr -> AST
;; Transforms a binding expression into its corresponding AST representation
(define/contract (parse-binding expr)
 (-> expr? ast?)
  (list 'bind 
        (car (cadr expr))            ; Extract variable
        (parse (cadr (cadr expr)))    ; Extract and parse the value
        (parse (caddr expr))))        ; Parse the body
;; EXAMPLES:
;; (parse-binding '(bind x 5 body))
;; (parse-binding '(bind y (add 3 4) body))

;; parse : Expr -> AST
;; Transforms a 450LangExpr to 450LangAST
(define/contract (parse expr)
  (-> expr? ast?)
  (cond
    [(number? expr) (list 'num expr)]
    [(symbol? expr) (list 'vari expr)]
    [(and (pair? expr)
          (eq? (car expr) 'bind)
          (pair? (cadr expr))
          (= (length (cadr expr)) 2)
          (symbol? (car (cadr expr))))
     (parse-binding expr)]
    [(and (pair? expr) (eq? (car expr) 'bind))
     (raise (exn:fail:syntax:cs450 "Invalid bind syntax" (current-continuation-marks)))]
    [(and (pair? expr) (not (null? expr)))
     (parse-call expr)]
    [else
     (raise (exn:fail:syntax:cs450 "Invalid syntax" (current-continuation-marks)))]))
;; EXAMPLES:
;; (parse 4) 
;; (parse 'hello)

;; Helper functions for run
;; handle-var : Symbol Env -> Result
;; Handles variable expressions by looking up the value in the environment
(define/contract (handle-var x env)
 (-> symbol? env? result?)
  (let ([val (env-lookup x env)])
    (if (UNDEFINED-ERROR? val)
        UNDEFINED-ERROR
        val)))
;; EXAMPLS:
;; (define env '((x 10)))
;; (handle-var 'x env)
;; (handle-var 'y env) ; gives error

;; handle-bind : Symbol AST AST Environment (AST -> Environment -> Result) -> Result
;; Handles binding expressions by evaluating the bound expression, 
;; updating the environment, and evaluating the body in the new environment
(define/contract (handle-bind var expr-to-eval body env eval-expr)
 (-> symbol? any/c any/c any/c any/c result?)
  (let* ([evaluated-expr (eval-expr expr-to-eval env)]
         [updated-env (env-add var evaluated-expr env)])
    (eval-expr body updated-env)))
;; EXAMPLES:
;; (handle-bind 'x '(num 10) '(vari x) '() (lambda (expr env) 10))
;; (handle-bind 'x '(num 5)
;;             '(bind x (num 15) 
;;                     (vari x))
;;             '() 
;;             (lambda (expr env) 15))

;; handle-call : AST (Listof AST) Environment (AST -> Environment -> Result) -> Result
;; Handles function calls by evaluating the function and its arguments
;; and applying the function if valid.
(define (handle-call fn-expr args env eval-expr)
  (let* ([fn-value (eval-expr fn-expr env)]
         [arg-values (map (lambda (arg) (eval-expr arg env)) args)])
    (cond
      [(or (UNDEFINED-ERROR? fn-value) (ormap UNDEFINED-ERROR? arg-values))
       UNDEFINED-ERROR]
      [(procedure? fn-value)
       (apply fn-value arg-values)]
      [else
       NOT-FN-ERROR])))
;; EXAMPLES
;; (handle-call '(vari multiply) '((num 4) (num 5)) '((multiply *))
;;             (lambda (expr env) *))
;; (handle-call '(vari add) '((num 2) (num 3)) '((add +))
;;             (lambda (expr env) +))

;; run : AST -> Result
;; Entry point for evaluating a program represented as an AST in an initial environment.
(define/contract (run ast)
  (-> ast? result?)

    ;; run/eval-expr : AST Env -> Result
    ;; Evaluates an AST expression with respect to the given environment
    ;; ACCUMULATOR: env : Env
    ;; INVARIANT: env represents the current state of variable bindings at each point of evaluation
    (define/contract (run/eval-expr expr env)
     (-> ast? env? result?)
      (match expr
        [`(num ,n) n]
        [`(vari ,x) (handle-var x env)]
        [`(bind ,var ,e-expr ,body) (handle-bind var e-expr body env run/eval-expr)]
        [`(call ,fn ,args) (handle-call fn args env run/eval-expr)]
        [_ UNDEFINED-ERROR]))

  (run/eval-expr ast INIT-ENV))
;; EXAMPLES:
;; (run (num 7)) 
;; (run '(bind x (num 5) (num 10))) 
