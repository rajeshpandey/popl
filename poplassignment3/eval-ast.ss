#lang racket
(provide (all-defined-out))
(require eopl "ast.ss" "parse.ss" "env.ss" "store.ss" "semantic-domain.ss")

;; eva-ast: ast? env? -> any/c
;; this function is used to evaluate the ast generated after the parse.
(define eval-args
  (lambda(rands env s)
    (cond
      [ (null? rands)(values s '())]
      [else(let*-values 
                    ([(new_st v1) (eval-ast (first rands) env s)]
                     [(new_st1 v2) (eval-args (rest rands) env new_st)])
                  (values new_st1 (cons v1 v2)))])))

(define eval-ast 
  (lambda (a env s)
  (cases ast-datatype a
    [num-ast (datum) (values s datum)]
    [bool-ast (datum) (values s datum)]
    [id-ref-ast (id) (values s (lookup-env env id))] 
    
    [if-ast (cond-ast ast1 ast2)   ;; succ-ast---ast1 , fail-ast--ast2
            (let-values ([(s cnd) (eval-ast cond-ast env s)]) ;;; condition cnd
              (cond
                [(eq? cnd #t) (eval-ast ast1 env s)]
                [(eq? cnd #f) (eval-ast ast2 env s)]
                [(error "Error:  problem in if-ast (condition must be of bool type)")]
              ))]
    
    [fn-ast (formals body) (values s (closure formals body env))]
    [fix-ast (fbinds body)
             (let ([new-env (extended-fix-env (map fbind-fname fbinds) (map fbind-formals fbinds) (map fbind-body fbinds) env)])
               (eval-ast body new-env s))]  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;   [app-ast (rator-ast rands-ast)
    ;         (let ([rator (eval-ast rator-ast env)]
    ;               [rands (map (lambda (x) (eval-ast x env)) rands-ast)])
   ;            (if (proc? rator) (apply-proc rator rands) (error "Error: in eval-ast: (error app-ast, rator is not a proc )"))
   ;          )]
    
    [app-ast (rator-ast rands-ast)
           (let-values ([(s p) (eval-ast rator-ast env s)]
                 [(s1 args) (eval-args rands-ast env s)])  
             (if (proc? p) 
                 (apply-proc p args s1)
                 (error "Error: in eval-ast: (error app-ast, rator is not a proc )")))]
    ;;;;;;;;;;;;;;;;;;;;;;;
    [new-ref-ast (ast-new)
               (let-values ([(s v) (eval-ast ast-new env s)])
                 (match-let ([(list s r) (new-ref s v)])
                    (values s r)))]
    
    
    [deref-ast (ast-new)
               (let-values ([(s r)(eval-ast ast-new env s)])
                 (values s (deref s r)))]
    
    [setref-ast (ast-new1 ast-new2)
                 (let-values ([(s1 r1)(eval-ast ast-new1 env s)]  ;; this will return the store and reference
                              [(s2 v2)(eval-ast ast-new2 env s)] ;; this will return store and value.
                             )
                   (let ([s (set-ref s r1 v2)])(values s (void)))
                  )]
    [seq-ast (ast-new)
             (let-values([(s val)( eval-args ast-new env s)])
               (values s (list-ref val (- (length val) 1)))
               )]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    [else (error "Error: in eval-ast")]
  )))

;; apply-closure: formals? body? env? rands? ->any/c
;; this applies closure and evaluates body.
(define apply-closure
  (lambda (formals body env rands s)
  (let ([new-env (extended-env formals rands env)])
    (eval-ast body new-env s))))

;;apply-proc : list? list? -> primitive? | closure?
;; returns primitive or closure?
(define apply-proc
  (lambda (rator rands s)
  (cases proc rator
    [primitive (op signature) (apply-prim-proc op signature rands s)]
    [closure (formals body env) (apply-closure formals body env rands s)]
  )))

;; apply-prim-proc op? rator? rands? ->any/c
;; this applies closure and evaluates value of the primitiev procedures.
(define apply-prim-proc
  (lambda (op rator rands s)
  (let ([return-type (first rator)]
        [arg-types (second rator)])
    (cond
      [(or (arg-types rands) (andmap arg-types rands))(values s (apply op rands) )]
      [(error "Error:  in apply-prim-proc: (incorrect type of arguments to primitive)")]
    ))))

;; it initailises the intial environments and binds the meaning of primitive operators to it real meaning.
(define init-env
                      (extended-env '(* / + - sub1 add1 < > <= >= 0? = not)
                      (list
                         (primitive * (list number? (list-of number?)))   
                         (primitive / (list number? (list-of number?))) 
                         (primitive + (list number? (list-of number?)))
                         (primitive - (list number? (list-of number?)))
                         (primitive sub1 (list number? number?))
                         (primitive add1 (list number? number?))
                         (primitive < (list boolean? (list-of number?)))
                         (primitive > (list boolean? (list-of number?)))
                         (primitive <= (list boolean? (list-of number?)))
                         (primitive >= (list boolean? (list-of number?)))
                         (primitive zero? (list boolean? number?))
                         (primitive = (list boolean? (list-of number?)))
                         (primitive not (list boolean? boolean?))) 
                         (empty-env)
  ))



;;run: exp? env? -> value?
;; this is the driver program.
;(define run   
 ; (lambda (exp  env)
 ; (eval-ast (parse exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define run
  (lambda(exp env1)
    (let-values ([(s v)( eval-ast (parse exp) env1 '())])
      v)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(run '(let ([x (new 5)]
 ;           [y (new 6)])
  ;      (+ (deref x) (deref y))) init-env)

;(eval-ast (parse '(+ 5 8)) init-env)
;(eval-ast (parse '(if  (> 1 9) #t #f )) init-env)
;(eval-ast (parse '(if #f #t #f ))  (empty-env))
;(eval-ast (parse '(and #t #t #t 5)) (empty-env))
;(run '(+ 2 2 2))
;(run '(sub1 5))
;(run '(cond
       ;  [(=  1) #f]
 ;        [(> 2 1) #t]
  ;       [else (+ 1 1)]))
;(run '( (fn (x y) (+ x y)) 5 6))