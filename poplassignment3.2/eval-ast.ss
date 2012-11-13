#lang racket
(provide (all-defined-out))
(require eopl "ast.ss" "parse.ss" "env.ss" "store.ss")

;;eval-args: rands?,env?,store? -> [store? ,list?] 
;; it will return the store and the corresponding list.
(define eval-args
  (lambda(rands env s)
    (cond
      [(null? rands)(values s '())]
     [else (let*-values 
                    ([(news v) (eval-ast (first rands) env s)]
                     [(news1 v1) (eval-args (rest rands) env news)])
                  (values news1 (cons v v1)))])))


;;lookup-env-new-set: env?,symbol?, store? -> env?
;; it will return you the corresponding environemt.
(define lookup-env-new-set
  (lambda (e x s)
    (cases env e
      [empty-env () (error e x "Error: Symbol not in env.")]
      [extended-env (symbols values outer-env)
                    (let ([i (list-index symbols x)])
                      (cond [(= i -1) (lookup-env-new-set outer-env x s)]
                            [else (list-ref values i)]))]
      [extended-fix-env (fids fformals fbodies outer-env) 
                        (let ([i (list-index fids x)]) 
                        (cond 
                          [(= i -1)(lookup-env-new-set outer-env x s)]
                          [else (closure (list-ref fformals i)(list-ref fbodies i) e)]
                          ))]
      )))

;;new-ref-create: store?, list? -> [store? , list?]
;; this is an aid for creating the new references.
(define new-ref-create
  (lambda (s args)
    (cond
      [(null? args)(values s '())]
      [else (match-let([(list new_s v)(new-ref s (first args))])
              (let*-values([(new_st val)(new-ref-create new_s (rest args))])(values new_st (cons v val))))]
      )))


;; eva-ast: ast? env? -> any/c
;; this function is used to evaluate the ast generated after the parse.

(define (eval-ast a env s)
  (cases ast-datatype a
    [num-ast (datum) (values s datum)]
    [bool-ast (datum) (values s datum)]
    [id-ref-ast (id) (values s (lookup-env env id s))]
 
    [app-ast (rator rands)
           (let*-values ([(s p) (eval-ast rator env s)]
                 [(s1 args) (eval-args rands env s)] [(s2 r)(new-ref-create s1 args)])
             (if (proc? p) 
                 (apply-proc p r s2)
                 (error "Error: operator must be a defined procedure ")))]
    
    [if-ast (cond-ast ast1 ast2)   ;; succ-ast---ast1 , fail-ast--ast2
            (let-values ([(s cnd) (eval-ast cond-ast env s)]) ;;; condition cnd
              (cond
                [(eq? cnd #t) (eval-ast ast1 env s)]
                [(eq? cnd #f) (eval-ast ast2 env s)]
                [(error "Error:  problem in if-ast (condition must be of bool type)")]
              ))]
    
     [fn-ast (formals body) (values s (closure formals body env))]
    
   ; [fn-ast (formals body) (closure formals body env)]
    [fix-ast (fbinds body)
             (let ([new-env (extended-fix-env (map fbind-fname fbinds) (map fbind-formals fbinds) (map fbind-body fbinds) env)])
               (eval-ast body new-env s))]
    
   
     [set-ast (symbol ast) (let*-values
                               ([(r) (lookup-env-new-set env symbol s)]
                                [(s v)(eval-ast ast env s)]
                                [(s) (set-ref s r v)])
                             (values s (void)))
      ]
    
     [seq-ast (asts)
               (let-values ([(s x) (eval-args asts env s)])
                 (values s (list-ref x (- (length x) 1))))
      ]
    
    
 
    [else (error "Error:  in eval-ast")]
  ))

;; apply-closure: formals? body? env? rands? ->any/c
;; this applies closure and evaluates body.
(define (apply-closure formals body env rands s)
  (let ([new-env (extended-env formals rands env)])
    (eval-ast body new-env s)))

;;apply-proc : list? list? -> primitive? | closure?
;; returns primitive or closure?
(define (apply-proc rator rands s)
  (cases proc rator
    [primitive (op signature) (apply-prim-proc op signature rands s)]
    [closure (formals body env) (apply-closure formals body env rands s)]
  ))



;; apply-prim-proc op? rator? rands? ->any/c
;; this applies closure and evaluates value of the primitiev procedures.
(define (apply-prim-proc op rator rands s)
  (let ([return-type (first rator)]
        [arg-types (second rator)]
        [args (map (lambda (r) (deref s r)) rands)])
    (cond
      ;[(or (arg-types rands) (andmap arg-types rands)) (apply op rands)]
      [(arg-types args) (values s (apply op args))]
      [(error "Error:  in apply-prim-proc: (incorrect type of arguments to primitive)")]
    )))

;; it initailises the intial environments and binds the meaning of primitive operators to it real meaning.
(define store '())
(define init-env
  (lambda ()
    (match-let*
        ([(list store op+) (new-ref store (primitive + (list number? (list-of number?))))]
         [(list store op-) (new-ref store (primitive - (list number? (list-of number?))))]
         [(list store op*) (new-ref store (primitive * (list number? (list-of number?))))]
         [(list store op/) (new-ref store (primitive / (list number? (list-of number?))))]
         [(list store op<) (new-ref store (primitive < (list boolean? (list-of number?))))]
         [(list store op>) (new-ref store (primitive > (list boolean? (list-of number?))))]
         [(list store op<=) (new-ref store (primitive <= (list boolean? (list-of number?))))]
         [(list store op>=) (new-ref store (primitive >= (list boolean? (list-of number?))))]
         [(list store op=) (new-ref store (primitive = (list boolean? (list-of number?))))]
         [(list store opnot) (new-ref store (primitive not (list boolean? boolean?)))])
        (values store (extended-env '(+ - * / < > <= >= = not)
                                (list op+ op- op* op/ op< op> op<= op>= op= opnot)
                                (empty-env))))))



;;lookup-env : env? symbol? -> any/c (ie any scheme value)
;;this is a function which is used for the looking up the specific environment whenever needed.
 (define lookup-env
   (lambda (e x s)
    (cases env e 
      [empty-env() (error 'lookup-env "Sorry: no environment found.")]
      [extended-env (syms vals outer-env)
         (let ([i (list-index syms x)])
               (cond
                 [( = i -1) (lookup-env outer-env x s)]
                 [else (deref s (list-ref vals i))]))]
                 [extended-fix-env (fids fformals fbodies outer-env) 
                        (let ([i (list-index fids x)]) 
                        (cond 
                              [(= i -1)(lookup-env outer-env x s)]
                              [else (closure (list-ref fformals i)(list-ref fbodies i) e)]))]
      )))

;;list-index: list? symbol? -> value?
 ;; it checks for the index of the element in a given list.
(define list-index
  (lambda (lst x)
    (cond
          [(empty? lst) -1]
          [(list? (member x lst)) (- (length lst) (length (member x lst)))]
          [-1])))


;;run: exp? env? -> value?
;; this is the driver program.
;(define run   
 ; (lambda (exp  env)
 ; (eval-ast (parse exp) env)))

(define run
  (lambda(exp)
    (let-values([(s e)( init-env)])(let-values ([(s v)(eval-ast (parse exp) e s)]) v))
))


;(run '(let ((x (+ 2 3 4)))
 ;                           (let ((f (fn (y) (sub1 (sub1 x)))))
  ;                                  (f 5))) )
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

;(run '(let ([x 5]
     ;       [y 15])
     ;   (+ x y)))