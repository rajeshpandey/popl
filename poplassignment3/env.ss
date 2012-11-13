#lang racket
(provide (all-defined-out))
(require eopl "ast.ss")

;;empty-env?: env? -> boolean?
;; check for the empty env and return boolean value.
(define empty-env? 
  (lambda(e)
    (cases env e 
      [empty-env () #t]
      [else #f])))

;; extended-env-ids: env? ->list-of symbol?
;; this works as a extractor i.e. it extracts the symbol from the extended-env-ids ast and returns the list-of symbol.
(define extended-env-ids 
  (lambda(x)
    ( cases env x
       [extended-env (syms vals o-env) syms]
       [else (error "extended-env-ids")])))

;; extended-env-vals: env? ->any?
;; this works as a extractor i.e. it extracts the vals from the extended-env-vals ast and returns the vals .
(define extended-env-vals 
  (lambda(x)
    ( cases env x
       [extended-env (syms vals o-env) vals]
       [else (error "Error: problem in extended-env-vals (not a extended-env) ")])))

;; extended-env-outer-env: env? ->any?
;; this works as a extractor i.e. it extracts the vals from the extended-env-outer-env ast and returns theouter environment .
(define extended-env-outer-env
  (lambda(x)
    ( cases env x
       [extended-env (syms vals o-env) o-env]
       [else (error "Error: problem in extended-env-outer-env (not a extended-env)")])))

;; extended-fix-env-fids: env? ->list-of symbol?
;; this works as a extractor i.e. it extracts the ffids from the extended-fix-env-fids ast and returns ffids  .
(define extended-fix-env-fids 
  (lambda(x)
    ( cases env x
       [extended-fix-env (ffids fformals fbodies foutenv) ffids]
       [else (error "Error: problem in extended-fix-env-fids (not a extended-fix-env)")])))

;; extended-fix-env-fformals: env? ->list-of symbol?
;; this works as a extractor i.e. it extracts the vals from the extended-fix-env-fformals ast and returns fformals  .
(define extended-fix-env-fformals 
  (lambda(x)
    ( cases env x
       [extended-fix-env (ffids fformals fbodies foutenv) fformals]
       [else (error "Error: problem in extended-fix-env-fformals (not a extended-fix-env)")])))

;; extended-fix-env-fbodies env? ->ast?
;; this works as a extractor i.e. it extracts the vals from the extended-fix-env-fbodies ast and returns body ast  .
(define extended-fix-env-fbodies 
  (lambda(x)
    ( cases env x
       [extended-fix-env (ffids fformals fbodies foutenv) fbodies]
       [else (error "Error: problem in extended-fix-env-fbodies (not a extended-fix-env)")])))

;; extended-fix-env-outer-env env? ->env?
;; this works as a extractor i.e. it extracts the vals from the extended-fix-env-outer-env ast and returns environment.  
(define extended-fix-env-outer-env
  (lambda(x)
    ( cases env x
       [extended-fix-env (ffids fformals fbodies foutenv) foutenv]
       [else (error "Error: problem in extended-fix-env-fbodies (not a extended-fix-env)")])))

;; primitive-op proc? ->procedure?
;; this works as a extractor i.e. it extracts the vals from the primitive-op ast and returns operator.  
(define primitive-op
  (lambda(x)
    ( cases proc x
       [primitive (op sign) op]
       [else (error "Error: problem in primitive-op (not a primitive)")])))

;; primitive-sig proc? ->list-of symbols?
;; this works as a extractor i.e. it extracts the vals from the primitive-sig ast and returns list-f the symbol as a signature.
(define primitive-sig
  (lambda(x)
    ( cases proc x
       [primitive (op sign) sign]
       [else (error "Error: problem in primitive-sig (not a primitive)")])))

;; closure-formals list? ->list-of symbols?
;; this works as a extractor i.e. it extracts the formals from the closure-formals list and returns list-of the symbol as a formals.
(define closure-formals
  (lambda(x)
    ( cases proc x
       [closure (formals body env ) formals]
       [else (error "Error: problem in closure-formals (not a closure)")])))

;; closure-body list? ->ast?
;; this works as a extractor i.e. it extracts the vals from the closure-body list and returns body as ast.
(define closure-body
  (lambda(x)
    ( cases proc x
       [closure (formals body env ) body]
       [else (error "Error: problem in closure-body (not a closure)")])))

;; closure-env list? ->env?
;; this works as a extractor i.e. it extracts the vals from the closure-env list and returns environment.
(define closure-env
  (lambda(x)
    ( cases proc x
       [closure (formals body env ) env]
       [else (error "Error: problem in closure-env (not a closure)")])))


;; defining the constructor for the environment.
;; it also validates the environments.
(define-datatype env env?
  [empty-env]
  [extended-env (syms (list-of symbol?)) (vals (list-of any/c)) (outer-env env?)]
  [extended-fix-env (f-ids  (list-of symbol?) )
                     (f-formals (list-of (list-of symbol?)))     
                     (f-body (list-of ast?))
                     (f-outer-env env?)])

;; defining the constructor for the proc.
;;defining the proc datatype.
(define-datatype proc proc?
  [closure (formals (list-of symbol?)) (body ast?)(env env?)]
  [primitive (prim-op procedure?) (signature (list-of procedure?))])

;;lookup-env : env? symbol? -> any/c (ie any scheme value)
;;this is a function which is used for the looking up the specific environment whenever needed.
 (define lookup-env
   (lambda (e x)
    (cases env e 
      [empty-env() (error 'lookup-env "Sorry: no environment found.")]
      [extended-env (syms vals outer-env)
         (let ([i (list-index syms x)])
               (cond
                 [( = i -1) (lookup-env outer-env x)]
                 [ (list-ref vals i)]))]
                 [extended-fix-env (fids fformals fbodies outer-env) 
                        (let ([i (list-index fids x)]) 
                        (cond 
                              [(= i -1)(lookup-env outer-env x)]
                              [ (closure (list-ref fformals i)(list-ref fbodies i) e)]))]
      )))

;;list-index: list? symbol? -> value?
 ;; it checks for the index of the element in a given list.
(define list-index
  (lambda (lst x)
    (cond
          [(empty? lst) -1]
          [(list? (member x lst)) (- (length lst) (length (member x lst)))]
          [-1])))
