#lang racket
(provide (all-defined-out))
(require eopl "ast.ss")

;;parse: exp? ->ast?
;;it takes the expressions and generates the corresponding ast.
(define parse
  (lambda (exp)
    (cond
      [(number? exp) (num-ast exp)]
      [(boolean? exp) (bool-ast exp)]  
      [(symbol? exp) (id-ref-ast exp)]
      [(and (= (length exp) 4)(eq? (first exp) 'if)) 
       (if-ast (parse (second exp)) (parse (third exp)) (parse (fourth exp)))]
    
      [(and (>= (length exp) 2)(eq? (first exp) 'and) (translate-ast (and-ast (map parse (rest exp)))))]
      [(and (>= (length exp) 2)(eq? (first exp) 'or)
           (translate-ast (or-ast (map parse (rest exp)))))]
      [(eq? (first exp) 'let) (translate-ast (let-ast (map let-parse (second exp)) (parse (third exp))))]
      [(and (procedure? (first exp) ) (list? exp)) (app-ast (first exp) (map parse (second exp))) ]
      [(and (eq? (first exp) 'fn) (list? exp) (= (length exp) 3)) (fn-ast (second exp) (parse (third exp)))]
      [(and  (list? exp) (eq? (first exp) 'cond)) (translate-ast ( cond-ast (map  cond-temp (rest exp))))]
      [(and (list? exp) (= (length exp) 3) (eq? (first exp) 'fix))
              (let* ([fbs (second exp)]
                     [fbinds (map fbinds-aux fbs)]
                     [body (third exp)]
                     [body-ast (parse body)])
        (fix-ast fbinds body-ast))]
     
      
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      [(and (eq? 'new-ref (first exp))) (new-ref-ast (parse (second exp)))]
      [(and (eq? 'deref (first exp))) (deref-ast (parse (second exp)))]
      [(and (eq? 'setref (first exp))) (setref-ast (parse (second exp)) (parse (third exp)))]
      [(and(eq? 'seq (first exp))) (seq-ast (map parse (rest exp)))] 
      [(and (>= (length exp ) 2) (app-ast (parse (first exp)) (map parse (rest exp))))]
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      [#f])))

;;fbinds-aux: list? -> fbind?
;; this is an auxiliary function for making the bindings.
(define fbinds-aux
  (lambda (x)
    (cond
        [(and (list? x) (= (length x) 2))
           (let ([fname (first (first x))]
                 [formals (rest (first x))]
                 [body (parse (second x))]
            )
          (fbind fname formals body))]
        [(error "Error: problem in fbinds-aux (either x is list or its length !=3) ")])))

;;cond-temp: list? -> clause?
;; this is an auxiliary function for making the condition ast.
(define cond-temp
  (lambda(exp)
    (cond
      [(= (length exp) 2) (clause (parse (first exp))  (parse (second exp)))]
      [error "Error: problem in cond-temp (its length !=2)"])))

;;let-parse list? -> bind?
;; this is an auxiliary function for making the binding of the symbols and body.
(define let-parse
  (lambda(exp)
    (bind (first exp) (parse (second exp)))))

;;translate-ast: ast? ->  ast?
;;it takes and / or ast and it translates it into the corresponding if-ast. i.e. and-ast is tranlated into some recursive if's. 
(define translate-ast
  (lambda (exp)
    (cond 
      [(and-ast? exp) 
          (let ([asts (and-asts exp)])
             (if (= (length asts) 1)  (if-ast (first asts) (bool-ast #t) (bool-ast #f))
                 (if-ast (first asts) (translate-ast (and-ast (rest asts))) (bool-ast #f))))]
      [(or-ast? exp) 
         (let ([asts (or-asts exp)])
            (if (= (length asts) 1)  (if-ast (first asts) (bool-ast #t) (bool-ast #f) )
                (if-ast (first asts) (bool-ast #t)(translate-ast (or-ast (rest asts))))))]
      [(cond-ast? exp)                                         
         (let ([clauses (cond-clauses exp)])
         (cond
           [(and (= (length clauses) 1) (id-ref-ast? (clause-test (first clauses))) (eq? (id-ref-ast-id (clause-test (first clauses))) 'else)) 
            (clause-then (first clauses)) ]
           [ (let ([cls (first clauses)])
                   (if (= (length clauses) 1) 
                      (error 'Translate-Func "cond shud have  an else  clause. otherwise it will throw error")
                      (if-ast (clause-test cls) (clause-then cls)  (translate-ast (cond-ast (rest clauses))))
                      ))]
           ))]
      [(let-ast? exp)
       (app-ast  (fn-ast (map bind-id (let-ast-binds exp)) (let-ast-body exp)) (map bind-ast (let-ast-binds exp)))]
      [(error "Error: problem in translating ast" )])))
;(parse 5)
;(parse #t)
;(parse '(+ 1 2))
;(parse '(if (> x y) (f 2 3) (g 34 23)))
;(parse '(or (> x y) (f 2 3) (g 34 23)))
;(parse '(let ('(x 3) '(y 5)) (+ x y)))
; (parse '(fix ([(! n) (cond [(0? n) 1] [else (* n (! (sub1 n)))])]) (! 3)))
