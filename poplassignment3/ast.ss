#lang racket
(provide (all-defined-out))
(require eopl)

;;num-ast?: ast? -> boolean?
;; check for the number ast.
(define num-ast?
  (lambda(x)
    ( cases ast-datatype x
       [num-ast (datum) #t]
       [else #f])))

;; num-ast-datum: ast? -> value?
;; this works as a extractor i.e. it extracts the value from the number ast and returns the value.
(define num-ast-datum
  (lambda(x)
    ( cases ast-datatype x
       [num-ast (datum) datum]
       [else (error "Error: problem in num-ast-datum (wrong num-ast) ")])))

;;bool-ast?: ast? -> boolean
;; check for the boolean ast.
(define bool-ast?
  (lambda(x)
    ( cases ast-datatype x
       [bool-ast (datum) #t]
       [else #f])))

;; bool-ast-datum: ast? -> value?
;; this works as a extractor i.e. it extracts the value from the bool ast and returns the value.
(define bool-ast-datum
  (lambda(x)
    ( cases ast-datatype x
       [bool-ast (datum) datum]
       [else (error "Error: problem in bool-ast-datum (wrong bool-ast) ")])))

;;id-ref-ast?: ast? -> boolean?      ; subtype predicate
;; checks for the symbol ast .
(define id-ref-ast?
  (lambda(x)
    ( cases ast-datatype x
       [id-ref-ast (sym) #t]
       [else #f])))

;; id-ref-ast-id: ast? -> symbol?
;; this works as a extractor i.e. it extracts the value from the id-ref-ast and returns the symbol.
(define id-ref-ast-id
  (lambda(x)
    ( cases ast-datatype x
       [id-ref-ast (sym) sym]
       [else (error "Error: problem in id-ref-ast-id (not a id-ref-ast) ")])))

;;app-ast?: ast? -> boolean? 
;; checks for the app ast .
(define app-ast?
  (lambda(x)
    ( cases ast-datatype x
       [app-ast (rator rands)#t]
       [else #f])))

;; app-ast-rator ast? ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the app-ast-rator and returns the symbol.
(define app-ast-rator
  (lambda(x)
    ( cases ast-datatype x
       [app-ast (rator rands) rator]
       [else (error "Error: problem in app-ast-rator (not a  app-ast) ")])))

;; app-ast-rator ast? ast? -> list-of ast?
;; this works as a extractor i.e. it extracts the value from the app-ast-rator and returns the list-of asts.
(define app-ast-rands
  (lambda(x)
    ( cases ast-datatype x
       [app-ast (rator rands) rands]
       [else (error "Error: problem in app-ast-rands (not a  app-ast) ")])))

;;if-ast?: ast? -> boolean? 
;; checks for the if  ast .
(define if-ast?
  (lambda(x)
    ( cases ast-datatype x
       [if-ast (exp1 exp2 exp3) #t]
       [else #f])))

;; if-ast-test: ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the if-ast-test and returns the test ast.
(define if-ast-test
  (lambda(x)
    ( cases ast-datatype x
       [if-ast (exp1 exp2 exp3) exp1]
       [else (error "Error: problem in if-ast-test (not a  if-ast) ")])))

;; if-ast-then: ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the if-ast-then and returns the then ast.
(define if-ast-then
  (lambda(x)
    ( cases ast-datatype x
       [if-ast (exp1 exp2 exp3) exp2]
       [else (error "Error: problem in if-ast-then (not a  if-ast) ")])))

;; if-ast-else: ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the if-ast-else and returns the else ast.
(define if-ast-else
  (lambda(x)
    ( cases ast-datatype x
       [if-ast (exp1 exp2 exp3) exp3]
       [else (error "Error: problem in if-ast-else (not a  if-ast) ")])))

;;and-ast?: ast? -> boolean? 
;; checks for the  and  ast .
(define and-ast?
  (lambda(x)
    ( cases ast-datatype x
       [and-ast (rands) #t]
       [else #f])))

;; and-asts ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the and-asts and returns the operands ast.
(define and-asts
  (lambda(x)
    ( cases ast-datatype x
       [and-ast (rands) rands]
       [else (error "Error: problem in and-asts (not a  and-asts) ")])))

;;or-ast?: ast? -> boolean? 
;; checks for the or  ast .
(define or-ast?
  (lambda(x)
    ( cases ast-datatype x
       [or-ast (rands) #t]
       [else #f])))

;; or-asts ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the or-asts and returns the operands ast.
(define or-asts
  (lambda(x)
    ( cases ast-datatype x
       [or-ast (rands) rands]
       [else (error "Error: problem in or-asts (not a  or-asts) ")])))

;;let-ast?: ast? -> boolean? 
;; checks for the let-ast .
(define let-ast?
  (lambda(x)
    ( cases ast-datatype x
       [let-ast (binds body) #t]
       [else #f])))

;; let-ast-binds ast? -> bind?
;; this works as a extractor i.e. it extracts the value from the let-ast and returns the binds.
(define let-ast-binds
  (lambda(x)
    ( cases ast-datatype x
       [let-ast (binds body) binds]
       [else (error "Error: problem in let-ast-binds (not a  let-ast)")])))

;; let-ast-body ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the let-ast-body and returns the body ast.
(define let-ast-body
  (lambda(x)
   ( cases ast-datatype x
       [let-ast (binds body) body]
       [else (error "Error: problem in let-ast-body (not a  let-ast)")])))

;; bind-id: ast? -> symbol?
;; this works as a extractor i.e. it extracts the value from the bind-id and returns the id.
(define bind-id
  (lambda(x)
    (cases bind-datatype x
      [bind (sym a) sym]
      [else (error "Error: problem in bind-id (not a  bind)")])))

;; bind-ast: ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the bind-id and returns the id.
(define bind-ast
  (lambda(x)
    (cases bind-datatype x
      [bind (sym a) a]
      [else (error "Error: problem in bind-ast (not a  bind)")])))

;; clause-test ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the clause-test and returns the ast1 i.e. test ast.
(define clause-test
  (lambda(x)
    (cases clause-datatype x
      [clause (ast1 ast2) ast1]
      [else (error "Error: problem in clause-test (not a  clause)")])))

;; clause-then ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the clause-then and returns the ast1 i.e. then ast.
(define clause-then
  (lambda(x)
    (cases clause-datatype x
      [clause (ast1 ast2) ast2]
      [else (error "Error: problem in clause-then (not a  clause)")])))

;; cond-ast? ast? -> boolean?
;; check whether it is a valid cond-ast or not.
(define cond-ast?
  (lambda(x)
    ( cases ast-datatype x
       [cond-ast (clauses) #t]
       [else #f])))

;; cond-clauses ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the cond-clauses and returns the clauses.
(define cond-clauses
  (lambda(x)
    ( cases ast-datatype x
       [cond-ast (clauses) clauses]
       [else (error "Error: problem in clause-then (not a  clause)" )])))

;; fn-ast? ast? -> boolean?
;; check whether it is a valid fn-ast or not.
(define fn-ast?
  (lambda(x)
    ( cases ast-datatype x
       [fn-ast (formals body) #t]
       [else #f])))

;; fn-ast-formals ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the fn-ast-formals and returns the formals ast.
(define fn-ast-formals
  (lambda(x)
    ( cases ast-datatype x
       [fn-ast (formals body) formals]
       [else (error "Error: problem in fn-ast-formals (not a  fn-ast)")])))

;; fn-ast-body ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the fn-ast-body and returns the body ast.
(define fn-ast-body
  (lambda(x)
    ( cases ast-datatype x
       [fn-ast (formals body) body]
       [else (error "Error: problem in fn-ast-formals (not a  fn-ast)")])))

;; fbind-fname ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the fbind-fname and returns the body fname.
(define fbind-fname
  (lambda(x)
    ( cases fbind-datatype x
       [fbind (fname  formals body) fname]
       [else (error "Error: problem in fbind-fname (not a  fbind)")])))

;; fbind-formals ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the fbind-formals and returns the formals ast.
(define fbind-formals
  (lambda(x)
    ( cases fbind-datatype x
       [fbind (fname  formals body) formals]
       [else (error "Error: problem in fbind-formals (not a  fbind)")])))

;; fbind-body ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the fbind-body and returns the body ast.
(define fbind-body
  (lambda(x)
    ( cases fbind-datatype x
       [fbind (fname  formals body) body]
       [else (error "Error: problem in fbind-body (not a  fbind)")])))

;; fix-ast? ast? -> boolean?
;; check whether it is a valid fix-ast or not.
(define fix-ast?
  (lambda(x)
    ( cases ast-datatype x
       [fix-ast ( fbinds body) #t]
       [else #f])))

;; fix-ast-fbinds ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the fix-ast-fbinds and returns the fbinds ast.
(define fix-ast-fbinds
  (lambda(x)
    ( cases ast-datatype x
       [fix-ast ( fbinds body) fbinds]
       [else (error "Error: problem in fix-ast-fbinds (not a  fix-ast)") ])))

;; fix-ast-body ast? -> ast?
;; this works as a extractor i.e. it extracts the value from the fix-ast-body and returns the body ast.
(define fix-ast-body
  (lambda(x)
    ( cases ast-datatype x
       [fix-ast ( fbinds body) body]
       [else (error "Error: problem in fix-ast-body (not a  fix-ast)") ])))

;;it validates the ast datatypes and defines the constructor for the different asts. 
(define-datatype ast-datatype ast?
   [num-ast (datum number?)]
   [bool-ast (datum boolean?)]
   [id-ref-ast  (sym symbol?)] 
   [app-ast (rator ast?) (rands (list-of ast?))]
   [if-ast  (exp1 ast?) (exp2 ast?) (exp3 ast?)] 
   [and-ast  (rands (list-of ast?))]
   [or-ast  (rands (list-of ast?))]
   [let-ast (binds (list-of bind?)) (body ast?)]
   [cond-ast (asts (list-of clause?))]
   [fn-ast (formals (list-of symbol?)) (body ast?)]
   [fix-ast (fbinds (list-of fbind?)) (body ast?)]
   [new-ref-ast (data ast?)]
   [setref-ast (data ast?)(ref ast?)]
   [deref-ast (ref ast?)]
   [seq-ast (ast-seq (list-of ast?))]
  )

;; A datatype which binds the symbols and the ast and it is also helpful in validating the bind datatype.
;; constructor for the bind-datatype
(define-datatype bind-datatype bind?
  [bind (b-id symbol?)  (b-ast ast?)])

;;constructor for the clause-datatype
;; it is needed for creating the clause and also validates the clause. 
(define-datatype clause-datatype clause?
  [clause (test ast?)  (then ast?)])

;;constructor for the fbind-datatype
;; it is needed for creating the fbind and also validates the fbind. 
(define-datatype fbind-datatype fbind?
  [fbind (fname symbol?) (formals (list-of symbol?)) (body ast?)])
  