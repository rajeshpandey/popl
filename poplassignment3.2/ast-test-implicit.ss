#lang racket
(require rackunit "ast.ss" "parse.ss" "eval-ast.ss" "env.ss" "semantic-domain.ss")

;;unit testing for ast
(check-equal? (ast? (num-ast 5)) #t "number test")
(check-pred ast? (bool-ast #t) "boolean test")
(check-true (ast? (app-ast (id-ref-ast '+) (list (num-ast 5) (num-ast 6)))) "app-ast test")
(check-true (ast? (if-ast (bool-ast #t) (num-ast 5) (num-ast 6))) "if-ast test")
(check-true (ast? (id-ref-ast 'x)) "id-ref test")
(check-true (ast? (let-ast (list (bind 'x (num-ast 5))) (app-ast (id-ref-ast '+) (list (num-ast 5) (id-ref-ast 'x))))) "let-ast test")
(check-true (ast? (fn-ast (list 'x) (app-ast (id-ref-ast '+) (list (num-ast 5) (id-ref-ast 'x))))) "fn-ast test")
(check-true (ast? (app-ast (id-ref-ast '+) (list (num-ast 5) (id-ref-ast 'x)))) "app-ast test")
(check-true (ast? (seq-ast (list (app-ast (id-ref-ast '+) (list (num-ast 5) (id-ref-ast 'x))) (num-ast 5)))) "seq-ast test")
(check-true (ast? (set-ast 'x (num-ast 5))) "num-ast test")

;;; the app-ast ast builder  doesn't do type checking
(check-equal? (ast? (app-ast (id-ref-ast '+) (list (num-ast 5) (bool-ast #t)))) #t "app-ast test2")
(check-equal? (ast? (if-ast (bool-ast #t) (num-ast 5) (num-ast 6))) #t "if-app test3")

;;unit testing for parse
(check-equal? (num-ast 5)   (parse 5) "parse5")
(check-equal? (bool-ast #t) (parse #t) "parse#t")
(check-equal? (id-ref-ast 'sym) (parse 'sym) "parse 'sym")
(check-equal? (if-ast (bool-ast #t) (bool-ast #f) (bool-ast #t)) (parse '(if #t #f #t)) "if-app error")
(check-equal? (translate-ast (let-ast (list (bind 'x (num-ast 5))) (app-ast (id-ref-ast '+) (list (num-ast 2) (id-ref-ast 'x)))))
              (parse '(let ([x 5]) (+ 2 x))) "let-ast test")
(check-equal? (translate-ast (let-ast (list (bind 'x (num-ast 5))) (app-ast (id-ref-ast '+) (list (num-ast 2) (id-ref-ast 'x)))))
              (parse '((fn (x) (+ 2 x)) 5)) "let-fn-test")
(check-equal? (fn-ast (list 'x) (app-ast (id-ref-ast '+) (list (num-ast 2) (id-ref-ast 'x))))
              (parse '(fn (x) (+ 2 x))) "fn test")
(check-equal? (app-ast (id-ref-ast '+) (list (num-ast 3)))
              (parse '(+ 3)) "parse+")
(check-equal? (app-ast (id-ref-ast '/) (list (num-ast 3) (bool-ast #t)))
              (parse '(/ 3 #t)) "parse/")
(check-equal? (parse '(fix ([(! n) (cond [(0? n) 1] [else (* n (! (sub1 n)))])]) (! 3)))
              (fix-ast (list
                        (fbind
                         '!
                         '(n)
                         (if-ast
                          (app-ast (id-ref-ast '0?) (list (id-ref-ast 'n)))
                          (num-ast 1)
                          (app-ast
                           (id-ref-ast '*)
                           (list (id-ref-ast 'n) (app-ast (id-ref-ast '!) (list (app-ast (id-ref-ast 'sub1) (list (id-ref-ast 'n))))))))))
                       (app-ast (id-ref-ast '!) (list (num-ast 3)))))
(check-equal? (parse '(and #t #t #t))
              (if-ast (bool-ast #t) (if-ast (bool-ast #t) (if-ast (bool-ast #t) (bool-ast #t) (bool-ast #f)) (bool-ast #f)) (bool-ast #f))
              "and-test -- and has been translated to if-ast")

(check-equal? (parse '(let ([x 5]) (+ 2 x)))
              (translate-ast (let-ast 
                          (list (bind 'x (num-ast 5))) 
                          (app-ast (id-ref-ast '+) (list (num-ast 2) (id-ref-ast 'x)))))
              "let test")

;; all the below mentioned test cases may be written using eval-ast as well.
;;unit testing for run
;(define e)
(check-equal? (run 5) 5 "number test")
(check-equal? (run #t) #t "boolean test")
(check-equal? (run '(+ 5 7) ) 12 "eval-ast: app-ast-test")
(check-equal? (run '(let ([x 5]) (+ 2 x)) ) 7 "let-ast test")
(check-equal? (run '((fn (x) (+ 2 x)) 5) ) 7 "fn and app test")
(check-equal? (run '(fix ([(! n) (if (= n 0) 1 (* n (! (- n 1))))]) (! 6)) ) 720 "recursion test")
(check-equal? (run '(and #t #t #t) ) #t "and test")
(check-equal? (run '(or #f #t #t) ) #t "or test")
(check-equal? (run '(let ((x (+ 2 3 4)))
                            (let ((f (fn (y) (- (- x 1) 1))))
                                   (f 5))) )
              7 "let test")
(check-equal? (run '(fix ([(even? n) (if (= 0 n) #t (odd? (- n 1)))]
                             [(odd? n) (if (= 0 n) #f (even? (- n 1)))])
                            (odd? 2001)) ) #t "recfun - test")
(check-equal? (run '(fix ([(! n) (if (= 0 n) 1 (* n (! (- n 1))))])
                            (! 6)) ) 720 "recfun test")
(check-equal? (run '(let ([x 5]
                             [y 6])
                            (+ x y)) ) 11 "new and deref test")
(check-equal? (run '(let ([x 5])
                            (seq (set x 7)
                                x)) ) 7 "set-ref test")



;; unit testing for exceptions in run
(check-exn exn? (lambda () (run '(if 4 5 6) )) "if-app-test: invalid arguments to constructor if-ast")
(check-exn exn? (lambda () (run '($ 5 7) )) "run: incorrect-operator")
(check-exn exn? (lambda () (run '(/ 5 0) )) "/: division by 0")
(check-exn exn? (lambda () (run '(let ([x 5]) (+ 2 y)) ) "error in let. y is not defined in the environment"))
(check-exn exn? (lambda () (run '(and 5 #t) )) "and: invalid type of arguments")
(check-exn exn? (lambda () (run '(or 5 #t) )) "or: invalid type of arguments")
(check-exn exn? (lambda () (run '(cond [5 6]) )) "cond: invalid type of arguments")
