#lang racket
(provide (all-defined-out))
(require eopl "semantic-domain.ss")

;; this is for defining the list-of storeable values.
(define store? 
  (list-of storable-value?))

;;ref?: store? ->nat? -> boolean?
;; this is for checking for the valid reference of a variable in some store.
(define ref?
  (lambda(s i)
    (and (number? i) (< i (length s)))))

;;new-ref: store?,any/c? -> list?
;;this is for creating the new-ref .it will return the new store and the new reference.
(define new-ref
  (lambda(s v)
   (list (cons v s) (length s))
    )
  )

;;deref: store?,nat? -> any/c?
;; for dereferencing the value for the given reference. 
(define deref
  (lambda (s r)
    (cond
      [(and (ref? s r)) (let ([n (- (length s) r 1)]) (list-ref s n))]
      [(error("Error: deref. not a valid refernce in store(out of range) "))])
    )
  )

;;set-ref: store?,nat?, nat? -> store?
;; for setting  the value for a particular store. 
(define set-ref
  (lambda (s r v)
    (cond
      [(and (ref? s r))(let ([n (- (length s) r 1)]) (replace s n v))]
      [ (error("Error: set-ref "))])))


;;replace: store?,nat? , nat? -> list?
;; this is an auxiliary function for adding the values to a given reference.
(define replace
  (lambda (s i v)
    (cond
      [(zero? i)(cons v (rest s))]
      [ (cons (first s) ( replace s (- i 1) (rest s)))])))

;(define s '())
;(define s1 (new-ref s 5))
;(define s2 (new-ref (first s1) 10))
;(deref (first s2) 1)
;(deref (first s2) 0)
;(set-ref (first s2) 1 12)
