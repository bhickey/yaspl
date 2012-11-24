#lang typed/racket

(provide (all-defined-out))

(struct: expr-constraint ((left : Term) (right : Term)) #:transparent)
(struct: binding-constraint ((id : Symbol) (type : (U ty-abs Type))) #:transparent)


(struct: ty-abs ((names : (Listof Symbol))
                (body : Type)) #:transparent)


(define-type Type (U ty-app const-ty ty-var))
(define-predicate type/c Type)

(struct: ty-app ((op : Type)
                (arg : Type)) #:transparent)
(struct: const-ty ((val : Any)) #:transparent)
(struct: ty-var ((name : Symbol)) #:transparent)


(define-type Term (U unification-term identifier-term const-term app-term))
(define-predicate term/c Term)

(struct: unification-term ((name : Symbol)) #:transparent)
(struct: identifier-term ((id : Symbol)) #:transparent)
(struct: const-term ((val : Any))  #:transparent)
(struct: app-term ((op : Term) (arg : Term))  #:transparent)
