#lang typed/racket

(provide (all-defined-out))

(define-type Type #f)
(define-type Expr (U bind un-pack case int str id inst app-fun make-tuple tuple-ref))

(struct: bind ((id : Symbol) (expr : Expr) (body : Expr)))
(struct: un-pack ((type-id : Symbol) (new-val-id : Symbol) (orig-val-id : Symbol) (body : Expr)))
(struct: case ((id : Symbol) (clauses : (Listof clause))))

(struct: int ((val : Integer)))
(struct: str ((val : String)))
(struct: id ((val : Symbol)))
(struct: inst ((id : Symbol) (type : Type)))
(struct: app-fun ((fun-id : Symbol) (arg-ids : (Listof Symbol))))
(struct: pack ((id : Symbol) (type-id : Symbol) (inner-type : Type) (outer-type : Type)))

(struct: make-tuple ((values : (Listof Symbol))))
(struct: tuple-ref ((id : Symbol) (index : Natural)))

(struct: clause ((pattern : Pattern) (expr : Expr)))
(define-type Pattern (U id-pattern constructor-pattern))
(struct: id-pattern ((id : Symbol)))
(struct: constructor-pattern ((name : Symbol) (ids : (Listof Symbol))))

