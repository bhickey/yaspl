#lang typed/racket

(provide (all-defined-out))

(struct: module
  ((funs : (HashTable Symbol function))
   (defns : (HashTable Symbol mod-function))) #:transparent)


(struct: function ((args : (Listof Symbol)) (body : Expr)) #:transparent)
(struct: mod-function ((fun : Symbol)) #:transparent)

(define-type Type #f)
(define-type Expr (U bind unpack case int str id toplevel-id inst app-fun pack make-tuple tuple-ref))

(struct: bind ((id : Symbol) (expr : Expr) (body : Expr)) #:transparent)
(struct: unpack ((type-id : Symbol) (new-val-id : Symbol) (orig-val-id : Symbol) (body : Expr)) #:transparent)
(struct: case ((id : Symbol) (clauses : (Listof clause))) #:transparent)

(struct: int ((val : Integer)) #:transparent)
(struct: str ((val : String)) #:transparent)
(struct: id ((val : Symbol)) #:transparent)
(struct: toplevel-id ((val : Symbol)) #:transparent)
(struct: inst ((id : Symbol) (type : Type)) #:transparent)
(struct: app-fun ((fun-id : Symbol) (arg-ids : (Listof Symbol))) #:transparent)
(struct: pack ((id : Symbol) (type-id : Symbol) (inner-type : Type) (outer-type : Type)) #:transparent)

(struct: make-tuple ((values : (Listof Symbol))) #:transparent)
(struct: tuple-ref ((id : Symbol) (index : Natural)) #:transparent)

(struct: clause ((pattern : Pattern) (expr : Expr)) #:transparent)
(define-type Pattern (U id-pattern constructor-pattern))
(struct: id-pattern ((id : Symbol)) #:transparent)
(struct: constructor-pattern ((name : Symbol) (ids : (Listof Symbol))) #:transparent)

