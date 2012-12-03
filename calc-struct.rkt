#lang typed/racket 

(provide (all-defined-out))

(define-type Expr (U Binop Integer))
(define-type Operator (U '+ '- '/ '*))
(struct: Binop ((op : Operator) (arg0 : Expr) (arg1 : Expr)) #:transparent)
