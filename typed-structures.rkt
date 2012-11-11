#lang racket

(require
  (prefix-in src: "source-structures.rkt"))

(provide (all-defined-out))

(struct: int-type ())
(struct: str-type ())
(struct: fn-type (arg-type return-type))

(struct: int (val))
(struct: str (val))
(struct: lex-id (val type))
(struct: mod-id (val type))
(struct: lam (arg body type))
(struct: app (arg body type))
(struct: case (expr clauses type))
(struct: clause (pattern expr type))

(define (typeof expr)
  (match expr
    ((int _) (int-type))
    ((str _) (str-type))
    ((app _ _ t) t)
    ((id _ t) t)
    ((lam _ _ t) t)
    ((case _ _ t) t)
    ((clause _ _ t) t)))

(define (convert env expr)
  (match expr
    ((src:int v) (int v))
    ((src:str v) (str v))
    ((src:id v) (id v (dict-ref env v)))
    ((src:lam arg body) 
      (define arg-type (dict-ref env arg))
      (define typed-body (convert env body))
      (lam arg typed-body (fn-type arg-type (typeof typed-body))))
    ((src:app arg body) (app arg body (convert env body)))
    ((src:case expr clauses)
      (define typed-clauses (map (convert env) clauses))
      (case expr typed-clauses (typeof (first typed-clauses))))
    ((src:clause pattern expr) (clause pattern (convert env expr)))))
  

