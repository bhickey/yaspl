#lang racket

(provide (all-defined-out))

(struct int (val))
(struct string (val))
(struct id (val type))
(struct bind (id expr body))
(struct app-fun (fun-id arg-ids type))
(struct case (id clauses type))
(struct make-closure (fun-id env-id))
(struct closure-env (id))
(struct closure-fun (id))

(struct make-env (values))
(struct env-ref (id index))

(struct clause (pattern expr type))
(struct id-pattern (id))
(struct constructor-pattern (name ids))

#|
(require
  unstable/list
  (prefix-in typed: "typed-structures.rkt"))

(struct: lam (arglist body type))


;; old:expr -> (values lifted:expr Set[(U Symbol Type)])
(define (lift expr)
  (define (rlift (expr) (lift env expr)))
  (match expr
    ((typed:int v) (values (int v) (set)))
    ((typed:str v) (values (str v) (set)))
    ((typed:id v t) (values (id v t) (set (list v t))))
    ((typed:case expr clauses type)
      (define-values (lifted-clauses free-vars) (map2 rlift clauses))
      (values
        (case (rlift expr) (lifted-clauses) type)
        (foldl set-union (set) free-vars)))
    ((typed:clause pattern expr type)
      (define-values (lifted-expr free-vars) (rlift expr))
      (values (clause pattern lifted-expr type) free-vars))
    ((typed:lam args body type)
      (define-values (lifted-body maybe-free-vars) (rlift body))
      (define free-vars (set-subtract maybe-free-var args))
      (values
        (lam (concatenate args free-vars) lifted-body type)
        free-vars))))
|#
