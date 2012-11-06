#lang typed/racket/base

(require racket/match racket/set)

(provide (all-defined-out))


(struct: (binder expr) case-clause
         ((pattern : (Pattern binder))
          (body : expr)) #:transparent)


(define-type (Pattern binder)
 (Rec Pattern
  (U nobind-pattern
     (identifier-pattern binder)
     (constructor-pattern binder Pattern))))

(struct: nobind-pattern () #:transparent)
(struct: (binder) identifier-pattern ((name : binder)) #:transparent)
(struct: (binder Pattern) constructor-pattern
         ((pattern  : binder)
          (tag : Symbol)
          (field-patterns : (Listof Pattern))) #:transparent)


(: pattern-bound-variables (All (binder) (Pattern binder) -> (Setof binder)))
(define (pattern-bound-variables pattern)
 (: recur (All (binder) (Pattern binder) -> (Setof binder)))
 (define (recur pattern)
  (match pattern
   ((nobind-pattern) (set))
   ((identifier-pattern name) (set name))
   ((constructor-pattern pattern tag patterns)
    (apply set-union ((inst set binder))
           ((inst map (Setof binder) (Pattern binder)) recur patterns)))))
 (recur pattern))
