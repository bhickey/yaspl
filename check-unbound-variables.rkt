#lang typed/racket

(: introduced-bindings ((U data defn variant) -> (Setof Symbol)))
(define (introduced-bindings v)
  (match v
    ((data name params variants)
     (apply set-union* (map introduced-bindings variants)))
    ((defn name expr)
     (set name))
    ((variant name _) (set name))))

(: introduced-bindings/import (import module-interfaces -> (Setof Symbol)))
(define (introduced-bindings/import i interfaces)
  (match i
    ((import import-name)
     (apply set
       (for/list: : (Listof Symbol)
         ((export (module-interface-var-exports (hash-ref interfaces import-name))))
         (var-export-name export))))))

(: check-unbound-variables! ((U module program) module-interfaces -> Void))
(define (check-unbound-variables! v interfaces)
  (match v
    ((module name imports exports data defns)
     (define bound-names
       (apply set-union*
              (append
                (for/list: : (Listof (Setof Symbol))
                  ((import imports))
                  (introduced-bindings/import import interfaces))
                (map introduced-bindings (append data defns)))))
     (for ((defn defns))
       (check-unbound-variables/expr! (defn-expr defn) bound-names)))
    ((program imports body)
     (define bound-names (apply set-union* (map (lambda: ((import : import))
                                                   (introduced-bindings/import import interfaces)) imports)))
     (check-unbound-variables/expr! body bound-names))))

(: check-unbound-variables/expr! (Expression (Setof Symbol) -> Void))
(define (check-unbound-variables/expr! expr symbols)
  (define free-vars (free-variables expr))
  (define unbound-vars (set-subtract free-vars symbols))
  (when (not (set-empty? unbound-vars))
    (error 'check-unbound-variables "Unbound variables: ~a" (set->list unbound-vars))))


