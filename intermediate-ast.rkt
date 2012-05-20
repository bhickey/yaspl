#lang racket

(require racket/require)
(require "source-ast.rkt")

(provide (all-from-out "source-ast.rkt")
         (all-defined-out))



(struct closure-expr (name type captured body) #:transparent)
(struct closure-def (name type closed-args body) #:transparent)
(struct create-closure (def args) #:transparent)
(struct constructor-expr (variant args) #:transparent)
(struct letrecur (bindings body) #:transparent)
(struct bind (binding body) #:transparent)
(struct binding (name body) #:transparent)

(struct lifted-module
        (import exports data-definitions var-definitions functions) #:transparent)

(define (fresh-name (hint 'fresh))
 (cond
  ((symbol? hint) (gensym hint))
  ((string? hint) (gensym hint))
  ((identifier-expr? hint) (gensym (identifier-expr-name hint)))))
  
