#lang racket

(require racket/require)
(require "source-ast.rkt")
(require "names.rkt")
(require "unique.rkt")

(provide (all-from-out "source-ast.rkt")
         (all-defined-out))



(struct closure-expr (name type captured body) #:transparent)
(struct closure-def (name type closed-args body) #:transparent)
(struct create-closure (def args) #:transparent)
(struct constructor-expr (tag args) #:transparent)
(struct letrecur (bindings body) #:transparent)
(struct bind (binding body) #:transparent)
(struct binding (name body) #:transparent)

(struct lifted-module
        (import exports data-definitions var-definitions functions) #:transparent)

  
