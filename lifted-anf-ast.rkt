#lang racket

(provide (all-defined-out))

(struct program (datadefs top-levels functions) #:transparent)

(struct existential-type (name body) #:transparent)
(struct universal-type (type-args value-types body) #:transparent)
(struct product-type (types) #:transparent)
(struct structure-type (datatype args) #:transparent)
(struct identifier-type (name) #:transparent)

(struct datatype-definition (name type-args variants) #:transparent)
(struct variant-definition (name fields) #:transparent)

(struct function (args return-type body) #:transparent)
(struct argument (name type) #:transparent)

(struct unpack-expr (type-name val-name value body) #:transparent)
(struct pack-expr (hidden-type type value) #:transparent)
(struct bind-expr (name bound body) #:transparent)
(struct app-expr (fun type-args val-args) #:transparent)
(struct identifier-expr (name) #:transparent)
(struct tuple-proj-expr (index value) #:transparent)
(struct tuple-expr (values) #:transparent)
(struct constructor-expr (variant values) #:transparent)
(struct case-expr (value clauses) #:transparent)

(struct case-clause (pattern body) #:transparent)
(struct nobind-pattern () #:transparent)
(struct identifier-pattern (name) #:transparent)
(struct constructor-pattern (variant field-patterns) #:transparent)

(define (fresh-name (hint 'fresh))
 (cond
  ((symbol? hint) (gensym hint))
  ((string? hint) (gensym hint))
  ((identifier-expr? hint) (gensym (identifier-expr-name hint)))))

