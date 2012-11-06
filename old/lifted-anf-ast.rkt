#lang racket

(require "case-clause.rkt")
(require "datatypes.rkt")
(require "exports.rkt")

(provide (all-defined-out))
(provide (all-from-out "case-clause.rkt"))
(provide (all-from-out "datatypes.rkt"))
(provide (all-from-out "exports.rkt"))

(struct program (modules main-module))
(struct module (exports datadefs top-levels functions) #:transparent)

(struct existential-type (name body) #:transparent)
(struct universal-type (type-args value-types body) #:transparent)
(struct product-type (types) #:transparent)
(struct structure-type (datatype args) #:transparent)
(struct identifier-type (name) #:transparent)


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

