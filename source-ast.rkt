#lang racket

(provide (all-defined-out))

(struct module (imports exports forms) #:transparent)

(struct module-signature (exports) #:transparent)

(struct type-export (value) #:transparent)
(struct pattern-export (value) #:transparent)
(struct value-export (value) #:transparent)

(struct module-import (name prefix) #:transparent)

(struct datatype (name args variants) #:transparent)
(struct variant (name datatype fields) #:mutable #:transparent)

(struct abstract-type () #:transparent)
(struct arrow-type (arg result) #:transparent)
(struct forall-type (name body) #:transparent)
(struct variable-type (name) #:transparent)
(struct structure-type (datatype args) #:transparent)

(struct datatype-definition (name args variants) #:transparent)
(struct variant-definition (name fields) #:transparent)
(struct variable-declaration (name type) #:transparent)
(struct variable-definition (name expr) #:transparent)

(struct case-expr (expr clauses) #:transparent)
(struct case-clause (pattern body) #:transparent)
(struct nobind-pattern () #:transparent)
(struct identifier-pattern (name) #:transparent)
(struct constructor-pattern (variant field-patterns) #:transparent)
(struct app-expr (fun arg) #:transparent)
(struct identifier-expr (name) #:transparent)
(struct lambda-expr (name body) #:transparent)



(struct program (modules main-module-name expr))
