#lang racket

(require "case-clause.rkt")
(require "datatypes.rkt")
(require "exports.rkt")

(provide (all-defined-out))
(provide (all-from-out "case-clause.rkt"))
(provide (all-from-out "datatypes.rkt"))
(provide (all-from-out "exports.rkt"))

(struct module (id imports exports forms) #:transparent)

(struct module-signature (exports) #:transparent)


(struct module-import (name) #:transparent)


(struct abstract-type () #:transparent)
(struct arrow-type (arg result) #:transparent)
(struct forall-type (name body) #:transparent)
(struct variable-type (name) #:transparent)
(struct structure-type (datatype args) #:transparent)

(struct variable-declaration (name type) #:transparent)
(struct variable-definition (name expr) #:transparent)

(struct case-expr (expr clauses) #:transparent)
(struct app-expr (fun arg) #:transparent)
(struct identifier-expr (name) #:transparent)
(struct lambda-expr (name body) #:transparent)



(struct program (modules main-module-name expr))
