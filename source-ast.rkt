#lang typed/racket

(require "case-clause.rkt")
(require "datatypes.rkt")
(require "exports.rkt")
(require "names.rkt")

(provide (all-defined-out))
(provide (all-from-out "case-clause.rkt"))
(provide (all-from-out "datatypes.rkt"))
(provide (all-from-out "exports.rkt"))

(struct: (binding expr) module
         ((id : Symbol)
          (imports : (Listof module-import))
          (exports : (Listof (Export binding #f)))
          (forms : (Listof expr))) #:transparent)

(struct: module-signature
         ((exports : (Listof (Export source-name #f))))
         #:transparent)


(struct: module-import ((name : Symbol)) #:transparent)


;(struct abstract-type () #:transparent)
;(struct arrow-type (arg result) #:transparent)
;(struct forall-type (name body) #:transparent)
;(struct variable-type (name) #:transparent)
;(struct structure-type (datatype args) #:transparent)

(struct: (binder) variable-declaration
         ((name : binder) (type : #f)) #:transparent)
(struct: (binder expr) variable-definition
         ((name : binder) (expr : expr)) #:transparent)

(struct: (binder expr) case-expr
         ((expr : expr)
          (clauses : (Listof (case-clause binder expr)))) #:transparent)
(struct: (expr) app-expr
         ((fun : expr)
          (arg : expr)) #:transparent)
(struct: (binder) identifier-expr
         ((name : binder)) #:transparent)
(struct: (binder expr) lambda-expr
         ((name : binder)
          (body : expr)) #:transparent)


(struct: (binding expr) program
         ((modules : (HashTable Symbol (module binding expr)))
          (main-module-name : Symbol)
          (expr : expr)))
