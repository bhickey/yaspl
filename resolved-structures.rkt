#lang typed/racket

(require (prefix-in src: "source-structures.rkt"))
(provide (all-defined-out))

(define-type Kind src:Kind)

(define-type module-interfaces (HashTable Symbol module-interface))
(struct: module-interface
         ((name : Symbol)
          (type-exports : (Listof type-export))
          (var-exports : (Listof var-export))
          (pattern-exports : (Listof pattern-export))))


(struct: module
  ((name : Symbol)
   (imports : (Listof Symbol))
   (exports : exports)
   (data : (Listof data))
   (defn : (Listof defn))) #:transparent)

;; Symbol value is the internal renamed symbol
(struct: exports
  ((types : (Listof (List Symbol type-export)))
   (vars : (Listof (List Symbol var-export)))
   (patterns : (Listof (List Symbol pattern-export)))) #:transparent)

(struct: type-export
  ((name : Symbol)
   (value : Type)) #:transparent)

(struct: var-export
  ((name : Symbol)
   (type : Type)) #:transparent)

(struct: pattern-export
  ((name : Symbol)
   (pattern : variant)) #:transparent)




(: type->kind (Type -> Kind))
(define (type->kind ty)
  ;; TODO
  (src:type-kind))

(define-type Type (U type-constructor type-app type-id type-abs))
(define-predicate type/c Type)


(struct: type-abs ((args : (Listof (List Symbol Kind)))
                   (base : Type)) #:transparent)
(struct: type-constructor
  ((module : Symbol)
   (name : Symbol)
   (kind : Kind)) #:transparent)
(struct: type-app
  ((op : Type)
   (arg : Type)) #:transparent)
(struct: type-id ((val : Symbol)
                  (kind : Kind)) #:transparent)

(define fun-type-constructor
  (type-constructor 'prim '-> (src:arr-kind (src:type-kind) (src:arr-kind (src:type-kind) (src:type-kind)))))
(define int-type-constructor
  (type-constructor 'prim 'int (src:type-kind)))
(define string-type-constructor
  (type-constructor 'prim 'sting (src:type-kind)))


(struct: data ((name : Symbol)
               (variants : (Listof variant))) #:transparent)
(struct: variant
  ((name : Symbol)
   (params : (Listof (List Symbol Kind)))
   (args : (Listof Type))
   (type : Type)) #:transparent)


(struct: defn ((name : Symbol) (type : Type) (expr : Expression)) #:transparent)


(define-type Expression (U int str id id toplevel-id lam app case make-variant))
(struct: int ((val : Integer)) #:transparent)
(struct: str ((val : String)) #:transparent)

(struct: id ((val : Symbol) (type : Type)) #:transparent)
(struct: toplevel-id ((module : Symbol) (val : Symbol) (type : Type)) #:transparent)
(struct: inst ((expr : Expression) (types : (Listof Type))) #:transparent)

(struct: lam ((arg : Symbol) (type : Type) (body : Expression)) #:transparent)
(struct: app ((fn : Expression)
              (argument : Expression)
              (type : Type)) #:transparent)
(struct: case ((expr : Expression)
               (clauses : (Listof clause))
               (arg-type : Type)
               (return-type : Type)) #:transparent)
(struct: clause ((pattern : Pattern)
                 (expr : Expression)
                 (pattern-type : Type)
                 (return-type : Type)) #:transparent)
(struct: make-variant ((name : Symbol) (args : (Listof Expression))))

(define-type Pattern (U number-pattern string-pattern identifier-pattern wildcard-pattern constructor-pattern))
;; Patterns
(struct: number-pattern ((val : Integer)) #:transparent)
(struct: string-pattern ((val : String)) #:transparent)
(struct: identifier-pattern ((sym : Symbol)) #:transparent)
(struct: wildcard-pattern () #:transparent)
(struct: constructor-pattern ((variant : variant) (args : (Listof Pattern))) #:transparent)




(: module->module-interface (module -> module-interface))
(define (module->module-interface mod)
  (match (module-exports mod)
    ((exports (list (list _ #{types : (Listof type-export)}) ...)
              (list (list _ #{vars : (Listof var-export)}) ...)
              (list (list _ #{patterns : (Listof pattern-export)}) ...))
     (module-interface (module-name mod) types vars patterns))))

(: modules->module-interfaces ((Listof module) -> module-interfaces))
(define (modules->module-interfaces mods)
  (for/hash: : module-interfaces ((mod mods))
    (values (module-name mod) (module->module-interface mod))))

