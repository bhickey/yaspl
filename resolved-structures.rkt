#lang typed/racket

(require (prefix-in src: "source-structures.rkt"))
(provide (all-defined-out))

(define-type Kind src:Kind)

(define-type module-interfaces (HashTable Symbol module-interface))
(struct: module-interface
         ((name : Symbol)
          (type-exports : (Listof type-export))
          (var-exports : (Listof var-export))))


(struct: module
  ((name : Symbol)
   (imports : (Listof Symbol))
   (exports : exports)
   (data : (Listof data))
   (defn : (Listof defn))) #:transparent)

(struct: exports
  ((types : (Listof (List Symbol type-export)))
   (vars : (Listof (List Symbol var-export)))) #:transparent)

(struct: type-export
  ((name : Symbol)
   (kind : Kind)) #:transparent)
(struct: var-export
  ((name : Symbol)
   (type : type-scheme)) #:transparent)

(struct: type-scheme ((args : (Listof (List Symbol Kind)))
                      (base : Type)) #:transparent)

(define-type Type (U type-constructor type-app type-id))


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
               (params : (Listof (List Symbol Kind)))
               (variants : (Listof variant))) #:transparent)
(struct: variant ((name : Symbol) (fields : (Listof Type))) #:transparent)

(struct: defn ((name : Symbol) (type : type-scheme) (expr : Expression)) #:transparent)


(define-type Expression (U int str id id lam app case))
(struct: int ((val : Integer)) #:transparent)
(struct: str ((val : String)) #:transparent)

(struct: id ((val : Symbol) (type : Type)))
(struct: inst ((expr : Expression) (types : (Listof Type))))

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

(define-type Pattern (U number-pattern string-pattern identifier-pattern wildcard-pattern constructor-pattern))
;; Patterns
(struct: number-pattern ((val : Integer)) #:transparent)
(struct: string-pattern ((val : String)) #:transparent)
(struct: identifier-pattern ((sym : Symbol)) #:transparent)
(struct: wildcard-pattern () #:transparent)
(struct: constructor-pattern ((constructor : Symbol) (args : (Listof Pattern))) #:transparent)




(: module->module-interface (module -> module-interface))
(define (module->module-interface mod)
  (match (module-exports mod)
    ((exports (list (list _ #{types : (Listof type-export)}) ...)
              (list (list _ #{vars : (Listof var-export)}) ...))
     (module-interface (module-name mod) types vars))))

(: modules->module-interfaces ((Listof module) -> module-interfaces))
(define (modules->module-interfaces mods)
  (make-immutable-hash
    (for/list: : (Listof (Pair Symbol module-interface)) ((mod mods))
      (cons (module-name mod) (module->module-interface mod)))))

