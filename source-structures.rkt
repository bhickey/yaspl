#lang typed/racket
(provide (all-defined-out))

;; Syntatic forms
;; program structures
(struct: module ((name : Symbol)
                 (imports : (Listof import))
                 (exports : (Listof export))
                 (data : (Listof data))
                 (defn : (Listof defn))) #:transparent)
(struct: program ((imports : (Listof import))
                  (expr : Expression)) #:transparent)

(define-type module-interfaces (HashTable Symbol module-interface))
(struct: module-interface
         ((name : Symbol)
          (exports : (Listof export))))

;; Top level forms
(struct: import ((name : Symbol)) #:transparent)
(struct: export ((name : Symbol)) #:transparent)
(struct: data ((name : Symbol)
               (params : (Listof Symbol))
               (variants : (Listof variant))) #:transparent)
(struct: variant ((name : Symbol) (fields : (Listof Symbol))) #:transparent)
(struct: defn ((name : Symbol) (expr : Expression)) #:transparent)

(define-type Expression (U int str id lam app case))
;; Expressions
(struct: int ((val : Integer)) #:transparent)
(struct: str ((val : String)) #:transparent)
(struct: id ((val : Symbol)) #:transparent)
(struct: lam ((arg : Symbol) (body : Expression)) #:transparent)
(struct: app ((fn : Expression)
              (argument : Expression)) #:transparent)
(struct: case ((expr : Expression)
               (clauses : (Listof clause))) #:transparent)
(struct: clause ((pattern : Pattern)
                 (expr : Expression)) #:transparent)

(define-type Pattern (U number-pattern string-pattern identifier-pattern wildcard-pattern constructor-pattern))
;; Patterns
(struct: number-pattern ((val : Integer)) #:transparent)
(struct: string-pattern ((val : String)) #:transparent)
(struct: identifier-pattern ((sym : Symbol)) #:transparent)
(struct: wildcard-pattern () #:transparent)
(struct: constructor-pattern ((constructor : Symbol) (args : (Listof Pattern))) #:transparent)

;; Other constructors

(: lam* ((Listof Symbol) Expression -> Expression))
(define (lam* args body)
  (foldr lam body args))

(: app* (Expression (Listof Expression) -> Expression))
(define (app* op args)
  (foldl (lambda: ((arg : Expression)
                   (acc : Expression)) (app acc arg))
         op args))


;; Free-variables

(: set-union* (All (a) ((Setof a) * -> (Setof a))))
(define (set-union* . args)
  (if (null? args)
      (set)
      (apply set-union (first args) (rest args))))

(: free-variables (Expression -> (Setof Symbol)))
(define (free-variables expr)
  (match expr
    ((or (int _) (str _)) (set))
    ((id v) (set v))
    ((lam arg body) (set-remove (free-variables body) arg))
    ((app fn arg) (set-union (free-variables fn) (free-variables arg)))
    ((case expr clauses)
     (apply set-union
            (free-variables expr)
            (map clause-free-variables clauses)))))

(: clause-free-variables (clause -> (Setof Symbol)))
(define (clause-free-variables cl)
  (match cl
    ((clause pat body)
     (set-subtract (free-variables body)
                   (bound-variables pat)))))

(: bound-variables (Pattern -> (Setof Symbol)))
(define (bound-variables pat)
  (match pat
   ((or (number-pattern _) (string-pattern _) (wildcard-pattern))
    (set))
   ((identifier-pattern id) (set id))
   ((constructor-pattern name args)
    (apply set-union* (map bound-variables args)))))


(: introduced-bindings ((U data defn variant) -> (Setof Symbol)))
(define (introduced-bindings v)
  (match v
    ((data name params variants)
     (apply set-union (set name) (map introduced-bindings variants)))
    ((defn name expr)
     (set name))
    ((variant name _) (set name))))

(: introduced-bindings/import (import module-interfaces -> (Setof Symbol)))
(define (introduced-bindings/import i interfaces)
  (match i
    ((import import-name)
     (apply set
       (for/list: : (Listof Symbol)
         ((export (module-interface-exports (hash-ref interfaces import-name))))
         (export-name export))))))

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


(: module->module-interface (module -> module-interface))
(define (module->module-interface mod)
  (match mod
    ((module name import exports defn data)
     (module-interface name exports))))

(: modules->module-interfaces ((Listof module) -> module-interfaces))
(define (modules->module-interfaces mods)
  (make-immutable-hash
    (for/list: : (Listof (Pair Symbol module-interface)) ((mod mods))
      (cons (module-name mod) (module->module-interface mod)))))
