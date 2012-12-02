#lang typed/racket

(require
  "unique.rkt"
  "hash.rkt"
  "linearize-modules.rkt"
  (prefix-in src: "source-structures.rkt")
  (prefix-in res: "resolved-structures.rkt"))

(require/typed "type-inference.rkt"
  (infer-types ((Listof src:defn) (HashTable Symbol res:type-scheme) -> (HashTable Symbol res:Type))))


(provide resolve-module resolve-modules)

(: hash-value-map (All (a b c) ((b -> c) (HashTable a b) -> (HashTable a c))))
(define (hash-value-map fn hash)
  (make-immutable-hash
    (hash-map hash (lambda: ((k : a) (v : b)) (cons k (fn v))))))

;; Makes all identifiers bound locally within a definition unique
(: unique-defn (src:defn -> src:defn))
(define (unique-defn defn)
  (define-type Env (HashTable Symbol Symbol))

  (: unique-expr (src:Expression Env -> src:Expression))
  (define (unique-expr expr env)
    (match expr
      ((src:int v) (src:int v))
      ((src:str v) (src:str v))
      ((src:id sym) (src:id (hash-ref env sym (lambda () sym))))
      ((src:lam arg body)
       (define new-arg (unique arg))
       (src:lam new-arg (unique-expr body (hash-set env arg new-arg))))
      ((src:app fn arg)
       (src:app (unique-expr fn env) (unique-expr arg env)))
      ((src:case expr clauses)
       (src:case (unique-expr expr env) (map (unique-clause env) clauses)))))

  (: unique-clause (Env -> (src:clause -> src:clause)))
  (define ((unique-clause env) clause)
    (match clause
      ((src:clause pattern expr)
       (define-values (new-pattern new-env) (unique-pattern pattern env))
       (src:clause new-pattern (unique-expr expr new-env)))))

  (: unique-pattern (src:Pattern Env -> (values src:Pattern Env)))
  (define (unique-pattern pattern env)
    (match pattern
      ((src:number-pattern v)
       (values (src:number-pattern v) env))
      ((src:string-pattern v) 
       (values (src:string-pattern v) env))
      ((src:identifier-pattern id)
       (define new-id (unique id))
       (values (src:identifier-pattern new-id)
               (hash-set env id new-id)))
      ((src:wildcard-pattern)
       (values (src:wildcard-pattern) env))
      ((src:constructor-pattern name args)
       (define-values (new-args new-env)
         (for/fold: ((new-args : (Listof src:Pattern) null)
                     (env : (HashTable Symbol Symbol) env))
                    ((arg : src:Pattern args))
           (define-values (new-arg new-env)
             (unique-pattern arg env))
           (values (cons new-arg new-args) new-env)))
       (values (src:constructor-pattern name (reverse new-args))
               new-env))))

  (match defn
    ((src:defn name type expr)
     (src:defn name type (unique-expr expr (make-immutable-hash null))))))

(: rename-type (src:Type (HashTable Symbol Symbol) -> src:Type))
(define (rename-type ty env)
  (match ty
    ((src:int-ty) ty)
    ((src:string-ty) ty)
    ((src:fun-ty arg res) (src:fun-ty (rename-type arg env) (rename-type res env)))
    ((src:ty-app op arg) (src:ty-app (rename-type op env) (rename-type arg env)))
    ((src:id-ty v) (src:id-ty (hash-ref env v (lambda () v))))))


(: bogus-type-scheme res:type-scheme)
(define bogus-type-scheme
  (res:type-scheme null (res:type-id 'bogus (src:type-kind))))

(: bogus-type res:Type)
(define bogus-type
  (res:type-id 'bogus (src:type-kind)))

(: rename (Symbol -> (values Symbol Symbol)))
(define (rename v) (values v (unique v)))


(: convert-type (src:Type (HashTable Symbol res:type-constructor) (HashTable Symbol src:Kind) -> res:Type))
(define (convert-type ty types env)
  (match ty
    ((src:int-ty) res:int-type-constructor)
    ((src:string-ty) res:string-type-constructor)
    ((src:ty-app op arg)
     (res:type-app (convert-type op types env) (convert-type arg types env)))
    ((src:fun-ty arg res)
     (res:type-app
       (res:type-app res:fun-type-constructor
                   (convert-type arg types env))
       (convert-type res types env)))
    ((src:id-ty val)
     (hash-ref types val
          (lambda () (res:type-id val (hash-ref env val)))))))


(: convert-data ((HashTable Symbol res:type-constructor)
                 (Listof src:data) -> 
                 (values 
                   (HashTable Symbol Symbol)
                   (Listof res:data))))
(define (convert-data types datas)

  (: data-var-syms (HashTable Symbol Symbol))
  (define data-var-syms
    (for*/hash: : (HashTable Symbol Symbol)
        ((data datas)
         (variant (src:data-variants data)))
      (rename (src:variant-name variant))))

  (: new-datas (Listof res:data))
  (define new-datas
    (for/list: ((data : src:data datas))
      (match-define (src:data name params variants) data)
      (define param-names (map (inst first Symbol Any) params))
      (define param-kinds (map (inst second Symbol src:Kind Null) params))
      (define type (hash-ref types name))

      (define (resolve-variant variant)
        (match variant
          ((src:variant var-name fields)
           (define new-param-names (map unique param-names))
           (define new-params-map
             (make-immutable-hash (map (inst cons Symbol Symbol) param-names new-param-names)))
           (define env
             (for/hash: : (HashTable Symbol src:Kind)
                        ((new-param-name new-param-names)
                         (param-kind param-kinds))
               (values new-param-name param-kind)))

           (: convert-field (src:Type -> res:Type))
           (define (convert-field ty)
             (convert-type (rename-type ty new-params-map) types env))

           (res:variant
             (hash-ref data-var-syms var-name)
             new-param-names
             (map convert-field fields)
             (for/fold:
                 ((ty : res:Type type))
                 ((param new-param-names)
                  (kind param-kinds))
               (res:type-app ty (res:type-id param kind)))))))

      (res:data
        (res:type-constructor-name type)
        (map resolve-variant variants))))

  (values
    data-var-syms
    new-datas))


(: resolve-module (src:module res:module-interfaces -> res:module))
(define (resolve-module src-module module-interfaces)
  (match-define (src:module module-name imports exports datas orig-defns) src-module)
  (define defns (map unique-defn orig-defns))


  (: imported-interfaces (Listof res:module-interface))
  (define imported-interfaces
    (for/list ((import imports))
      (hash-ref module-interfaces (src:import-name import))))

  (: imported-var-ids (HashTable Symbol res:toplevel-id))
  (define imported-var-ids
    (for*/hash: : (HashTable Symbol res:toplevel-id)
        ((interface imported-interfaces)
         (var (res:module-interface-var-exports interface)))
      (match-define (res:var-export var-name var-type) var)
      ;;TODO let variables have polymorphic types
      (values var-name
              (res:toplevel-id
                (res:module-interface-name interface)
                var-name
                bogus-type))))

  (: imported-types (HashTable Symbol res:type-constructor))
  (define imported-types
    (for*/hash: : (HashTable Symbol res:type-constructor)
        ((interface imported-interfaces)
         (type (res:module-interface-type-exports interface)))
      (define mod-name (res:module-interface-name interface))
      (match-define (res:type-export type-name value) type)
      (values type-name (res:type-constructor mod-name type-name (res:type->kind value)))))

  (: imported-patterns (HashTable Symbol res:variant))
  (define imported-patterns
    (for*/hash: : (HashTable Symbol res:variant)
        ((interface imported-interfaces)
         (export (res:module-interface-pattern-exports interface)))
      (match-define (res:pattern-export name desc) export)
      (values name desc)))


  (: defined-syms (HashTable Symbol Symbol))
  (define defined-syms
    (for*/hash: : (HashTable Symbol Symbol)
        ((defn defns))
       (rename (src:defn-name defn))))


  (: data-type-types (HashTable Symbol res:type-constructor))
  (define data-type-types
    (for/hash: : (HashTable Symbol res:type-constructor)
        ((data datas))
      (match-define (src:data name params _) data)
      (values
        name
        (res:type-constructor
          module-name
          (unique name)
          (for/fold: ((kind : src:Kind (src:type-kind)))
                     ((arg : (List Symbol src:Kind) (reverse params)))
            (src:arr-kind (second arg) kind))))))

  (: module-type-ids (HashTable Symbol res:type-constructor))
  (define module-type-ids 
    (hash-union imported-types data-type-types))

  (: data-var-syms (HashTable Symbol Symbol))
  (: new-datas (Listof res:data))
  (define-values (data-var-syms new-datas) (convert-data module-type-ids datas))

  (: data-variants (HashTable Symbol res:variant))
  (define data-variants
    (for*/hash: : (HashTable Symbol res:variant)
        ((data new-datas)
         (variant (res:data-variants data)))
      (values (res:variant-name variant) variant)))




  (: defined-var-type-schemes (HashTable Symbol res:type-scheme))
  (define defined-var-type-schemes
    (let ()
      (for/hash: : (HashTable Symbol res:type-scheme) ((defn defns))
        (match-define (src:type-scheme params type) (src:defn-type defn))
        (values (src:defn-name defn)
                (res:type-scheme params
                  (convert-type
                    type
                    module-type-ids
                    (for/hash: : (HashTable Symbol src:Kind)
                        ((param : (List Symbol src:Kind) params))
                      (values (first param) (second param)))))))))


  ;; TODO clean up with extracting from the variants
  (: data-var-type-schemes (HashTable Symbol res:type-scheme))
  (define data-var-type-schemes
    (apply hash-union (ann (make-immutable-hash empty) (HashTable Symbol res:type-scheme))
      (for/list: : (Listof (HashTable Symbol res:type-scheme))
          ((data : src:data datas))
        (match-define (src:data name params variants) data)
        (define param-names (map (inst first Symbol Any) params))
        (define param-kinds (map (inst second Symbol src:Kind Null) params))
        (define new-param-names (map unique param-names))
        (define new-params-map
          (make-immutable-hash (map (inst cons Symbol Symbol) param-names new-param-names)))
        (define new-params
          (for/list: : (Listof (List Symbol src:Kind))
              ((name new-param-names)
               (kind param-kinds))
            (list name kind)))

        (define new-env
          (for/hash: : (HashTable Symbol src:Kind)
                     ((new-param-name new-param-names)
                      (param-kind param-kinds))
            (values new-param-name param-kind)))
        (define return-type
          (for/fold: ((t : res:Type (hash-ref module-type-ids name)))
                     ((param-kind : src:Kind  param-kinds)
                      (new-param-name : Symbol new-param-names))
            (res:type-app t (res:type-id new-param-name param-kind))))

        (define (convert-variant variant)
          (match variant
            ((src:variant _ fields)
             (res:type-scheme
               new-params
               (for/fold: ((t : res:Type return-type))
                          ((field : src:Type (reverse fields)))
                 (res:type-app 
                   (res:type-app res:fun-type-constructor
                     (convert-type (rename-type field new-params-map) module-type-ids new-env))
                     t))))))

        (make-immutable-hash
          (map (inst cons Symbol res:type-scheme)
               (map src:variant-name variants)
               (map convert-variant variants))))))

  (: module-var-ids (HashTable Symbol (U res:id res:toplevel-id)))
  (define module-var-ids 
    (let ((inject (lambda: ((sym : Symbol)) (ann (res:toplevel-id module-name sym bogus-type) (U res:id res:toplevel-id)))))
      ;; TODO fix when TR doesn't suck so much
      (hash-union (hash-value-map (lambda: ((id : res:toplevel-id)) (ann id (U res:id res:toplevel-id)))
                                  imported-var-ids)
                  (hash-value-map inject defined-syms)
                  (hash-value-map inject data-var-syms))))



  (: substitution (HashTable Symbol res:Type))
  (define substitution
    (infer-types defns
                 (hash-union
                   ;; TODO fix when TR doesn't suck so much
                   (cast defined-var-type-schemes (HashTable Symbol res:type-scheme))
                   (cast data-var-type-schemes (HashTable Symbol res:type-scheme)))))


  (: resolve-expr (src:Expression -> res:Expression))
  (define (resolve-expr expr)
    (define-type Env (HashTable Symbol (U res:id res:toplevel-id)))
    (: resolve-expr (src:Expression Env -> res:Expression))
    (define (resolve-expr expr env)
      (match expr
        ((src:int v) (res:int v))
        ((src:str v) (res:str v))
        ((src:id sym) (hash-ref env sym))
        ((src:lam arg body)
         (define new-arg (unique arg))
         (res:lam new-arg bogus-type (resolve-expr body (hash-set env arg (res:id new-arg bogus-type)))))
        ((src:app fn arg)
         (res:app (resolve-expr fn env) (resolve-expr arg env) bogus-type))
        ((src:case expr clauses)
         (res:case (resolve-expr expr env) (map (resolve-clause env) clauses)
                   bogus-type
                   bogus-type))))

    (: resolve-clause (Env -> (src:clause -> res:clause)))
    (define ((resolve-clause env) clause)
      (match clause
        ((src:clause pattern expr)
         (define-values (new-pattern new-env) (resolve-pattern pattern env))
         (res:clause new-pattern (resolve-expr expr new-env) bogus-type bogus-type))))
    (: resolve-pattern (src:Pattern Env -> (values res:Pattern Env)))
    (define (resolve-pattern pattern env)
      (match pattern
        ((src:number-pattern v)
         (values (res:number-pattern v) env))
        ((src:string-pattern v)
         (values (res:string-pattern v) env))
        ((src:identifier-pattern id)
         (define new-id (unique id))
         (values (res:identifier-pattern new-id)
                 (hash-set env id (res:id new-id bogus-type))))
        ((src:wildcard-pattern)
         (values (res:wildcard-pattern) env))
        ((src:constructor-pattern name args)
         (define-values (new-args new-env)
           (for/fold: ((new-args : (Listof res:Pattern) null)
                       (env : Env env))
                      ((arg : src:Pattern args))
             (define-values (new-arg new-env)
               (resolve-pattern arg env))
             (values (cons new-arg new-args) new-env)))
         ;; TODO fix this when TR doesn't suck so much
         (values (res:constructor-pattern
                   (hash-ref (cast data-variants (HashTable Symbol res:variant))
                             (hash-ref data-var-syms name))
                   (reverse new-args))
                 new-env))))
    (resolve-expr expr module-var-ids))





  (: new-defns (Listof res:defn))
  (define new-defns
    (for/list ((defn defns))
      (match-define (src:defn name ty expr) defn)
      (res:defn (hash-ref defined-syms name) bogus-type-scheme (resolve-expr expr))))


  (: new-exports res:exports)
  (define new-exports
    (res:exports
      (for/list: : (Listof (List Symbol res:type-export))
          ((name (filter (lambda: ((name : Symbol)) (hash-has-key? module-type-ids name))
                         (map src:export-name exports))))
        (list (res:type-constructor-name (hash-ref module-type-ids name))
              (res:type-export name (hash-ref module-type-ids name))))
      (for/list: : (Listof (List Symbol res:var-export))
          ((name (filter (lambda: ((name : Symbol)) (hash-has-key? module-var-ids name))
                         (map src:export-name exports))))
        (match (hash-ref module-var-ids name)
          ((res:toplevel-id (== module-name) new-name type)
           (list new-name (res:var-export name bogus-type-scheme)))))
      ;; TODO add pattern exports
      null))


  (: new-imports (Listof Symbol))
  (define new-imports (map src:import-name imports))

  (res:module module-name new-imports new-exports new-datas new-defns))

(: resolve-modules ((Listof src:module) -> (Listof res:module)))
(define (resolve-modules mods)
  (define linear-mods (linearize-modules mods))
  (: interfaces res:module-interfaces)
  (define interfaces (make-hash))
  (for/list ((mod linear-mods))
    (define res-mod
      (resolve-module mod interfaces))
    (hash-set! interfaces (src:module-name mod)
               (res:module->module-interface res-mod))
    res-mod))



