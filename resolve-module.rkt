#lang typed/racket

(require
  "unique.rkt"
  (prefix-in src: "source-structures.rkt")
  (prefix-in res: "resolved-structures.rkt"))

(require/typed unstable/hash
  (hash-union (All (b) ((HashTable Symbol b) (HashTable Symbol b) * -> (HashTable Symbol b)))))

(require/typed "type-constraints.rkt"
  (#:opaque ConstTerm const-term?)
  (#:opaque AppTerm app-term?))

(require/typed "type-inference.rkt"
  (infer-types ((Listof src:defn) (HashTable Symbol res:type-scheme) -> (HashTable Symbol res:Type))))


(provide resolve-module)

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
    ((src:defn name expr)
     (src:defn name (unique-expr expr (make-immutable-hash null))))))





(: resolve-module (src:module #f -> res:module))
(define (resolve-module src-module module-interfaces)
  (match-define (src:module module-name imports exports datas orig-defns) src-module)
  (define defns (map unique-defn orig-defns))


  ;; TODO 
  (: imported-var-ids (HashTable Symbol (U res:module-id res:lexical-id)))
  (: imported-types (HashTable Symbol res:type-constructor))
  (define imported-var-ids (make-immutable-hash null))
  (define imported-types (make-immutable-hash null))

  (: defined-syms (HashTable Symbol Symbol))
  (define defined-syms
    (make-immutable-hash
      (for/list: : (Listof (Pair Symbol Symbol))
          ((defn defns))
        (define name (src:defn-name defn))
        (cons name (unique name)))))

  (: data-var-syms (HashTable Symbol Symbol))
  (define data-var-syms
    (for*/fold: ((env : (HashTable Symbol Symbol)
                      (ann (make-immutable-hash null)
                           (HashTable Symbol Symbol))))
                ((data : src:data datas)
                 (variant : src:variant (src:data-variants data)))
      (define name (src:variant-name variant))
      (hash-set env name (unique name))))


  (: data-type-syms (HashTable Symbol Symbol))
  (define data-type-syms
    (make-immutable-hash
      (for/list: : (Listof (Pair Symbol Symbol))
          ((data datas))
        (define name (src:data-name data))
        (cons name (unique name)))))

  (: data-type-types (HashTable Symbol res:type-constructor))
  (define data-type-types
    (make-immutable-hash
      (for/list: : (Listof (Pair Symbol res:type-constructor))
          ((data datas))
        (match data
          ((src:data name params _)
           (cons name
             (res:type-constructor
               module-name
               (hash-ref data-type-syms name)
               (for/fold: ((kind : src:Kind (src:type-kind)))
                          ((arg : (List Symbol src:Kind) (reverse params)))
                 (src:arr-kind (second arg) kind)))))))))


  (: module-type-ids (HashTable Symbol res:type-constructor))
  (define module-type-ids 
    (hash-union imported-types data-type-types))

  (: data-var-type-schemes (HashTable Symbol res:type-scheme))
  (define data-var-type-schemes
    (apply hash-union (ann (make-immutable-hash empty) (HashTable Symbol res:type-scheme))
      (for/list: : (Listof (HashTable Symbol res:type-scheme))
          ((data : src:data datas))
        (match-define (src:data name params variants) data)
        (define param-names (map (inst first Symbol Any) params))
        (define param-kinds (map (inst second Symbol src:Kind Null) params))
        (define new-param-names (map unique param-names))
        (define new-params
          (for/list: : (Listof (List Symbol src:Kind))
              ((name new-param-names)
               (kind param-kinds))
            (list name kind)))

        (define new-env 
          (for/fold: ((env : (HashTable Symbol res:Type)
                        (hash-value-map (inst values res:Type) module-type-ids)))
                     ((param-name param-names)
                      (param-kind param-kinds)
                      (new-param new-param-names))
            (hash-set env param-name (res:type-id new-param param-kind))))
        (define return-type
          (for/fold: ((t : res:Type (hash-ref module-type-ids name)))
                     ((param-name : Symbol param-names)) 
            (res:type-app t (hash-ref new-env param-name))))

        (define (convert-variant variant)
          (match variant
            ((src:variant _ fields)
             (res:type-scheme
               new-params
               (for/fold: ((t : res:Type return-type))
                          ((field : Symbol (reverse fields)))
                 (res:type-app 
                   (res:type-app res:fun-type-constructor
                     (hash-ref new-env field))
                     t))))))

        (make-immutable-hash
          (map (inst cons Symbol res:type-scheme)
               (map src:variant-name variants)
               (map convert-variant variants))))))

  (: module-var-ids (HashTable Symbol (U res:module-id res:lexical-id)))
  (define module-var-ids 
    (let: ((inject : (Symbol -> (U res:module-id res:lexical-id)) res:lexical-id))
      (hash-union imported-var-ids 
                  (hash-value-map inject defined-syms)
                  (hash-value-map inject data-var-syms))))



  (: substitution (HashTable Symbol res:Type))
  (define substitution (infer-types defns data-var-type-schemes))


  (: resolve-expr (src:Expression -> res:Expression))
  (define (resolve-expr expr)
    (define-type Env (HashTable Symbol (U res:module-id res:lexical-id)))
    (: resolve-expr (src:Expression Env -> res:Expression))
    (define (resolve-expr expr env)
      (match expr
        ((src:int v) (res:int v))
        ((src:str v) (res:str v))
        ((src:id sym) (hash-ref env sym))
        ((src:lam arg body)
         (define new-arg (unique arg))
         (res:lam new-arg (resolve-expr body (hash-set env arg (res:lexical-id new-arg)))))
        ((src:app fn arg)
         (res:app (resolve-expr fn env) (resolve-expr arg env)))
        ((src:case expr clauses)
         (res:case (resolve-expr expr env) (map (resolve-clause env) clauses)))))
    (: resolve-clause (Env -> (src:clause -> res:clause)))
    (define ((resolve-clause env) clause)
      (match clause
        ((src:clause pattern expr)
         (define-values (new-pattern new-env) (resolve-pattern pattern env))
         (res:clause new-pattern (resolve-expr expr new-env)))))
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
                 (hash-set env id (res:lexical-id new-id))))
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
         (values (res:constructor-pattern (hash-ref data-var-syms name) (reverse new-args))
                 new-env))))
    (resolve-expr expr module-var-ids))





  (: new-defns (Listof res:defn))
  (define new-defns
    (for/list ((defn defns))
      (match-define (src:defn name expr) defn)
      (res:defn (hash-ref defined-syms name) (resolve-expr expr))))

  (: new-datas (Listof res:data))
  (define new-datas
    (for/list ((data datas))
      (match-define (src:data name params variants) data)
      (define param-names (map (inst first Symbol Any) params))
      (define param-kinds (map (inst second Symbol src:Kind Null) params))
      (define new-name (hash-ref data-type-syms name))
      (define new-param-names (map unique param-names))
      (define new-env 
        (for/fold: ((env : (HashTable Symbol res:Type)
                      (hash-value-map (inst values res:Type) module-type-ids)))
                   ((param-name param-names)
                    (param-kind param-kinds)
                    (new-param new-param-names))
          (hash-set env param-name (res:type-id new-param param-kind))))

      (define (resolve-variant variant)
        (match variant
          ((src:variant name fields)
           (res:variant
             (hash-ref data-var-syms name)
             (for/list: : (Listof res:Type) ((field : Symbol fields))
                (hash-ref new-env field))))))
      (: Pair (Symbol src:Kind -> (List Symbol src:Kind)))
      (define (Pair a b) (list a b))
      (res:data new-name (map Pair new-param-names param-kinds) (map resolve-variant variants))))

  ;; TODO
  (: new-exports res:exports)
  (define new-exports
    (res:exports null null))

  ;; TODO
  (: new-imports (Listof Symbol))
  (define new-imports null)

  (res:module module-name new-imports new-exports new-datas new-defns))




