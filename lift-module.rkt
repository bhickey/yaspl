#lang typed/racket

(require
  "unique.rkt"
  "hash.rkt"
  (prefix-in res: "resolved-structures.rkt")
  (prefix-in li: "lifted-structures.rkt"))

(provide lift-module)

(: bogus-symbol Symbol)
(define bogus-symbol (unique 'bogus))
(: bogus-type li:Type)
(define bogus-type #f)

(: lift-module (res:module -> li:module))
(define (lift-module mod)
  (: mod-name Symbol)
  (: imports (Listof Symbol))
  (: exports res:exports)
  (: data (Listof res:data))
  (: defns (Listof res:defn))
  (match-define (res:module mod-name imports exports data defns) mod)


  (: functions (HashTable Symbol li:function))
  (define functions (make-hash))

  (: lift (res:Expression -> li:Expr))
  (define (lift expr)
    (match expr
      ((res:int v) (li:int v))
      ((res:str v) (li:str v))
      ((res:id v t) (li:id v))
      ((res:toplevel-id mod v t) (li:toplevel-id mod v))
      ((res:case expr clauses _ _)
       (define case-val (unique 'case-val))
       (li:bind case-val (lift expr)
         (li:case case-val
           (map lift-clause clauses))))
      ((res:lam arg type body)
       (define-values (fun-name free-vars) (lift-fun (list arg) body))
       (define local-fun-name (unique 'local-fun))
       (define env-name (unique 'env))
       (define closure-name (unique 'closure))
       (li:bind local-fun-name (li:toplevel-id mod-name fun-name)
         (li:bind env-name (li:make-tuple free-vars)
           (li:bind closure-name (li:make-tuple (list local-fun-name env-name))
             (li:pack closure-name bogus-symbol bogus-type bogus-type)))))
      ((res:app orig-fun orig-arg _)
       (define closure (unique 'fun-pos))
       (define arg (unique 'arg-pos))
       (define unpacked-closure (unique 'unpacked))
       (define fun (unique 'fun))
       (define env (unique 'env))
       (li:bind closure (lift orig-fun)
         (li:bind arg (lift orig-arg)
           (li:unpack bogus-symbol unpacked-closure closure
              (li:bind fun (li:tuple-ref unpacked-closure 0)
                (li:bind env (li:tuple-ref unpacked-closure 1)
                  (li:app-fun fun (list env arg))))))))
      ((res:make-variant name args)
       (define arg-names (map (lambda (a) (unique 'arg)) args))
       (for/fold: ((base : li:Expr (li:make-variant name arg-names)))
                  ((arg-name (reverse arg-names))
                   (arg (reverse args)))
         (li:bind arg-name (lift arg) base)))
      ))

  (: lift-clause (res:clause -> li:clause))
  (define (lift-clause clause)
    (match clause
      ((res:clause pattern expr _ _)
       (li:clause (lift-pattern pattern) (lift expr)))))

  (: lift-pattern (res:Pattern -> li:Pattern))
  (define (lift-pattern pattern)
    (match pattern
      ((res:identifier-pattern sym) (li:id-pattern sym))
      ((res:constructor-pattern (res:variant name params args type)
                                (list (res:identifier-pattern ids) ...))
       ;;TODO remove cast when TR doesn't suck so much
       (li:constructor-pattern
         (li:variant name)
         (cast ids (Listof Symbol))))))


  (: lift-fun ((Listof Symbol) res:Expression -> (values Symbol (Listof Symbol))))
  (define (lift-fun args body)
    (define-values (lifted-body old-free-vars new-free-vars)
       (rename-free-vars args (lift body)))
    (define env-name (unique 'env))
    (define env-wrapped-body
      (for/fold: : li:Expr
          ((body lifted-body))
          ((new-var new-free-vars)
           (i (in-naturals)))
          ;;TODO remove assert when TR is less broken
        (li:bind new-var (li:tuple-ref env-name (cast i Natural)) body)))
    (define fun-name (unique 'fun))
    (hash-set! functions fun-name (li:function (cons env-name args) env-wrapped-body))
    (values fun-name old-free-vars))

  (: rename-free-vars ((Listof Symbol) li:Expr -> (values li:Expr (Listof Symbol) (Listof Symbol))))
  (define (rename-free-vars ignore body)
    (: renamed (HashTable Symbol Symbol))
    (define renamed (make-hash))

    (: recur (li:Expr (Listof Symbol) -> li:Expr))
    (define (recur expr ignore)

      (: rename (Symbol -> Symbol))
      (define (rename sym)
        (cond
          ((member sym ignore) sym)
          ((hash-ref! renamed sym (lambda () (unique sym))) => values)))

      (match expr
        ((li:int _) expr)
        ((li:str _) expr)
        ((li:toplevel-id _ _) expr)
        ((li:id sym) (li:id (rename sym)))
        ((li:inst sym ty) (li:inst (rename sym) ty))
        ((li:app-fun fun-id arg-ids)
         (li:app-fun (rename fun-id) (map rename arg-ids)))
        ((li:pack id ty-id inner-ty outer-ty)
         (li:pack (rename id) ty-id inner-ty outer-ty))
        ((li:make-tuple ids)
         (li:make-tuple (map rename ids)))
        ((li:tuple-ref id index)
         (li:tuple-ref (rename id) index))
        ((li:make-variant name ids)
         (li:make-variant name (map rename ids)))
        ((li:bind id expr body)
         (li:bind id
            (recur expr ignore)
            (recur body (cons id ignore))))
        ((li:unpack type-id new-val-id orig-value-id body)
         (li:unpack type-id new-val-id (rename orig-value-id) (recur body (cons new-val-id ignore))))
        ((li:case id clauses)
         (: recur-clause (li:clause -> li:clause))
         (define (recur-clause clause)
           (match clause
             ((li:clause pattern expr)
              (match pattern
                ((li:id-pattern id)
                 (li:clause pattern (recur expr (cons id ignore))))
                ((li:constructor-pattern name ids)
                 (li:clause pattern (recur expr (append ids ignore))))))))
         (li:case (rename id) (map recur-clause clauses)))))

    (: new-expr li:Expr)
    (define new-expr (recur body ignore))
    (: old-syms (Listof Any))
    (: new-syms (Listof Any))
    (match-define (list (cons old-syms new-syms) ...) (hash-map renamed (inst cons Symbol Symbol)))
    ;; TODO remove casts once TR doesn't suck
    (values new-expr
            (cast old-syms (Listof Symbol))
            (cast new-syms (Listof Symbol))))




  (: new-defns (HashTable Symbol li:ModValue))
  (define new-defns
    (for/hash: : (HashTable Symbol li:ModValue)
        ((defn defns))
      (match-define (res:defn name _ (res:lam arg type body)) defn)
      (define-values (fun-name free-vars) (lift-fun (list arg) body))
      (assert free-vars null?)

      (values name (li:mod-function fun-name))))

  (: new-data (HashTable Symbol li:ModValue))
  (define new-data
    (for*/hash: : (HashTable Symbol li:ModValue)
        ((datum data)
         (variant (res:data-variants datum)))

      (match-define (res:variant name params fields type) variant)
      
      (if (empty? fields)
          (values name (li:mod-adt-const name))
          (let ()
            (define field-args (map (lambda (f) (unique 'field)) fields))

            (define body
              (for/fold: ((expr : res:Expression (res:make-variant name (map res:id field-args fields))))
                         ((arg (reverse (rest field-args)))
                          (type (reverse (rest fields))))
                (res:lam arg type expr)))
            (define-values (fun-name free-vars) (lift-fun (list (first field-args)) body))
            (assert free-vars null?)

            (values name (li:mod-function fun-name))))))



  (: new-exports li:exports)
  (define new-exports
    (li:exports
      (for/hash: : (HashTable Symbol Symbol)
          ((export (res:exports-vars exports)))
        (match-define (list inner-name (res:var-export outer-name ty)) export)
        ;;TODO remove when TR doesn't suck so much
        (values outer-name (cast inner-name Symbol)))))

  (li:module mod-name functions (hash-union new-defns new-data) new-exports))
