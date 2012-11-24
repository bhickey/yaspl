#lang racket

(provide infer-types)

(require
  "unique.rkt"
  (prefix-in res: "resolved-structures.rkt")
  (rename-in "source-structures.rkt"
    (int-ty src:int-ty)
    (string-ty src:string-ty)
    (id-ty src:id-ty)
    (ty-app src:ty-app)
    (ty-app? src:ty-app?)
    (ty-app-op src:ty-app-op)
    (ty-app-arg src:ty-app-arg)
    (struct:ty-app src:struct:ty-app)
    (fun-ty src:fun-ty))
  "type-constraints.rkt")

#|
(define int-constructor (unique 'int))
(define string-constructor (unique 'string))
(define fun-constructor (unique 'function))

(define int-ty-constraint (const-term int-constructor))
(define int-ty (const-ty int-constructor))
(define string-ty-constraint (const-term string-constructor))
(define string-ty (const-ty string-constructor))
(define (fun-ty arg res)
  (ty-app
    (ty-app (const-ty fun-constructor)
            arg) res))
|#
(define int-constraint (const-term res:int-type-constructor))
(define string-constraint (const-term res:string-type-constructor))

(define (fun-constraint arg res)
  (app-term
    (app-term
      (const-term res:fun-type-constructor)
      arg)
    res))


(define (convert-type t env)
  (match t
   ((res:type-app op arg)
    (ty-app (convert-type op env)
            (convert-type arg env)))
   ((res:type-constructor _ _ _) (const-ty t))
   ((res:type-id id _) (dict-ref env id))))

(define (convert-type-scheme t)
  (match t
   ((res:type-scheme (list (list poly-names kinds) ...) body)
    (define new-names (map unique poly-names))
    (define env
      (for/hash ((name poly-names) (new-name new-names))
        (values name (ty-var new-name))))
    (ty-abs new-names (convert-type body env)))))


(define (unconvert-type t)
  (match t
   ((ty-app op arg)
    (res:type-app
      (unconvert-type op)
      (unconvert-type arg)))
   ((const-ty t) t)))


(define (generate-constraints defns full-env)

  (define ty-schemes
    (for/hash (((name scheme) full-env))
       (values name (convert-type-scheme scheme))))


  (define/contract (get-constructor-constraints constructor)
      (-> symbol? (values term/c (listof term/c)))

    (match-define (res:type-scheme (list (list poly-names kinds) ...) body)
      (dict-ref full-env constructor))
    (define env
      (for/hash ((arg poly-names))
        (values arg (unification-term (unique arg)))))
    (define (convert t) (type->term (convert-type t env)))

    (define (linearize-fun-type t)
      (match t
        ((res:type-app
          (res:type-app
            (== res:fun-type-constructor)
            arg) res)
         (cons arg (linearize-fun-type res)))
        (_ (list t))))

    (match-define (list arg-types ... res-type)
      (linearize-fun-type body))
    (values (convert res-type) (map convert arg-types)))


  (define (generate-constraints expr name)
    (match expr
      ((int _) (set (expr-constraint name int-constraint)))
      ((str _) (set (expr-constraint name string-constraint)))
      ((id sym) (set (expr-constraint name (identifier-term sym))))
      ((lam arg body)
       (define body-name (unification-term (unique 'lam-body)))
       (set-add
         (generate-constraints body body-name)
         (expr-constraint name (fun-constraint (identifier-term arg) body-name))))
      ((app op arg)
       (define op-name (unification-term (unique 'app-op)))
       (define arg-name (unification-term (unique 'app-arg)))
       (set-add
         (set-union
           (generate-constraints op op-name)
           (generate-constraints arg arg-name))
         (expr-constraint op-name (fun-constraint arg-name name))))
      ((case expr (list (clause patterns bodies) ...))
       (define expr-name (unification-term (unique 'case-expr)))
       (apply set-union*
         (generate-constraints expr expr-name)
         (append
           (for/list ((body bodies))
             (define body-name (unification-term (unique 'case-body)))
             (set-add
               (generate-constraints body body-name)
               (expr-constraint name body-name)))
           (for/list ((pat patterns))
             (define pat-name (unification-term (unique 'case-pattern)))
             (set-add
               (generate-pattern-constraints pat pat-name)
               (expr-constraint expr-name pat-name))))))))
  
  (define (generate-pattern-constraints pat name)
    (match pat
      ((number-pattern _) (set (expr-constraint name int-constraint)))
      ((string-pattern _) (set (expr-constraint name string-constraint)))
      ((identifier-pattern id) (set (expr-constraint name (identifier-term id))))
      ((wildcard-pattern) (set))
      ((constructor-pattern constructor args)
       (define arg-names (map (lambda (_) (unique 'pat-arg)) args))
       (define-values (pat-term arg-terms)
          (get-constructor-constraints constructor))
       (set-add
         (apply set-union*
           (append
             (map generate-constraints args arg-names)
             (map expr-constraint arg-names arg-terms)))
         (expr-constraint name pat-term)))))

  (apply set-union*
    (cons
      (for/set (((name scheme) ty-schemes))
        (binding-constraint name scheme))
      (for/list ((def defns))
        (match def
          ((defn name type expr)
           (generate-constraints expr (identifier-term name))))))))

(define (infer-types defns env)
  (for/hash (((id val) (unify (generate-constraints defns env))))
    (values id (unconvert-type (term->type val)))))
  
