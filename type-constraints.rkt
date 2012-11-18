#lang racket

(require "unique.rkt")

(provide
  (contract-out
    (struct expr-constraint ((left term/c) (right term/c)))
    (struct ty-abs ((names (listof symbol?)) (body type/c)))
    (struct const-ty ((val any/c)))))

(provide
  unify
  term/c
  type->term
  term->type
  (struct-out binding-constraint)
  (struct-out ty-app)
  (struct-out ty-var)
  (struct-out unification-term)
  (struct-out identifier-term)
  (struct-out const-term)
  (struct-out app-term))

(struct expr-constraint (left right) #:transparent)
(struct binding-constraint (id type) #:transparent)

(struct ty-abs (names body) #:transparent)
(struct ty-app (op arg) #:transparent)
(struct const-ty (val) #:transparent)
(struct ty-var (name) #:transparent)

(define type/c (or/c ty-app? const-ty? ty-var?))


(struct unification-term (name) #:transparent)
(struct identifier-term (id) #:transparent)
(struct const-term (val)  #:transparent)
(struct app-term (op arg)  #:transparent)

(define term/c (or/c unification-term? identifier-term? const-term? app-term?))


(define (type->term t)
  (match t
    ((ty-app op arg)
     (app-term (type->term op) (type->term arg)))
    ((const-ty v) (const-term v))
    ((ty-var n) (identifier-term n))))

(define (term->type t)
  (match t
    ((app-term op arg)
     (ty-app (term->type op) (term->type arg)))
    ((const-term v) (const-ty v))))



(define (unify constraints)

  (define known-bindings
    (for/hash ((cons constraints)
               #:when (binding-constraint? cons))
      (match cons
        ((binding-constraint id type)
         (values id type)))))

  (define (get-instantiated-binding id)
    (define ty (hash-ref known-bindings id))
    (match ty
      ((ty-abs names body)
       (define env
         (for/hash ((name names))
           (values name (unification-term (unique name)))))
       (define (convert ty)
         (match ty
           ((const-ty val)
            (const-term val))
           ((ty-app op arg)
            (app-term (convert op) (convert arg)))
           ((ty-var name)
            (hash-ref env name))))
       (convert body))
      (_ ty)))


  (define (fill-in-known-bindings term)
    (match term
      ((unification-term _) term)
      ((const-term val) term)
      ((app-term op arg)
       (app-term (fill-in-known-bindings op)
                 (fill-in-known-bindings arg)))
      ((identifier-term id)
       (if (hash-has-key? known-bindings id)
           (get-instantiated-binding id)
           term))))

  (define expr-constraints
    (for/list ((cons constraints)
               #:when (expr-constraint? cons))
     (match cons
       ((expr-constraint left right)
        (expr-constraint (fill-in-known-bindings left)
                         (fill-in-known-bindings right))))))

  (define (occurs-check! id term)
    (define (recur term)
      (match term
       ((identifier-term id-other)
        (when (equal? id id-other)
          (error 'unify "Occurs check")))
       ((unification-term id-other)
        (when (equal? id id-other)
          (error 'unify "Occurs check")))
       ((const-term val) (void))
       ((app-term op arg)
        (recur op) (recur arg))))
    (recur term))

  (define (substitute target source constraints substitution)
    (define (subst-term term)
      (match term
        ((== source) target)
        ((app-term op arg)
         (app-term (subst-term op) (subst-term arg)))
        (_ term)))
    (define (subst-constraint const)
      (match const
        ((expr-constraint left right)
         (expr-constraint (subst-term left) (subst-term right)))))
    (values
      (map subst-constraint constraints)
      (for/hash (((id term) substitution))
        (values id (subst-term term)))))

  (define (handle-constraint const constraints subs)
    (match const
     ((expr-constraint v v)
      (values constraints subs))
     ((expr-constraint (and left (unification-term id)) right)
      (occurs-check! id right)
      (substitute right left constraints subs))
     ((expr-constraint left (and right (unification-term id)))
      (occurs-check! id left)
      (substitute left right constraints subs))
     ((expr-constraint (and left (identifier-term id)) right)
      (occurs-check! id right)
      (substitute right left constraints (dict-set subs id right)))
     ((expr-constraint left (and right (identifier-term id)))
      (occurs-check! id left)
      (substitute left right constraints (dict-set subs id left)))
     ((expr-constraint (app-term left-op left-arg) (app-term right-op right-arg))
      (values
        (list* (expr-constraint left-op right-op) (expr-constraint left-arg right-arg) constraints)
        subs))
     ((expr-constraint left right)
      (error 'unify "~a does not unify with ~a" left right))))


  (define (recur constraints subs)
    (if (null? constraints) subs
        (let ()
          (define-values (new-constraints new-subs)
             (handle-constraint (first constraints) (rest constraints) subs))
          (recur new-constraints new-subs))))

  (recur expr-constraints (hash)))
