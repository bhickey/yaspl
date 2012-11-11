#lang racket

(require "unique.rkt" "source-structures.rkt")

(struct constraint (left right))
(struct term-name (val) #:transparent)
(struct id-name (sym) #:transparent)

(struct adt-ty-constraint (name args))
(struct fun-ty-constraint (arg res))
(struct int-ty-constraint ())
(struct string-ty-constraint ())

(define (generate-constraints module)
  (define (get-constructor-constraints constructor)
    (error 'nyi))

  (define (generate-constraints expr name)
    (match expr
      ((int _) (set (constraint name (int-ty-constraint))))
      ((str _) (set (constraint name (string-ty-constraint))))
      ((id sym) (set (constraint name (id-name sym))))
      ((lam arg body)
       (define body-name (term-name (unique 'lam-body)))
       (set-add
         (generate-constraints body body-name)
         (constraint name (fun-ty-constraint (id-name arg) body-name))))
      ((app op arg)
       (define op-name(term-name (unique 'app-op)))
       (define arg-name (term-name (unique 'app-arg)))
       (set-add
         (set-union
           (generate-constraints op op-name)
           (generate-constraints arg arg-name))
         (constraint op-name (fun-ty-constraint arg-name name))))
      ((case expr (list (clause patterns bodies) ...))
       (define expr-name (unique 'case-expr))
       (apply set-union*
         (generate-constraints expr expr-name)
         (append
           (for/list ((body bodies))
             (define body-name (unique 'case-body))
             (set-add
               (generate-constraints body body-name)
               (constraint name body-name)))
           (for/list ((pat patterns))
             (define pat-name (unique 'case-pattern))
             (set-add
               (generate-pattern-constraints pat pat-name)
               (constraint expr-name pat-name))))))))
  
  (define (generate-pattern-constraints pat name)
    (match pat
      ((number-pattern _) (set (constraint name (int-ty-constraint))))
      ((string-pattern _) (set (constraint name (string-ty-constraint))))
      ((identifier-pattern id) (set (constraint name (id-name id))))
      ((wildcard-pattern) (set))
      ((constructor-pattern constructor args)
       (define arg-names (map (lambda (_) (unique 'pat-arg)) args))
       (define-values (pat-constraint arg-constraints)
          (get-constructor-constraints constructor))
       (set-add
         (apply set-union*
           (append
             (map generate-constraints args arg-names)
             (map constraint arg-names arg-constraints)))
         (constraint name pat-constraint)))))
  
  (error 'nyi))
