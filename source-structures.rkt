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

;; Top level forms
(struct: import ((name : Symbol)) #:transparent)
(struct: export ((name : Symbol)) #:transparent)
(struct: data ((name : Symbol)
               (params : (Listof Symbol))
               (variants : (Listof variant))) #:transparent)
(struct: variant ((name : Symbol) (fields : (Listof Symbol))) #:transparent)
(struct: defn ((name : Symbol) (expr : Expression)) #:transparent)

(define-type Expression (U int str unit id lam app prim-app case))
;; Expressions
(struct: int ((val : Integer)) #:transparent)
(struct: str ((val : String)) #:transparent)
(struct: unit () #:transparent)
(struct: id ((val : Symbol)) #:transparent)
(struct: lam ((arg : Symbol) (body : Expression)) #:transparent)
(struct: app ((fn : Expression)
              (argument : Expression)) #:transparent)
;; TODO replace this with something else as it embeds the evaluator in the source
(struct: prim-app ((prim : Symbol)
                   (info : Any)
                   (args : (Listof Symbol))))
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

(: free-variables (Expression -> (Setof Symbol)))
(define (free-variables expr)
  (match expr
    ((or (int _) (str _) (unit)) (set))
    ((id v) (set v))
    ((lam arg body) (set-remove (free-variables body) arg))
    ((app fn arg) (set-union (free-variables fn) (free-variables arg)))
    ((prim-app sym info args) (apply set args))
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
    (apply set-union ((inst set Symbol)) (map bound-variables args)))))

