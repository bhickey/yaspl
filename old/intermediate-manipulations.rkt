#lang racket

(require "intermediate-ast.rkt")
(require "names.rkt")
(require racket/pretty)
(require unstable/hash)
(provide convert-module module->signature)


(define (module->signature mod)
 (module-signature (module-exports mod)))


(define (convert-module module signatures)
  (let* (
         (module (add-import-declarations module signatures))
         (module (add-constructors module))
         (module (uniquify-names-module module))
         (module ((expr-fun->module-fun simplify-case-expr) module))
         (module ((expr-fun->module-fun wrap-cases) module))
         (module ((expr-fun->module-fun anf-expr) module))
         (module (close-module module))
         (module (lift-module module))
         )
    module))



 


(define ((expr-fun->program-fun f) prog)
 (match prog
  ((program modules main-name expr)
   (program (map (expr-fun->module-fun f) modules) main-name (f expr)))))


(define ((expr-fun->module-fun f) mod)
 (match mod
  ((module id imports exports forms)
   (module id imports exports
    (for/list ((form forms))
     (match form
      ((variable-definition name expr)
       (variable-definition name (f expr)))
      (else form)))))))

(define (add-import-declarations mod signatures)
 (match mod
  ((module id imports exports forms)
   (define new-defs
    (for/list ((import imports))
     (match import
      ((module-import mod-name)
       (match (hash-ref signatures mod-name)
        ((module-signature exports)
         (filter identity
          (for/list ((export exports))
           (match export
            ((value-export ext-name _ _)
             (variable-declaration (module-name mod-name ext-name) #f))
            (else #f))))))))))
   (module id imports exports (append* forms new-defs)))))

(define (add-constructors mod)
 (match mod
  ((module id imports exports forms)
   (define new-defs
    (for/list ((def forms)
               #:when (datatype-definition? def))
     (match def
      ((datatype-definition name args variants)
       (for/list ((var variants))
        (match var
         ((variant-definition tag pattern-name constructor-name fields)
          (define field-names (map fresh-name fields))
          (variable-definition constructor-name
           (foldr lambda-expr
            (constructor-expr tag (map identifier-expr field-names))
            field-names)))))))))
   (module id imports exports (append* forms new-defs)))))


(define (uniquify-names-module mod)
 (match mod
  ((module id imports exports forms)
   (define top-level (make-hash))
   (for* ((pass (list 'imports 'internal))
          (def forms))
    (match def
     ((variable-definition (source-name name) _)
      (when (equal? pass 'internal)
       (hash-set! top-level (source-name name) (fresh-top-name name))))
     ((variable-declaration (and mod-name (module-name mod name)) _)
      (when (equal? pass 'imports)
       (hash-set! top-level (source-name name) mod-name)))
     ((datatype-definition name args variants)
      (void))))

   (define frozen-top-level (make-immutable-hash (hash-map top-level cons)))


   (define (uniquify-case-clause clause env)
    (match clause
     ((case-clause pattern body)
      (define bound-variables (pattern-bound-variables pattern))
      (define new-env (hash-union env (fresh-mapping bound-variables)
                                  #:combine (lambda (old new) new)))
      (case-clause (uniquify-pattern pattern new-env)
                   (uniquify-expr body new-env)))))

   (define (uniquify-pattern pattern env)
    (define (recur pattern) (uniquify-pattern pattern env))
    (match pattern
     ((nobind-pattern) pattern)
     ((identifier-pattern name) (identifier-pattern (hash-ref env name)))
     ((constructor-pattern pattern tag patterns)
      (constructor-pattern pattern tag (map recur patterns)))))

   (define (uniquify-expr expr env)
    (define (recur expr) (uniquify-expr expr env))
    (match expr
     ((case-expr body clauses)
      (case-expr (recur body)
                 (map (lambda (e) (uniquify-case-clause e env)) clauses)))
     ((constructor-expr tag args)
      (constructor-expr tag (map recur args)))
     ((app-expr fun arg)
      (app-expr (recur fun) (recur arg)))
     ((bind (binding name bound) body)
      (define new-name (fresh-name name))
      (bind (binding new-name (recur bound))
            (uniquify-expr body (hash-set env name new-name))))
     ((identifier-expr name)
      (identifier-expr (hash-ref env name)))
     ((lambda-expr name body)
      (define new-name (fresh-name name))
      (lambda-expr new-name (uniquify-expr body (hash-set env name new-name))))))

   (define new-forms
    (for/list ((def forms))
     (match def
      ((variable-definition name body)
       (variable-definition (hash-ref frozen-top-level name)
                            (uniquify-expr body frozen-top-level)))
      ((variable-declaration (module-name mod name) type)
       def)
      ((datatype-definition name args variants)
       (datatype-definition name args
        (for/list ((variant variants))
         (match variant
          ((variant-definition tag pattern-name constructor-name fields)
           (variant-definition tag pattern-name
                               (hash-ref frozen-top-level constructor-name)
                               fields)))))))))
   (define new-exports
    (for/list ((export exports))
     (match export
      ((data-export name) export)
      ((value-export ext-name int-name type)
       (value-export ext-name (hash-ref frozen-top-level int-name) type)))))

   (module id imports new-exports new-forms))))


(define (complicated-case-pattern? pattern)
  (define (simple-pattern? pattern)
    (or (nobind-pattern? pattern) (identifier-pattern? pattern)))
  (match pattern
   ((? simple-pattern?) #f)
   ((constructor-pattern pattern tag patterns)
    (not (andmap simple-pattern? patterns)))))


(define (simplify-case-clause clause)
 (match clause
  ((case-clause pattern body)
   (case-clause pattern (simplify-case-expr body)))))

(define (simplify-case-expr expr)
 (match expr
  ((lambda-expr name body)
   (lambda-expr name (simplify-case-expr body)))
  ((letrecur (list (binding names bound-bodies) ...) body)
   (letrecur (map binding names (map simplify-case-expr bound-bodies)) body))
  ((identifier-expr name) expr)
  ((constructor-expr tag args)
   (constructor-expr tag (map simplify-case-expr args)))
  ((app-expr fun arg)
   (app-expr (simplify-case-expr fun) (simplify-case-expr arg)))
  ((case-expr expr (list (case-clause patterns bodies) ...))
   (define new-expr (simplify-case-expr expr))
   (define is-complicated (and (> (length patterns) 1)
                               (ormap complicated-case-pattern? patterns)))
   (if (not is-complicated)
       (case-expr new-expr
                  (map case-clause patterns (map simplify-case-expr bodies)))
       (let ((name (fresh-name 'cond-expr)))
        (simplify-case-expr
         (letrecur (list (binding name) new-expr)
          (case-expr (identifier-expr name)
           (list (case-clause (first patterns)
                              (simplify-case-clause (first bodies)))
                 (case-clause
                  (nobind-pattern)
                  (case-expr (identifier-expr name)
                             (map case-clause
                                  (rest patterns)
                                  (rest bodies)))))))))))))


(define (wrap-cases expr)
  (define (recur expr top)
   (match expr
    ((lambda-expr name body)
     (lambda-expr name (recur body #t)))
    ((identifier-expr _) expr)
    ((constructor-expr tag args)
     (constructor-expr tag (map (lambda (e) (recur e #f)) args)))
    ((bind (binding name bound) body)
     (bind (binding name (recur bound #f))
           (recur body #f)))
    ((letrecur (list (binding names bounds) ...) body)
     (letrecur (map binding names (map (lambda (e) (recur e #f)) bounds))
               (recur body #f)))
    ((app-expr fun arg)
     (app-expr (recur fun #f) (recur arg #f)))
    ((case-expr e (list (case-clause patterns bodies) ...))
     (if (and top (simple-expr? e))
         (case-expr (recur e #f)
                    (map case-clause patterns (map (lambda (e) (recur e #f)) bodies)))
         (let ((name (fresh-name 'scrutinee)))
          (recur (app-expr
                   (lambda-expr name 
                     (case-expr (identifier-expr name)
                                (map case-clause patterns bodies)))
                   e) #f))))))
  (recur expr #t))
    
                        
(define (simple-expr? e)
  (match e
   ((identifier-expr name) #t)
   ((lambda-expr name body) #t)
   (else #f)))


(define (anf-expr expr)

  (define (normalize-name term k)
    (normalize
      term
      (lambda (term2)
        (if (simple-expr? term2)
            (k term2)
            (let ((name (fresh-name)))
              (bind (binding name term2)
                    (k (identifier-expr name))))))))

  (define (normalize-names terms k)
    (if (empty? terms) (k '())
      (normalize-name (first terms)
       (lambda (term-name)
        (normalize-names (rest terms)
         (lambda (term-names)
           (k (cons term-name term-names))))))))


  (define (normalize-top expr)
   (match expr
    ((case-expr (? simple-expr? e) 
                (list (case-clause patterns bodies) ...))
     (case-expr e
        (map case-clause patterns (map normalize-expr bodies))))
    (eles (normalize-expr expr))))

  (define (normalize-expr expr)
    (normalize expr identity))

  (define (normalize expr k)
   (match expr
    ((app-expr fun arg)
      (normalize-name fun
       (lambda (fun-name)
        (normalize-name arg
         (lambda (arg-name)
           (k (app-expr fun-name arg-name)))))))
    ((constructor-expr tag args)
      (normalize-names args
       (lambda (arg-names)
           (k (constructor-expr tag arg-names)))))
    ((bind (binding name bound) body)
     (normalize bound (lambda (n) (bind (binding name n) (normalize body k)))))
    ((letrecur (list (binding names bounds) ...) body)
     (when (not (andmap simple-expr? bounds))
       (error 'anf-expr "Bad letrecur"))
     (letrecur (map binding names (map normalize-expr bounds))
               (normalize body k)))
    ((identifier-expr _) (k expr))
    ((lambda-expr name body)
     (k (lambda-expr name (normalize-top body))))))

  (normalize-top expr))



(define (free-variables expr)
  (define (recur expr)
   (match expr
    ((lambda-expr name body)
     (set-remove (recur body) name))
    ((identifier-expr name) (set name))
    ((bind (binding name bound) body)
     (set-union (recur bound) (set-remove (recur body) name)))
    ((letrecur (list (binding names bounds) ...) body)
     (set-subtract (apply set-union (map recur (cons body bounds)))
                   (list->set names)))
    ((app-expr fun arg)
     (set-union (recur fun) (recur arg)))
    ((constructor-expr tag args)
     (apply set-union (set) (map recur args)))
    ((case-expr e (list (case-clause patterns bodies) ...))
     (apply set-union (recur e) 
            (map (lambda (pattern body)
                   (set-subtract (recur body)
                                 (pattern-bound-variables pattern)))
                 patterns bodies)))))
  (set->list (recur expr)))


(define (replace-free-variables mapping expr)
  (define (recur expr)
   (match expr
    ((lambda-expr name body)
     (lambda-expr name (replace-free-variables (hash-remove mapping name) body)))
    ((identifier-expr name)
     (identifier-expr (hash-ref mapping name name)))
    ((bind (binding name bound) body)
     (bind (binding name (recur bound))
           (replace-free-variables (hash-remove mapping name) body)))
    ((letrecur (list (binding names bounds) ...) body)
     (define new-mapping (foldr hash-remove mapping names))
     (define (recur e) (replace-free-variables new-mapping e))
     (letrecur (map binding names (map recur bounds)) (recur body)))
    ((app-expr fun arg)
     (app-expr (recur fun) (recur arg)))
    ((constructor-expr tag args)
     (constructor-expr tag (map recur args)))
    ((case-expr e (list (case-clause patterns bodies) ...))
     (case-expr (recur e) 
            (map (lambda (pattern body)
                   (define new-mapping
                     (foldr (lambda (v h) (hash-remove h v)) mapping
                            (set->list (pattern-bound-variables pattern))))
                   (case-clause pattern (replace-free-variables new-mapping body)))
                 patterns bodies)))))
  (recur expr))
  

(define (fresh-mapping names)
  (for/hash ((name names))
    (values name (fresh-name name))))


(define (close-module mod)
 (match mod
  ((module id imports exports defs)
   (define top-level
     (for/list ((def defs) #:when (variable-definition? def))
               (variable-definition-name def)))
   ((expr-fun->module-fun (close top-level)) mod))))


(define ((close top-level) expr)

  (define (remove-top-level free)
    (filter local-name? free))

  (define (recur expr)
   (match expr
    ((lambda-expr name body)
     (define free-vars (remove-top-level (remove name (free-variables body))))
     (define mapping (fresh-mapping free-vars))
     (define new-body (recur (replace-free-variables mapping body)))
     (match-define (list (list captured fresh) ...)
       (hash-map mapping list))
     (create-closure (closure-def name #f fresh new-body) captured))
    ((identifier-expr name) expr)
    ((bind (binding name bound) body)
     (bind (binding name (recur bound)) (recur body)))
    ((letrecur (list (binding names bounds) ...) body)
     (letrecur (map binding names  (map recur bounds))
               (recur body)))
    ((app-expr fun arg)
     (app-expr (recur fun) (recur arg)))
    ((constructor-expr tag args)
     (constructor-expr tag (map recur args)))
    ((case-expr e (list (case-clause patterns bodies) ...))
     (case-expr (recur e) 
            (map case-clause patterns
                 (map recur bodies))))))
  (recur expr))

(define (lift-module mod)
 (define global-functions (make-hash))

 (define (lift expr)
  (define (recur expr)
   (match expr
    ((create-closure (closure-def name type fresh body) captured)
     (define def (closure-def name type fresh (recur body)))
     (define new-name (fresh-top-name 'closure))
     (hash-set! global-functions new-name def)
     (create-closure new-name captured))
    ((identifier-expr name) expr)
    ((bind (binding name bound) body)
     (bind (binding name (recur bound)) (recur body)))
    ((letrecur (list (binding names bounds) ...) body)
     (letrecur (map binding names  (map recur bounds))
               (recur body)))
    ((app-expr fun arg)
     (app-expr (recur fun) (recur arg)))
    ((constructor-expr tag args)
     (constructor-expr tag (map recur args)))
    ((case-expr e (list (case-clause patterns bodies) ...))
     (case-expr (recur e) 
            (map case-clause patterns
                 (map recur bodies))))))
  (recur expr))

 (match mod
  ((module id imports exports defs)
   (lifted-module imports exports
    (filter datatype-definition? defs)
    (for/hash ((def defs)
               #:when (variable-definition? def))
      (match def
       ((variable-definition name expr)
        (values name (lift expr)))))
    (make-immutable-hash (hash->list global-functions))))))


       

