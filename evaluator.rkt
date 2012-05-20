#lang racket

(require "lifted-anf-ast.rkt")
(require unstable/hash)
(require racket/pretty)

(provide run initial-state extract-value)

(struct location (name) #:transparent)

(struct indirect-val (location) #:transparent)
(struct direct-val (value) #:transparent) ;Unused currently (for int32 and floats)

(struct tuple-value (fields) #:transparent)
(struct constructor-value (variant fields) #:transparent)
(struct code-value (args body) #:transparent)
(struct prim-code-value (function) #:transparent)
(struct control-code-value (function) #:transparent)

(struct program-state (heap env stack expr) #:transparent)
(struct finished-state (heap val) #:transparent)
(struct error-state (heap env stack expr error) #:transparent)

(struct stack-frame (name env expr) #:transparent)

(struct environment (top local) #:transparent)

(define (env-initial top)
  (environment top top))

(define (env-lookup env name)
  (hash-ref (environment-local env) name))
(define (env-lookup-many env names)
  (for/list ((name names)) (env-lookup env name)))

(define (env-add env name val)
 (match env
  ((environment top local)
   (environment top (hash-set local name val)))))

(define (env-add-many env names vals)
 (for/fold ((env env)) ((name names) (v vals))
  (env-add env name v)))

(define (env-new env new-local)
 (match env
  ((environment top local)
   (environment top (hash-union top new-local)))))


(define fresh-location
 (let ((next-location 0))
  (lambda ()
   (begin0 (location next-location)
           (set! next-location (add1 next-location))))))


(define (bind val name body heap env stack)
 (program-state heap (env-add env name val) stack body))

(define (bind-value value name body heap env stack)
 (define loc (fresh-location))
 (define new-heap (hash-set heap loc value))
 (bind (indirect-val loc) name body new-heap env stack))

(define (return val heap stack)
 (match stack
  ((cons (stack-frame name env body) stack)
   (program-state heap (env-add env name val) stack body))))

(define (return-value value heap stack)
 (define loc (fresh-location))
 (define new-heap (hash-set heap loc value))
 (return (indirect-val loc) new-heap stack))


(define (bind/return val name body heap env stack)
  (if name
    (bind val name body heap env stack)
    (return val heap stack)))


(define (step state)
 (match state
  ((program-state heap env stack expr)
   (define (heap-lookup name)
     (hash-ref heap (indirect-val-location (env-lookup env name))))
   (match expr
    ;Handle Packing and Unpacking
    ((pack-expr _ _ value)
     (program-state heap env stack value))
    ((unpack-expr _ name value body)
     (program-state heap env stack (bind-expr name value body)))

    ;Handle calls and tail calls
    ((app-expr (identifier-expr fun-name) _
               (list (identifier-expr arg-names) ...))
     (define args (env-lookup-many env arg-names))
     (match (heap-lookup fun-name)
      ((code-value new-args body)
       (program-state heap
                      (env-new env (make-immutable-hash (map cons new-args args)))
                      stack body))
      ((prim-code-value fun)
       (match stack
        ((list (stack-frame new-name new-env expr) stack-rest ...)
         (define-values (new-heap new-val) (fun heap args))
         (program-state new-heap
                        (env-add new-env new-name new-val)
                        stack-rest expr))))
      ((control-code-value fun)
       (fun heap env stack args))))
    ((bind-expr return-name
                (app-expr (identifier-expr fun-name) _
                 (list (identifier-expr arg-names) ...))
                body)
     (define args (env-lookup-many env arg-names))
     (match (heap-lookup fun-name) 
      ((code-value new-args code-body)
       (program-state heap
                      (env-new env (make-immutable-hash (map cons new-args args)))
                      (cons (stack-frame return-name env body) stack) code-body))
      ((prim-code-value fun)
        (define-values (new-heap new-val) (fun heap args))
        (program-state new-heap
                       (env-add env return-name new-val)
                       stack body))
      ((control-code-value fun)
       (fun heap env (cons (stack-frame return-name env body) stack) args))))

    ;Handle Identifier return and bind
    ((identifier-expr name)
     (return (env-lookup env name) heap stack))
    ((bind-expr name (identifier-expr old-name) body)
     (bind (env-lookup env old-name) name body heap env stack))

    ;Handle Tuple return and bind
    ((tuple-expr (list (identifier-expr names) ...))
     (return-value (tuple-value (env-lookup-many env names)) heap stack))
    ((bind-expr name (tuple-expr (list (identifier-expr names) ...)) body)
     (bind-value (tuple-value (env-lookup-many env names)) name body heap env stack))

    ;Handle Tuple-Proj return and bind
    ((tuple-proj-expr index (identifier-expr old-name))
     (return (list-ref (tuple-value-fields (heap-lookup old-name)) index)
             heap stack))
    ((bind-expr name (tuple-proj-expr index (identifier-expr old-name)) body)
     (bind (list-ref (tuple-value-fields (heap-lookup old-name)) index)
           name body heap env stack))


    ;Handle Constructor return and bind
    ((constructor-expr variant (list (identifier-expr names) ...))
     (return-value (constructor-value variant (env-lookup-many env names))
                   heap stack))
    ((bind-expr name
       (constructor-expr variant (list (identifier-expr names) ...)) body)
     (bind-value (constructor-value variant (env-lookup-many env names))
                 name body heap env stack))

    ;Handle Case expr
    ((case-expr (identifier-expr value-name)
                       (list (case-clause patterns bodies) ...))
     (define value (heap-lookup value-name))
     (for/or ((pattern patterns) (body bodies))
      (match pattern
       ((nobind-pattern) (program-state heap env stack body))
       ((identifier-pattern new-name)
        (bind (env-lookup env value-name) new-name body heap env stack))
       ((constructor-pattern variant (list (identifier-pattern names) ...))
        (and (equal? variant (constructor-value-variant value))
               (program-state heap
                              (env-add-many env names
                                            (constructor-value-fields value))
                              stack body))))))))))

(define (run state)
  (let loop ((state state))
    (if (finished-state? state)
      state (loop (step state)))))


(define (extract-value state)
 (match state
  ((finished-state heap v)
   (define (convert v)
    (match (hash-ref heap (indirect-val-location v))
     ((tuple-value values) (map convert values))
     ((constructor-value variant values) (cons variant (map convert values)))))
   (convert v))))

(define stop-code
 (let ()
   (define (stop heap env stack args)
     (finished-state heap (first args)))
   (control-code-value stop)))

(define (initial-state prog expr)
 (define initial-heap (make-hash))
 (define initial-env (make-hash))

 (define stop-loc (fresh-location))
 (hash-set! initial-heap stop-loc stop-code)
 (define stop-name (fresh-name 'stop))


 (define stop-stack-frame
   (let ((value-name (fresh-name 'final)))
   (stack-frame value-name (env-initial (hash stop-name (indirect-val stop-loc)))
        (app-expr (identifier-expr stop-name) #f
                  (list (identifier-expr value-name))))))

 (define initial-stack (list stop-stack-frame))




 (match prog
  ((program datadefs tops funs)

   (define (convert-fun func)
    (match func
     ((function (list (argument args _) ...) _ body)
      (code-value args body))))
   (for (((name fun) funs))
    (define loc (fresh-location))
    (hash-set! initial-heap loc (convert-fun fun))
    (hash-set! initial-env name (indirect-val loc)))



   (define ((simple-eval env) expr (loc #f))
    (match expr
     ((pack-expr _ _ expr)
      ((simple-eval env) expr loc))
     ((identifier-expr name)
      (when loc
        (error 'simple-eval "Trying to evaluate an identifier into a location"))
      (hash-ref env name))
     ((bind-expr name bound body)
      (define bound-val ((simple-eval env) bound))
      ((simple-eval (hash-set env name bound-val)) body loc))
     ((tuple-expr values)
      (define real-loc (or loc (fresh-location)))
      (define value (tuple-value (map (simple-eval env) values)))
      (hash-set! initial-heap real-loc value)
      (indirect-val real-loc))
     ((constructor-expr variant values)
      (define real-loc (or loc (fresh-location)))
      (define value (constructor-value variant (map (simple-eval env) values)))
      (hash-set! initial-heap real-loc value)
      (indirect-val real-loc))))

   (for (((name _) tops))
    (hash-set! initial-env name (indirect-val (fresh-location))))

   (define frozen-env (make-immutable-hash (hash-map initial-env cons)))
   (for (((name expr) tops))
    (define loc (indirect-val-location (hash-ref initial-env name)))
    ((simple-eval frozen-env) expr loc))

   

 
   (define frozen-heap (make-immutable-hash (hash-map initial-heap cons)))
 
   (program-state frozen-heap (env-initial frozen-env) initial-stack expr))))


     

