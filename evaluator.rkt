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

(struct program-state (heap top-env env stack expr) #:transparent)
(struct finished-state (heap val) #:transparent)
(struct error-state (heap env stack expr error) #:transparent)

(struct stack-frame (name env expr) #:transparent)


(define fresh-location
 (let ((next-location 0))
  (lambda ()
   (begin0 (location next-location)
           (set! next-location (add1 next-location))))))

(define (step state)
 (match state
  ((program-state heap top-env env stack expr)
   (match* (stack expr)
    ((stack (pack-expr _ _ value))
     (program-state heap top-env env stack value))
    (((list (stack-frame new-name new-env expr) stack-rest ...)
      (identifier-expr name))
     (let ((v (hash-ref env name)))
       (program-state heap top-env (hash-set new-env new-name v) stack-rest expr)))
    ((stack (unpack-expr _ name value body))
     (program-state heap top-env env stack (bind-expr name value body)))
    ((stack (app-expr (identifier-expr fun-name) _
                      (list (identifier-expr arg-names) ...)))
     (define args (map (lambda (name) (hash-ref env name)) arg-names))
     (match (hash-ref heap (indirect-val-location (hash-ref env fun-name)))
      ((code-value new-args body)
       (program-state heap top-env
                      (hash-union top-env
                                  (make-immutable-hash (map cons new-args args)))
                      stack body))
      ((prim-code-value fun)
       (match stack
        ((list (stack-frame new-name new-env expr) stack-rest ...)
         (define-values (new-heap new-val) (fun heap args))
         (program-state new-heap top-env
                        (hash-set new-env new-name new-val)
                        stack-rest expr))))
      ((control-code-value fun)
       (fun heap env stack args))))
    ((stack (bind-expr return-name
                       (app-expr (identifier-expr fun-name) _
                        (list (identifier-expr arg-names) ...))
                       body))
     (define args (map (lambda (name) (hash-ref env name)) arg-names))
     (match (hash-ref heap (indirect-val-location (hash-ref env fun-name)))
      ((code-value new-args code-body)
       (program-state heap top-env
                      (hash-union (make-immutable-hash (map cons new-args args))
                                  top-env)
                      (cons (stack-frame return-name env body) stack) code-body))
      ((prim-code-value fun)
        (define-values (new-heap new-val) (fun heap args))
        (program-state new-heap top-env
                       (hash-set env return-name new-val)
                       stack body))
      ((control-code-value fun)
       (fun heap env (cons (stack-frame return-name env body) stack) args))))
    ((stack (bind-expr name (identifier-expr old-name) body))
     (program-state heap top-env
                    (hash-set env name (hash-ref env old-name))
                    stack body))
    ((stack (bind-expr name (tuple-proj-expr index (identifier-expr old-name)) body))
     (define tuple (hash-ref heap (indirect-val-location (hash-ref env old-name))))
     (program-state heap top-env
                    (hash-set env name
                      (list-ref (tuple-value-fields tuple) index))
                    stack body))

    ;Handle Tuple return and Tuple bind
    ((stack (tuple-expr (list (identifier-expr names) ...)))
     (define values (for/list ((name names)) (hash-ref env name)))
     (define loc (fresh-location))
     (define new-heap (hash-set heap loc (tuple-value values)))
     (match stack
      ((list (stack-frame name env body) stack-rest ...)
       (program-state new-heap top-env
                      (hash-set env name (indirect-val loc))
                      stack-rest body))))
    ((stack (bind-expr name (tuple-expr (list (identifier-expr names) ...)) body))
     (define values (for/list ((name names)) (hash-ref env name)))
     (define loc (fresh-location))
     (define new-heap (hash-set heap loc (tuple-value values)))
     (program-state new-heap top-env
                    (hash-set env name (indirect-val loc)) stack body))

    ;Handle Constructor return and Constructor bind
    ((stack (constructor-expr variant (list (identifier-expr names) ...)))
     (define values (for/list ((name names)) (hash-ref env name)))
     (define loc (fresh-location))
     (define new-heap (hash-set heap loc (constructor-value variant values)))
     (match stack
      ((list (stack-frame name env body) stack-rest ...)
       (program-state new-heap top-env
                      (hash-set env name (indirect-val loc))
                      stack-rest body))))
    ((stack (bind-expr name
              (constructor-expr variant (list (identifier-expr names) ...)) body))
     (define values (for/list ((name names)) (hash-ref env name)))
     (define loc (fresh-location))
     (define new-heap (hash-set heap loc (constructor-value variant values)))
     (program-state new-heap top-env
                    (hash-set env name (indirect-val loc)) stack body))

    ;Handle Case expr
    ((stack (case-expr (identifier-expr value-name)
                       (list (case-clause patterns bodies) ...)))
     (define value (hash-ref heap (indirect-val-location (hash-ref env value-name))))
     (for/or ((pattern patterns) (body bodies))
      (match pattern
       ((nobind-pattern) (program-state heap top-env env stack body))
       ((identifier-pattern new-name)
        (program-state heap top-env
                       (hash-set env new-name (hash-ref env value-name))
                       stack body))
       ((constructor-pattern variant (list (identifier-pattern names) ...))
        (and (equal? variant (constructor-value-variant value))
             (let ((new-env (for/fold ((env env))
                                      ((name names)
                                       (v (constructor-value-fields value)))
                             (hash-set env name v))))
               (program-state heap top-env new-env stack body)))))))))))

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
   (stack-frame value-name (hash stop-name (indirect-val stop-loc))
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
 
   (program-state frozen-heap frozen-env frozen-env initial-stack expr))))


     

