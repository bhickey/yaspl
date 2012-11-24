#lang typed/racket

(require
  "hash.rkt"
  "unique.rkt"
  (prefix-in lifted: "lifted-structures.rkt"))

(provide (all-defined-out))

(define-type Program (U running-program done-program))
(define-type Stack (Listof bind-frame))
(define-type Heap (HashTable Location HeapValue))
(define-type ToplevelEnv (HashTable (List Symbol Symbol) Location))
(define-type Env (HashTable Symbol Location))
(define-type Location Symbol)

(struct: running-program ((expr : lifted:Expr)
                          (stack : (Listof bind-frame))
                          (heap : Heap)
                          (toplevel-env : ToplevelEnv)
                          (env : Env)) #:transparent)

(struct: done-program ((loc : Location) (heap : Heap)) #:transparent)

(define-type HeapValue (U heap-int heap-string heap-variant heap-tuple fun))
(struct: heap-int ((v : Integer)) #:transparent)
(struct: heap-string ((v : String)) #:transparent)
(struct: heap-variant ((tag : Symbol) (fields : (Listof Location))) #:transparent)
(struct: heap-tuple ((values : (Listof Location))) #:transparent)

(struct: fun ((arg-names : (Listof Symbol))
              (expr : lifted:Expr)) #:transparent)

(struct: bind-frame ((id : Symbol) (expr : lifted:Expr) (env : Env)) #:transparent)


(define environment-ref hash-ref)
(define heap-ref hash-ref)

(: new-location! (-> Location))
(define (new-location!) (gensym 'loc))
(: heap-add (Heap HeapValue -> (Values Heap Location)))
(define (heap-add heap v)
  (define loc (new-location!))
  (values (hash-set heap loc v) loc))

(: run (Program -> done-program))
(define (run prog)
  (if (running-program? prog)
      (run (step prog))
      prog))

(: step (running-program -> Program))
(define (step prog)
  (match-define (running-program expr stack heap toplevel-env env) prog)
  (match expr
    ((lifted:int v)
     (define-values (new-heap loc) (heap-add heap (heap-int v)))
     (return loc stack new-heap toplevel-env))
    ((lifted:str v)
     (define-values (new-heap loc) (heap-add heap (heap-string v)))
     (return loc stack new-heap toplevel-env))
    ((lifted:id id)
     (return (environment-ref env id) stack heap toplevel-env))
    ((lifted:toplevel-id mod id)
     (return (hash-ref toplevel-env (list mod id)) stack heap toplevel-env))

    ((lifted:inst id type)
     (return (environment-ref env id) stack heap toplevel-env))
    ((lifted:pack id type-id inner-ty outer-ty)
     (return (environment-ref env id) stack heap toplevel-env))
    ((lifted:unpack type-id new-id old-id body)
     (running-program body stack heap toplevel-env (hash-set env new-id (hash-ref env old-id))))

    ((lifted:make-tuple values)
     (define-values (new-heap loc)
        (heap-add heap (heap-tuple
                         (for/list ((id values))
                            (environment-ref env id)))))
     (return loc stack new-heap toplevel-env))
    ((lifted:tuple-ref id index)
     (define val (heap-ref heap (environment-ref env id)))
     (if (heap-tuple? val)
         (return (list-ref (heap-tuple-values val) index) stack heap toplevel-env)
         (error 'step "Heap value was not a tuple, but tried to tuple-ref it")))
    ((lifted:app-fun id args)
     (define heap-val (heap-ref heap (environment-ref env id)))
     (if (fun? heap-val)
         (let ()
           (match-define (fun arg-names fun-body) heap-val)
           (define new-env
             (for/hash: : Env ((arg args) (name arg-names))
               (values name (environment-ref env arg))))
           (running-program fun-body stack heap toplevel-env new-env))
         (error 'step "Heap value was not a function, but tried to apply it")))
    ((lifted:case id clauses)
     (define loc (environment-ref env id))
     (define val (heap-ref heap loc))
     (handle-clauses val loc clauses stack heap toplevel-env env))
    ((lifted:bind id expr body)
     (running-program expr (cons (bind-frame id body env) stack) heap toplevel-env env))))


(: return (Location Stack Heap ToplevelEnv -> Program))
(define (return loc stack heap toplevel-env)
  (match stack
    ((cons (bind-frame id expr env) stack)
     (running-program expr stack heap toplevel-env (hash-set env id loc)))
    ((list)
     (done-program loc heap))))

(: handle-clauses (HeapValue Location (Listof lifted:clause) Stack Heap ToplevelEnv Env -> running-program))
(define (handle-clauses val loc clauses stack heap toplevel-env env)
  (if (empty? clauses)
      (error 'handle-clauses "Clauses did not match variable")
      (match clauses
        ((cons (lifted:clause pattern expr) clauses)
         (match pattern
          ((lifted:id-pattern id)
           (running-program expr stack heap toplevel-env (hash-set env id loc)))
          ((lifted:constructor-pattern pattern-constructor ids)
           (match val
            ((heap-variant heap-constructor fields)
             (if (equal? pattern-constructor heap-constructor)
                 (let ()
                   (unless (equal? (length fields) (length ids))
                     (error 'handle-clauses "Wrong number of ids for variant"))
                   (define new-env
                     (for/fold: ((env : Env env)) ((id ids) (val fields))
                        (hash-set env id val)))
                   (running-program expr stack heap toplevel-env new-env))
                 (handle-clauses val loc clauses stack heap toplevel-env env))))))))))
      
(: initialize-program ((Listof lifted:module)
                       (List Symbol Symbol)
                       (List Symbol Symbol)
                       -> running-program))
(define (initialize-program modules func arg)

  (: base-toplevel-env ToplevelEnv)
  (define base-toplevel-env
    (hash-union
      (for*/hash: : ToplevelEnv
          ((mod modules)
           (id (hash-keys (lifted:module-funs mod))))
        (values (list (lifted:module-name mod) id) (new-location!)))
      (for*/hash: : ToplevelEnv
          ((mod modules)
           (id (hash-keys (lifted:module-defns mod))))
        (values (list (lifted:module-name mod) id) (new-location!)))))

  (: exported-toplevel-env ToplevelEnv)
  (define exported-toplevel-env
    (for*/hash: : ToplevelEnv
        ((mod modules)
         (vars (in-value (lifted:exports-vars (lifted:module-exports mod))))
         (export-key  (hash-keys vars)))
      (values (list (lifted:module-name mod) export-key)
              (hash-ref base-toplevel-env
                        (list (lifted:module-name mod) (hash-ref vars export-key))))))

  (: toplevel-env ToplevelEnv)
  (define toplevel-env
    (hash-union base-toplevel-env exported-toplevel-env))

  (: empty-env-location Location)
  (define empty-env-location (new-location!))
  (: empty-env-value HeapValue)
  (define empty-env-value (heap-tuple null))


  (: initial-heap Heap)
  (define initial-heap
    (hash-union
      (make-immutable-hash (list (cons empty-env-location empty-env-value)))
      (for*/hash: : Heap
           ((mod modules)
            (funs (in-value (lifted:module-funs mod)))
            (id (hash-keys funs)))
        (values (hash-ref toplevel-env (list (lifted:module-name mod) id))
          (match (hash-ref funs id)
            ((lifted:function args body)
             (fun args body)))))
      (for*/hash: : Heap
           ((mod modules)
            (defns (in-value (lifted:module-defns mod)))
            (id (hash-keys defns)))
        (define mod-name (lifted:module-name mod))
        (values (hash-ref toplevel-env (list mod-name id))
                (match (hash-ref defns id)
                  ((lifted:mod-function fun-id)
                   (heap-tuple (list (hash-ref toplevel-env (list mod-name fun-id)) empty-env-location)))
                  ((lifted:mod-adt-const constructor)
                   (heap-variant constructor null)))))))


  (define closure (unique 'fun-pos))
  (define local-arg (unique (second arg)))
  (define unpacked-closure (unique 'unpacked))
  (define local-fun (unique (second func)))
  (define env (unique 'env))
  (define expr
    (lifted:bind closure (apply lifted:toplevel-id func)
      (lifted:bind local-arg (apply lifted:toplevel-id arg)
        (lifted:unpack (unique 'bogus) unpacked-closure closure
           (lifted:bind local-fun (lifted:tuple-ref unpacked-closure 0)
             (lifted:bind env (lifted:tuple-ref unpacked-closure 1)
               (lifted:app-fun local-fun (list env local-arg))))))))


  (running-program expr null initial-heap toplevel-env (make-immutable-hash null)))

     


