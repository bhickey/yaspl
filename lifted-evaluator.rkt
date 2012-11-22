#lang typed/racket

(require "hash.rkt"
  (prefix-in lifted: "lifted-structures.rkt"))

(define-type Program (U running-program done-program))
(define-type Stack (Listof bind-frame))
(define-type Heap (HashTable Location HeapValue))
(define-type Env (HashTable Symbol Location))
(define-type Location Symbol)

(struct: running-program ((expr : lifted:Expr)
                          (stack : (Listof bind-frame))
                          (heap : Heap)
                          (env : Env)))

(struct: done-program ((loc : Location) (heap : Heap)))

(define-type HeapValue (U heap-int heap-string heap-variant heap-tuple))
(struct: heap-int ((v : Integer)))
(struct: heap-string ((v : String)))
(struct: heap-variant ((tag : Symbol) (fields : (Listof Location))))
(struct: heap-tuple ((values : (Listof Location))))

(struct: fun ((arg-names : (Listof Symbol))
              (expr : lifted:Expr)))

(struct: bind-frame ((id : Symbol) (expr : lifted:Expr) (env : Env)))


(define environment-ref hash-ref)
(define heap-ref hash-ref)
(: heap-add (Heap HeapValue -> (Values Heap Location)))
(define (heap-add heap v)
  (define loc (gensym 'loc))
  (values (hash-set heap loc v) loc))

(: step-expr (running-program -> Program))
(define (step-expr prog)
  (match-define (running-program expr stack heap env) prog)
  (match expr
    ((lifted:int v)
     (define-values (new-heap loc) (heap-add heap (heap-int v)))
     (return loc stack new-heap))
    ((lifted:str v)
     (define-values (new-heap loc) (heap-add heap (heap-string v)))
     (return loc stack new-heap))
    ((lifted:id id)
     (return (environment-ref env id) stack heap))

    ((lifted:inst id type)
     (return (environment-ref env id) stack heap))
    ((lifted:pack id type-id inner-ty outer-ty)
     (return (environment-ref env id) stack heap))

    ((lifted:make-tuple values)
     (define-values (new-heap loc)
        (heap-add heap (heap-tuple
                         (for/list ((id values))
                            (environment-ref env id)))))
     (return loc stack new-heap))
    ((lifted:tuple-ref id index)
     (define val (heap-ref heap (environment-ref env id)))
     (if (heap-tuple? val)
         (return (list-ref (heap-tuple-values val) index) stack heap)
         (error 'step "Heap value was not a tuple, but tried to tuple-ref it")))
    ((lifted:app-fun id args)
     (define heap-val (heap-ref heap (environment-ref env id)))
     (if (fun? heap-val)
         (let ()
           (match-define (fun arg-names fun-body) heap-val)
           (define new-env
             (for/hash: : Env ((arg args) (name arg-names))
               (values name (environment-ref env arg))))
           (running-program fun-body stack heap new-env))
         (error 'step "Heap value was not a function, but tried to apply it")))
    ((lifted:case id clauses)
     (define loc (environment-ref env id))
     (define val (heap-ref heap loc))
     (handle-clauses val loc clauses stack heap env))
    ((lifted:bind id expr body)
     (running-program expr (cons (bind-frame id body env) stack) heap env))))


(: return (Location Stack Heap -> Program))
(define (return loc stack heap)
  (match stack
    ((cons (bind-frame id expr env) stack)
     (running-program expr stack heap (hash-set env id loc)))
    ((list)
     (done-program loc heap))))

(: handle-clauses (HeapValue Location (Listof lifted:clause) Stack Heap Env -> running-program))
(define (handle-clauses val loc clauses stack heap env)
  (if (empty? clauses)
      (error 'handle-clauses "Clauses did not match variable")
      (match clauses
        ((cons (lifted:clause pattern expr) clauses)
         (match pattern
          ((lifted:id-pattern id)
           (running-program expr stack heap (hash-set env id loc)))
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
                   (running-program expr stack heap new-env))
                 (handle-clauses val loc clauses stack heap env))))))))))
      


     


