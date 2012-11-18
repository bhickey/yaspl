#lang racket

(require (prefix-in lifted: "lifted-structures.rkt"))

(struct program (expr stack heap env))
(struct done-program (loc heap))

(struct heap-int (v))
(struct heap-string (v))
(struct heap-variant (tag fields))
;;env is a list of locations
(struct heap-env (values))
(struct heap-closure (fun env))

(struct fun (arg-names expr))

(struct bind-frame (id expr env))


(define heap-ref dict-ref)
(define heap-add dict-set)
(define environment-ref dict-ref)


(define (step-expr expr stack heap env)
  (match expr
    ((lifted:int v)
     (define-values (new-heap loc) (heap-add heap (heap-int v)))
     (return loc stack new-heap))
    ((lifted:string v)
     (define-values (new-heap loc) (heap-add heap (heap-string v)))
     (return loc stack new-heap))
    ((lifted:id id _)
     (return (environment-ref env id) stack heap))

    ((lifted:make-closure fun-id env-id)
     (define-values (new-heap loc)
        (heap-add heap (heap-closure 
                         (environment-ref env fun-id)
                         (environment-ref env env-id))))
     (return loc stack new-heap))
    ((lifted:closure-env id)
     (match-define (heap-closure fun env) (heap-ref heap (environment-ref env id)))
     (return env stack heap))
    ((lifted:closure-fun id)
     (match-define (heap-closure fun env) (heap-ref heap (environment-ref env id)))
     (return fun stack heap))


    ((lifted:make-env ids)
     (define-values (new-heap loc)
        (heap-add heap (heap-env 
                         (for/list ((id ids))
                           (environment-ref env id)))))
     (return loc stack new-heap))
    ((lifted:env-ref env-id index)
     (return (list-ref (heap-ref heap (environment-ref env env-id)) index) stack heap))

    ((lifted:app-fun id args _)
     (match-define (fun arg-names fun-body) (heap-ref heap (environment-ref env id)))
     (define new-env
       (for/hash ((arg args) (name arg-names))
         (values name (environment-ref env arg))))
     (program fun-body stack heap new-env))
    ((lifted:case id clauses _)
     (define val (heap-ref heap (environment-ref env id)))
     (handle-clauses val clauses stack heap env))
    ((lifted:bind id expr body)
     (program expr (cons (bind-frame id body env) stack) heap env))))


(define (return loc stack heap)
  (match stack
    ((cons (bind-frame id expr env) stack)
     (program expr stack heap (dict-set env id loc)))
    ((list)
     (done-program loc heap))))

(define (handle-clauses val clauses stack heap env)
  (if (empty? clauses)
      (error 'handle-clauses "Clauses did not match variable")
      (match clauses
        ((list (lifted:clause pattern expr _) clauses)
         (match pattern
          ((lifted:id-pattern id)
           (program expr stack heap (dict-set env id val)))
          ((lifted:constructor-pattern pattern-constructor ids)
           (match val
            ((heap-variant heap-constructor fields)
             (if (equal? pattern-constructor heap-constructor)
                 (let ()
                   (unless (equal? (length fields) (length ids))
                     (error 'handle-clauses "Wrong number of ids for variant"))
                   (define new-env
                     (for/fold ((env env)) ((id ids) (val fields))
                        (dict-set env id val)))
                   (program expr stack heap new-env))
                 (handle-clauses val clauses stack heap env))))))))))
      


     


