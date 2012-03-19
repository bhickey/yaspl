#lang plai

(define-type SEXP
  [num (n number?)]
  [int32 (n number?)]
  [str (s string?)]
  [if-then (test SEXP?) (then SEXP?) (else SEXP?)]
  [add (fn symbol?) (lhs SEXP?) (rhs SEXP?)]
  [sub (fn symbol?) (lhs SEXP?) (rhs SEXP?)]
  [div (fn symbol?) (lhs SEXP?) (rhs SEXP?)]
  [mul (fn symbol?) (lhs SEXP?) (rhs SEXP?)]
  [mod (fn symbol?) (lhs SEXP?) (rhs SEXP?)])

(define (parse num-context sexp)
  (cond
    [(number? sexp) (get-number num-context sexp)]
    [(string? sexp) (str sexp)]
    [(list? sexp)
     (case (first sexp)
       [(int32) (parse 'int32 (second sexp))]
       [(num) (parse 'num (second sexp))]
       [(if) (if-then (parse num-context (second sexp))
                     (parse num-context (third sexp))
                     (parse num-context (fourth sexp)))]
       [(+) (add num-context 
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))]
       [(-) (sub num-context
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))]
       [(*) (mul num-context
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))]
       [(/) (div num-context
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))]
       [(%) (mod num-context 
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))])]))

(define (new-num n)
  (num (floor n)))

(define (as-num n)
  (type-case SEXP n
    [num (n) (num n)]
    [int32 (n) (new-num n)]
    [else (error "Expected numeric type")]))

(define (new-int32 n)
  (int32 (floor (- (modulo (+ n 2147483648)
                         4294967296)
                 2147483648))))

(define (as-int32 n)
  (type-case SEXP n
    [num (n) (new-int32 n)]
    [int32 (n) (int32 n)]
    [else (error "Excepted numeric type")]))

(define (get-number num-context n)
  (if (eq? (floor n) n)
      (case num-context
        [(num) (new-num n)]
        [(int32) 
         (if (or (> n 2147483647)
                 (< n -2147483648))
             (error "Numeric constant is out of bounds")
             (new-int32 n))])
      (error "Expected integer type")))

(define (get-add num-context)
  (case num-context
    [(num) (lambda (a b) (new-num 
                          (+ (interp (as-num a)) (interp (as-num b)))))]
    [(int32) (lambda (a b) (new-int32 
                            (+ (interp (as-int32 a)) (interp (as-int32 b)))))]))

(define (get-sub num-context)
  (case num-context
    [(num) (lambda (a b) (new-num (- (interp (as-num a)) (interp (as-num b)))))]
    [(int32) (lambda (a b) (new-int32 (- (interp (as-int32 a)) (interp (as-int32 b)))))]))

(define (get-mul num-context)
  (case num-context
    [(num) (lambda (a b) (new-num (* (interp (as-num a)) (interp (as-num b)))))]
    [(int32) (lambda (a b) (new-int32 (* (interp (as-int32 a)) (interp (as-int32 b)))))]))

(define (get-div num-context)
  (case num-context
    [(num) (lambda (a b) (new-num (/ (interp (as-num a)) (interp (as-num b)))))]
    [(int32) (lambda (a b) (new-int32 (/ (interp (as-int32 a)) (interp (as-int32 b)))))]))

(define (get-mod num-context)
  (case num-context
    [(num) (lambda (a b) (new-num (modulo (interp (as-num a)) (interp (as-num b)))))]
    [(int32) (lambda (a b) (new-int32 (modulo (interp (as-int32 a)) (interp (as-int32 b)))))]))

(define (interp sexp)
  (type-case SEXP sexp
    [num (n) n]
    [int32 (n) n]
    [str (s) s]
    [if-then (i t e) (if (interp i) (interp t) (interp e))]
    [add (env lhs rhs) (interp ((get-add env) lhs rhs))]
    [sub (env lhs rhs) (interp ((get-sub env) lhs rhs))]
    [mul (env lhs rhs) (interp ((get-mul env) lhs rhs))]
    [div (env lhs rhs) (interp ((get-div env) lhs rhs))]
    [mod (env lhs rhs) (interp ((get-mod env) lhs rhs))]))

(define (repl)
  (interp (parse 'num (read))))