#lang racket

(provide (all-defined-out))

(struct case-clause (pattern body) #:transparent)
(struct nobind-pattern () #:transparent)
(struct identifier-pattern (name) #:transparent)
(struct constructor-pattern (pattern tag field-patterns) #:transparent)



(define (pattern-bound-variables pattern)
 (define (recur pattern)
  (match pattern
   ((nobind-pattern) (set))
   ((identifier-pattern name) (set name))
   ((constructor-pattern pattern tag patterns)
    (apply set-union (set) (map recur patterns)))))
 (recur pattern))
