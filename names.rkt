#lang racket

(require "unique.rkt")
(provide (all-defined-out))

(struct source-name (symbol) #:transparent)
(struct module-name (module name) #:transparent)
(struct local-name (uniq) #:transparent)

(define (fresh-name (hint 'fresh))
 (cond
  ((symbol? hint) (local-name (gen-uniq hint)))
  ((source-name? hint) (local-name (gen-uniq (source-name-symbol hint))))
  ((local-name? hint) (local-name (re-uniq (local-name-uniq hint))))
  (else (error 'fresh-name "Unsuported hint ~a" hint))))
