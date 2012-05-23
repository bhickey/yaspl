#lang racket

(require "unique.rkt")
(provide (all-defined-out))

(struct source-name (symbol) #:transparent)
(struct module-name (module name) #:transparent)
(struct top-local-name (uniq) #:transparent)
(struct local-name (uniq) #:transparent)

(define (fresh-name (hint 'fresh))
 (cond
  ((symbol? hint) (local-name (gen-uniq hint)))
  ((source-name? hint) (local-name (gen-uniq (source-name-symbol hint))))
  ((top-local-name? hint) (local-name (re-uniq (top-local-name-uniq hint))))
  ((local-name? hint) (local-name (re-uniq (local-name-uniq hint))))
  (else (error 'fresh-name "Unsuported hint ~a" hint))))

(define (fresh-top-name (hint 'fresh))
 (cond
  ((symbol? hint) (top-local-name (gen-uniq hint)))
  ((source-name? hint) (top-local-name (gen-uniq (source-name-symbol hint))))
  ((local-name? hint) (top-local-name (re-uniq (local-name-uniq hint))))
  ((top-local-name? hint) (top-local-name (re-uniq (top-local-name-uniq hint))))
  (else (error 'fresh-top-name "Unsuported hint ~a" hint))))

