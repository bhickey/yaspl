#lang racket

(require 
  "parser.rkt"
  "source-structures.rkt"
  "resolve-module.rkt"
  "lift-module.rkt"
  "old-evaluator.rkt"
  (prefix-in le: "lifted-evaluator.rkt"))




(define (read-yaspl-file path)
  (call-with-input-file path
    (lambda (port)
      (port-count-lines! port)
      (parse-yaspl (read-syntax path port)))))

(define unit-module (read-yaspl-file "yaspl/unit.ysp"))
(define bool-module (read-yaspl-file "yaspl/bool.ysp"))
(define bool2-module (read-yaspl-file "yaspl/bool2.ysp"))
(define bool-program1 (read-yaspl-file "yaspl/bool-prog1.ysp"))
(define bool-program2 (read-yaspl-file "yaspl/bool-prog2.ysp"))
(define color-module (read-yaspl-file "yaspl/color.ysp"))
(define modules (list bool-module unit-module bool2-module))

(define lifted-modules (map lift-module (resolve-modules modules)))

(define (run fun arg)
  (le:run (le:initialize-program lifted-modules fun arg)))

(run '(bool main) '(bool True))
(run '(bool2 main) '(bool2 arg))





#|
#;
(define byte-interface
  (module-interface 'byte
     (list (type-export 'bytes (type-kind)))
     (list (var-export 'byte (id-ty 'bytes))
           (var-export 'concat-bytes (fun-ty (id-ty 'bytes) (fun-ty (id-ty 'bytes) (id-ty 'bytes)))))))
(define module-store (initialize-module-store modules))
#;
(define interfaces (hash-set (modules->module-interfaces modules) 'byte byte-interface))
#;
(for ((mod (list color-module bool-module bool-program1 bool-program2)))
  (check-unbound-variables! mod interfaces))


(interp-program module-store bool-program1)
(interp-program module-store bool-program2)
;; (with-input-from-file "color.rkt" read)
;; (parse-yaspl (with-input-from-file "color.rkt" read))
|#
