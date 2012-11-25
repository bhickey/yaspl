#lang racket

(require 
  "parser.rkt"
  "resolve-module.rkt"
  "lift-module.rkt"
  (prefix-in le: "lifted-evaluator.rkt"))




(define (read-yaspl-file path)
  (call-with-input-file path
    (lambda (port)
      (port-count-lines! port)
      (parse-yaspl (read-syntax path port)))))

(define unit-module (read-yaspl-file "yaspl/unit.ysp"))
(define bool-module (read-yaspl-file "yaspl/bool.ysp"))
(define bool2-module (read-yaspl-file "yaspl/bool2.ysp"))
(define maybe-module (read-yaspl-file "yaspl/maybe.ysp"))
(define nat-module (read-yaspl-file "yaspl/natural.ysp"))
(define list-module (read-yaspl-file "yaspl/list.ysp"))
(define lists-module (read-yaspl-file "yaspl/lists.ysp"))
(define bool-program1 (read-yaspl-file "yaspl/bool-prog1.ysp"))
(define bool-program2 (read-yaspl-file "yaspl/bool-prog2.ysp"))
(define color-module (read-yaspl-file "yaspl/color.ysp"))
(define modules (list bool-module unit-module bool2-module maybe-module nat-module list-module lists-module))

(define lifted-modules (map lift-module (resolve-modules modules)))

(define (run fun arg)
  (le:run (le:initialize-program lifted-modules fun arg)))

(run '(bool main) '(bool True))
(run '(bool2 main) '(bool2 arg))
(run '(maybe isJust) '(maybe nothing))
(run '(maybe main) '(bool True))
(run '(nat main) '(nat zero))
(run '(list single) '(bool True))
(run '(lists app-to-bool-list) '(list reverse))


