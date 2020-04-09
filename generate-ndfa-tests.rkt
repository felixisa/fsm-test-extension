#lang racket
(require fsm test-engine/racket-tests "better-sm-test.rkt" "ndfas.rkt")
(provide generate-ndfa-tests)

; generate-ndfa-tests: ndfa --> (listof word)
; Purpose: To generate the words needed to test every state of the given dfa
(define (generate-ndfa-tests ndfa)
  (new-inputs (test-inputs (ndfa->dfa ndfa))))

(define (sm-test-ndfa m)
  (define inputs (generate-ndfa-tests m))
  (map (lambda (x) (list x (sm-apply m x))) inputs))

(test) 