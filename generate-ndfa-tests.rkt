#lang racket
(require fsm test-engine/racket-tests "better-sm-test.rkt" "ndfas.rkt")
(provide generate-ndfa-tests)

; generate-ndfa-tests: ndfa --> (listof word)
; Purpose: To generate the words needed to test every state of the given dfa
(define (generate-ndfa-tests ndfa)
  (new-inputs (test-inputs (ndfa->dfa ndfa))))

#|
(check-expect (generate-ndfa-tests KLEENESTAR-abUaba)
              '(() (a b) (a b a)))
(check-expect (generate-ndfa-tests AT-LEAST-ONE-MISSING)
              '(() (c) (a) (b)))
(check-expect (generate-ndfa-tests A)
              '(() (a)))
(check-expect (generate-ndfa-tests B)
              '(() (b)))
|#

(test) 