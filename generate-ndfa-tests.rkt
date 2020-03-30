#lang racket
(require fsm)
(require test-engine/racket-tests)
(require "better-sm-test.rkt")

; generate-ndfa-tests: ndfa --> (listof word)
; Purpose: To generate the words needed to test every state of the given dfa
(define (generate-ndfa-tests ndfa)
  (remove-EMP (new-inputs (test-inputs ndfa))))

(check-expect (generate-ndfa-tests KLEENESTAR-abUaba)
              '(() (a b) (a b a)))
(check-expect (generate-ndfa-tests AT-LEAST-ONE-MISSING)
              '(() (c) (a) (b)))
(check-expect (generate-ndfa-tests A)
              '(() (a)))
(check-expect (generate-ndfa-tests B)
              '(() (b)))


(test) 