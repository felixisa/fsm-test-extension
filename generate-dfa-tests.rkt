#lang racket
(require fsm test-engine/racket-tests "better-sm-test.rkt" "dfas.rkt")
(provide generate-dfa-tests)

; generate-dfa-tests: dfa --> (listof word)
; Purpose: To generate the words needed to test every state of the given dfa
(define (generate-dfa-tests dfa)
  (new-inputs (test-inputs dfa)))


(check-expect (generate-dfa-tests TEST-MACHINE)
              '(() (a a) (b a) (b b) (a b a) (a b b)))
(check-expect (generate-dfa-tests a-bc*-d)
              '(() (c) (d) (a b) (a c) (a a) (b a) (b b) (b c) (b d) (a d a) (a d b) (a d c) (a d d)))
(check-expect (generate-dfa-tests STATES)
              '(() (a b) (b a) (a a a) (a a b) (b b a) (b b b)))
(check-expect (generate-dfa-tests ALPHA)
              '(() (a) (b) (c a) (c b) (c c a) (c c b) (c c c)))

(define (sm-test-dfa m)
  (define inputs (generate-dfa-tests m))
  (map (lambda (x) (list x (sm-apply m x))) inputs))

(test)