#lang racket
(require fsm test-engine/racket-tests "better-sm-test.rkt" "ndfas.rkt")
(provide generate-ndfa-tests)

; generate-ndfa-tests: ndfa --> (listof word)
; Purpose: To generate the words needed to test every state of the given dfa
(define (generate-ndfa-tests ndfa)
  (new-inputs (test-inputs (ndfa->dfa ndfa))))

(check-expect (generate-ndfa-tests B) '(() (a a) (a b) (b a) (b b)))
(check-expect (generate-ndfa-tests A) '(() (a a) (a b) (b a) (b b)))
(check-expect (generate-ndfa-tests A*) '(() (a a a)))
(check-expect (generate-ndfa-tests AorB) '(() (a a) (a b) (b a) (b b)))
(check-expect (generate-ndfa-tests AT-LEAST-ONE-MISSING) '(()
                                                           (a a)
                                                           (b a)
                                                           (b b)
                                                           (c a)
                                                           (c b)
                                                           (c c)
                                                           (a b a)
                                                           (a b b)
                                                           (a c a)
                                                           (a c b)
                                                           (a c c)
                                                           (b c a)
                                                           (b c b)
                                                           (b c c)
                                                           (a b c a)
                                                           (a b c b)
                                                           (a b c c)))
(check-expect (generate-ndfa-tests KLEENESTAR-abUaba) '(() (a a) (b a) (b b) (a b b) (a b a a) (a b a b)))


(define (sm-test-ndfa m)
  (define inputs (generate-ndfa-tests m))
  (map (lambda (x) (list x (sm-apply m x))) inputs))


(test) 