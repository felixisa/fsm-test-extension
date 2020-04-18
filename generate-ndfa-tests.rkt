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

(define A*
  (make-ndfa '(Q0 Q1 Q2 Q3)
              '(a)
              'Q0
              '(Q1 Q3)
              `((Q0 a Q1)
                (Q1 ,EMP Q0)
                (Q0 a Q2)
                (Q2 a Q3)
                (Q3 ,EMP Q0))))

(define AorB
  (make-ndfa '(Q0 Q1 Q2)
             '(a b)
             'Q0
             '(Q1 Q2)
             `((Q0 a Q1)
               (Q1 ,EMP Q0)
               (Q0 b Q2)
               (Q2 ,EMP Q0))))

(test) 