#lang racket
(require fsm test-engine/racket-tests)

#|

L = aa* U a(aUb)*b

S: nothing consumed  [START]

A: ci starts with a, has 0 or more a's and b's, and the last ci is a  [FINAL]

B: ci starts with a, has 0 or more a's and b's, and the last ci is b  [FINAL]

|#

;checks if ci is empty
(define (S-INV ci) (empty? ci))

;checks if the first and last of the ci is equal to 'a
(define (A-INV ci) (and (eq? (first ci) 'a) (eq? (last ci) 'a)))

;checks if the first of the ci is 'a and last of the ci is equal to 'b
(define (B-INV ci) (and (eq? (first ci) 'a) (eq? (last ci) 'b)))

;simpler machine
(define aa*Ua-ab*-b2
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
             '(a b)
             'Q0
             '(Q1 Q4)
             `((Q0 a Q1)
               (Q0 a Q2)
               (Q1 a Q1)
               (Q2 a Q2)
               (Q2 b Q2)
               (Q2 b Q4))
             'no-dead))

;same machine but my version and not Marco's (i like mine better)
#;(define aa*Ua-ab*-b
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
             '(a b)
             'Q0
             '(Q1 Q4)
             `((Q0 a Q1)
               (Q0 a Q2)
               (Q1 a Q1)
               (Q1 b Q3)
               (Q2 a Q2)
               (Q2 b Q4)
               (Q3 a Q1)
               (Q3 b Q3)
               (Q4 a Q2)
               (Q4 b Q4))))

             
(check-expect (sm-apply aa*Ua-ab*-b '()) 'reject)
(check-expect (sm-apply aa*Ua-ab*-b '(b a a b)) 'reject)
(check-expect (sm-apply aa*Ua-ab*-b '(a)) 'accept)
(check-expect (sm-apply aa*Ua-ab*-b '(a a a)) 'accept)
(check-expect (sm-apply aa*Ua-ab*-b '(a a b b)) 'accept)



;(sm-visualize aa*Ua-ab*-b (list 'S S-INV) (list 'A A-INV) (list 'B B-INV))

;(sm-visualize aa*Ua-ab*-b2)
#|
             a          b
L = {S}   M = {A}       ds

  M          M        N = {B}

  N          M          N


STARTING STATE: S

FINAL STATES: '(A B)

|#

;checks if ci is empty
(define L-INV S-INV)

;checks if the first and last of the ci is equal to 'a
(define M-INV A-INV)

;checks if the first of the ci is 'a and last of the ci is equal to 'b
(define N-INV B-INV)

(define DFA
  (make-dfa '(S A B)
            '(a b)
            'S
            '(A B)
            '((S a A)
              (A a A)
              (A b B)
              (B a A)
              (B b B))))


;(sm-visualize DFA (list 'L L-INV) (list 'M M-INV) (list 'N N-INV))


(test)


             
