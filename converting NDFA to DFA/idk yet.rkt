#lang racket
(require fsm test-engine/racket-tests)

#|

L = aa* U a(ab)*b

S: nothing consumed  [START]

A: ci starts with a, has 0 or more a's and b's, and the last ci is a  [FINAL]

B: ci starts with a, has 0 or more a's and b's, and the last ci is b  [FINAL]

|#

(define (S-INV ci) (empty? ci))

(define (A-INV ci) (and (eq? (first ci) 'a) (eq? (last ci) 'a)))

(define (B-INV ci) (and (eq? (first ci) 'a) (eq? (last ci) 'b)))



(define aa*Ua-ab*-b
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

(sm-visualize aa*Ua-ab*-b (list 'S S-INV) (list 'A A-INV) (list 'B B-INV))



(test)


             
