#lang racket
(require fsm test-engine/racket-tests)



#|

L = (ab U aba)*

S = 

A =

B =

STARTING STATE: S

FINALS STATE: '(S)
|#

;S-INV

;A-INV

;B-INV

;accepts all inputs that only contain the pattern ab and/or aba
(define abUaba*
  (make-ndfa '(Q0 Q1 Q2)
             '(a b)
             'Q0
             '(Q0)
             `((Q0 a Q1)
               (Q1 b Q0)
               (Q1 b Q2)
               (Q2 a Q0))))


(define abUaba*2
  (make-ndfa  '(Q0 Q1 Q2)
             '(a b)
             'Q0
             '(Q0)
             `((Q0 a Q1)
               (Q1 b Q2)
               (Q2 a Q0)
               (Q2 ,EMP Q0))))

(sm-testequiv? abUaba* abUaba*2)

(check-expect (sm-apply abUaba* '()) 'accept)
(check-expect (sm-apply abUaba* '(a)) 'reject)
(check-expect (sm-apply abUaba* '(a b)) 'accept)
(check-expect (sm-apply abUaba* '(a b a)) 'accept)
(check-expect (sm-apply abUaba* '(a b b a)) 'reject)
(check-expect (sm-apply abUaba* '(a b a a a)) 'reject)
(check-expect (sm-apply abUaba* '(a b a a b)) 'accept)
(check-expect (sm-apply abUaba* '(a b a b a)) 'accept)
(check-expect (sm-apply abUaba* '(a b a a b a b a b a a b)) 'accept)



;(sm-graph abUaba*)
;(sm-visualize abUaba*)



#|

            a         b
L = {S}    ...       ...

..

..

|#


;;INVARIANTS


(define DFA
  (make-dfa '(S A B C)
            '(a b)
            'S
            '(S B C)
            '((S a A)
              (A b B)
              (B a C)
              (B b S)
              (C a A)
              (C b B))))

;(sm-testequiv? abUaba* DFA)                  ;;why are they returning that
;(sm-testequiv? abUaba*2 DFA)


(check-expect (sm-apply DFA '()) 'accept)
(check-expect (sm-apply DFA '(a)) 'reject)
(check-expect (sm-apply DFA '(a b)) 'accept)
(check-expect (sm-apply DFA '(a b a)) 'accept)
(check-expect (sm-apply DFA '(a b b a)) 'reject)
(check-expect (sm-apply DFA '(a b a a a)) 'reject)
(check-expect (sm-apply DFA '(a b a a b)) 'accept)
(check-expect (sm-apply DFA '(a b a b a)) 'accept)
(check-expect (sm-apply DFA '(a b a a b a b a b a a b)) 'accept)


(test)