#lang racket
(require fsm test-engine/racket-tests)

#|

L = a* U b* U c*

S: nothing consumed   [START]

A: has 0 or more a's  [FINAL]

B: has 0 or more b's  [FINAL]

C: has 0 or more c's  [FINAL]

|#

;checks if ci is empty
(define (S-INV ci) (empty? ci))

;checks if all ci is equal to 'a
(define (A-INV ci) (andmap (λ (s) (eq? s 'a)) ci))

;checks if all ci is equal to 'b
(define (B-INV ci) (andmap (λ (s) (eq? s 'b)) ci))

;checks if all ci is equal to 'c
(define (C-INV ci) (andmap (λ (s) (eq? s 'c)) ci))

;was going to check if ci is not equal to 'a,'b, or 'c but i don't think i need to
#;(define (D-INV ci) (or (not (C-INV ci))
                         (not (A-INV ci))
                         (not (B-INV ci))))        ;;;;;;; do i need this??

(define a*Ub*Uc*
  (make-ndfa '(S A B C)
             '(a b c)
             'S
             '(A B C) 
             `((S ,EMP A)
               (S ,EMP B)
               (S ,EMP C)
               (A a A)
               (B b B)
               (C c C))))

;(sm-visualize a*Ub*Uc* (list 'S S-INV) (list 'A A-INV) (list 'B B-INV) (list 'C C-INV))

(check-expect (sm-apply a*Ub*Uc* '()) 'accept)
(check-expect (sm-apply a*Ub*Uc* '(a)) 'accept)
(check-expect (sm-apply a*Ub*Uc* '(b)) 'accept)
(check-expect (sm-apply a*Ub*Uc* '(c)) 'accept)
(check-expect (sm-apply a*Ub*Uc* '(a a a)) 'accept)
(check-expect (sm-apply a*Ub*Uc* '(a b)) 'reject)
(check-expect (sm-apply a*Ub*Uc* '(b b b b c)) 'reject)


#|

                   a         b        c
L = {S A B C}   M = {A}   N = {B}   O = {C}

      M            M        ds        ds

      N            ds       N         ds

      O            ds       ds        O

STARTING STATE: S

FINAL STATES: '(A B C D)

|#

;checks if ci is empty
(define L-INV S-INV)

;checks if all ci is equal to 'a
(define M-INV A-INV)

;checks if all ci is equal to 'b
(define N-INV B-INV)

;checks if all ci is equal to 'c
(define O-INV C-INV)

;checks if all ci is not equal to 'a, 'b, or 'c
(define (DS-INV ci) (and (not (M-INV ci))
                         ;(not ci=a*)
                         (not (N-INV ci))
                         ;(not ci=b*)
                         (not (O-INV ci))
                         ;(not ci=c*)
                         ))                          ;;whats the point in DS-INV if we dont use it??


(define DFA
  (make-dfa '(S A B C)
            '(a b c)
            'S
            '(S A B C)
            '((S a A)
              (S b B)
              (S c C)
              (A a A)
              (B b B)
              (C c C))))


(check-expect (sm-apply DFA '()) 'accept)
(check-expect (sm-apply DFA '(a)) 'accept)
(check-expect (sm-apply DFA '(b)) 'accept)
(check-expect (sm-apply DFA '(c)) 'accept)
(check-expect (sm-apply DFA '(a a a)) 'accept)
(check-expect (sm-apply DFA '(a b)) 'reject)
(check-expect (sm-apply DFA '(b b b b c)) 'reject)

   
(sm-visualize DFA (list 'L L-INV) (list 'M M-INV) (list 'N N-INV) (list 'O O-INV))



(test)