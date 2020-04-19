#lang racket
(require fsm test-engine/racket-tests)

#|

L = aa* U ab*

S: nothing consumed [START]

A: ci starts with a and then has 0 or more a's [FINAL]

B: ci = a

C: ci starts with a and then has 0 or more b's [FINAL]

|#

(define (S-INV ci) (empty? ci))

(define (A-INV ci) (andmap (λ (s) (eq? s 'a)) ci))

(define (B-INV ci) (and (= (length ci) 1) (eq? (first ci) 'a)))

(define (C-INV ci) (and (eq? (first ci) 'a) (andmap (λ (s) (eq? s 'b)) (cdr ci))))


(define aa*Uab* (make-ndfa '(S A B C)
                           '(a b)
                           'S
                           '(A C)
                           `((S a A)
                             (S a B)
                             (A a A)
                             (B ,EMP C)
                             (C b C))))

(check-expect (sm-apply aa*Uab* '()) 'reject)
(check-expect (sm-apply aa*Uab* '(a a b a)) 'reject)
(check-expect (sm-apply aa*Uab* '(a a a a a)) 'accept)
(check-expect (sm-apply aa*Uab* '(a a b b b)) 'reject)
(check-expect (sm-apply aa*Uab* '(a b b b)) 'accept)

(sm-visualize aa*Uab* (list 'S S-INV) (list 'A A-INV) (list 'B B-INV) (list 'C C-INV))


#|
             a           b
L = {S}    M = {A B C}   ds

M          N = {A}       O = {C}

N          N             ds

O          ds            O

START STATE: L

FINAL STATES: M, N, O
|#

(define L-INV S-INV)

(define (M-INV ci) (or (A-INV ci) (B-INV ci) (C-INV ci)))

(define N-INV A-INV)

(define O-INV C-INV)

(define (DS-INV ci) (and (not (L-INV ci))
                         ;not empty
                         (not (M-INV ci))
                         ; (and (not ci=a^+) (or (len ci)!=1 ci=b)
                         (not (N-INV ci))
                         ; (not ci=a^+)
                         (not (O-INV ci))
                         ; (not ci=ab*)
                         ))

(define aa*Uab*-dfa (make-dfa '(L M N O)
                              '(a b)
                              'L
                              '(M N O)
                              '((L a M)
                                (M a N)
                                (M b O)
                                (N a N)
                                (O b O))))

(check-expect (sm-apply aa*Uab*-dfa '()) 'reject)
(check-expect (sm-apply aa*Uab*-dfa '(a a b a)) 'reject)
(check-expect (sm-apply aa*Uab*-dfa '(a a a a a)) 'accept)
(check-expect (sm-apply aa*Uab*-dfa '(a a b b b)) 'reject)
(check-expect (sm-apply aa*Uab*-dfa '(a b b b)) 'accept)

;(sm-visualize aa*Uab*-dfa (list 'L L-INV) (list 'M M-INV) (list 'N N-INV) (list 'O O-INV))

;(sm->grammar aa*Uab*-dfa)
#|
(rg
 '(DS L M N O)
 '(a b)
 (list
  (crule 'L 'a 'M)
  (crule 'M 'a 'N)
  (crule 'M 'b 'O)
  (crule 'N 'a 'N)
  (crule 'O 'b 'O)
  (crule 'DS 'a 'DS)
  (crule 'DS 'b 'DS)
  (crule 'L 'b 'DS)
  (crule 'N 'b 'DS)
  (crule 'O 'a 'DS)
  (srule 'L 'a)
  (srule 'M 'a)
  (srule 'M 'b)
  (srule 'N 'a)
  (srule 'O 'b))
 'L)
|#
          
                     
(test)
