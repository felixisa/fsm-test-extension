#lang racket
(require fsm test-engine/racket-tests)

#|
L = aa* U ab*

S: nothing consumed

A: ci starts with a and then has 0 or more a's FINAL

B: ci = a

C: ci starts with a and then has 0 or more b's FINAL
|#

(define (S-INV ci)
  (null? ci))

(define (A-INV ci)
  (andmap (lambda (i) (eq? i 'a)) ci))

(define (B-INV ci)
  (and (= (length ci) 1)
       (eq? (car ci) 'a)))

(define (C-INV ci)
  (and (eq? (car ci) 'a)
       (andmap (lambda (i) (eq? i 'b)) (cdr ci))))

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
(check-expect (sm-apply aa*Uab* '(a b b b a a)) 'reject)
(check-expect (sm-apply aa*Uab* '(b a)) 'reject)
(check-expect (sm-apply aa*Uab* '(a)) 'accept)
(check-expect (sm-apply aa*Uab* '(a a a a)) 'accept)
(check-expect (sm-apply aa*Uab* '(a b)) 'accept)
(check-expect (sm-apply aa*Uab* '(a b b b b)) 'accept)
#|
(S a A)
(S a B)
(A a A)
(B ,EMP C)
(C b C)

                 a             b
L={S}            M={A B C}     ds

M                N={A}         O={C}

N                N             ds

O                ds            O

START STATE: L
FINAL STATES: M, N, O
|#

(define L-INV S-INV)

(define (M-INV ci)
  (or (A-INV ci)
      (B-INV ci)
      (C-INV ci)))

(define N-INV A-INV)

(define O-INV C-INV)

(define (DS-INV ci)
  (and (not (L-INV ci))
       ; not empty
       (not (M-INV ci))
       ; (and (not ci=a^+) (or (len ci)!=1 ci=b) (not ci=ab*))
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
(check-expect (sm-apply aa*Uab*-dfa '(a b b b a a)) 'reject)
(check-expect (sm-apply aa*Uab*-dfa '(b a)) 'reject)
(check-expect (sm-apply aa*Uab*-dfa '(a)) 'accept)
(check-expect (sm-apply aa*Uab*-dfa '(a a a a)) 'accept)
(check-expect (sm-apply aa*Uab*-dfa '(a b)) 'accept)
(check-expect (sm-apply aa*Uab*-dfa '(a b b b b)) 'accept)

(test)