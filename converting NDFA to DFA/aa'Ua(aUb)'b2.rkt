#lang racket
(require fsm test-engine/racket-tests)

#|

L = aa* U a(aUb)*b

Q0: nothing consumed  [START]

Q1: the inputs in ci are all equal to a [FINAL]

Q2: checks if the first is a and the last is a or b

Q3: ci starts with a, has 0 or more a's and b's, and the last ci is b [FINAL]

|#

(define aa*Ua-aUb*-b2
  (make-ndfa '(Q0 Q1 Q2 Q3)
             '(a b)
             'Q0
             '(Q1 Q3)
             `((Q0 a Q1)
               (Q0 a Q2)
               (Q1 a Q1)
               (Q2 a Q2)
               (Q2 b Q2)
               (Q2 b Q3))
             'no-dead))

             
(check-expect (sm-apply aa*Ua-aUb*-b2 '()) 'reject)
(check-expect (sm-apply aa*Ua-aUb*-b2 '(b a a b)) 'reject)
(check-expect (sm-apply aa*Ua-aUb*-b2 '(a)) 'accept)
(check-expect (sm-apply aa*Ua-aUb*-b2 '(a a a)) 'accept)
(check-expect (sm-apply aa*Ua-aUb*-b2 '(a a b b)) 'accept)


;checks if ci is empty
(define (Q0-INV ci) (empty? ci))

;checks if all of the inputs in ci are equal to 'a
(define (Q1-INV ci) (and
                     (>= (length ci) 1)
                     (andmap (λ (s) (eq? s 'a)) ci)))

;checks if the first is a and the last is a or b
(define (Q2-INV ci) (and
                     (>= (length ci) 1)
                     (eq? (car ci) 'a)
                     (andmap (λ (s) (or (eq? s 'a)
                                        (eq? s 'b))) (drop-right 1 (cdr ci)))
                     (or (eq? (last ci) 'a) (eq? (last ci) 'b))))

;checks if the first of the ci is 'a and last of the ci is equal to 'b
(define (Q3-INV ci) (and (>= (length ci) 1) (eq? (car ci) 'a) (eq? (last ci) 'b)))



#|

INSIGHT
  E(q) = {q} U {p | (q, e) |-* (p, e)}

  For a transition (r a s), we must show that for the consumed
  input the INVs hold for all states in E(s).
  [In the vernacular, INV must hold for s and for all states
   reachable from s using only Ɛ-transitions.]

  No two states can have the same INV.


INV PROOF TABLE

        a                   b
Q0     E(Q1) = Q1          ∅
       E(Q2) = Q2

Q1     E(Q1) = Q1           ∅

Q2     E(Q2) = Q2        E(Q2) = Q2
                         E(Q3) = Q3

Q3       ∅                  ∅

Proof by induction on |w| that INVs always hold



Base |w| = 0

|w| = 0 ==> w = Ɛ
We must show that INVs hold for states in E(Q0) = Q0
 w = Ɛ ==> Q0-INV

Inductive Step

Assume: INVs hold for |w| = k
Show: INVs hold for |w| = k+1

|w| = k+1 ==> w = vi, v in (a, b)* and i in (a, b)

For consuming v the INVs hold by IH

Must show for (p i q) that INVs for states in E(q) hold given M-INV
holds and i is consumed:

Q0-INV && (Q0 a Q1))
  Q0-INV ==> v = Ɛ
         ==> ci=a
         ==> |ci| >=1  && ci has only a's
         ==> Q1-INV

Q0-INV && (Q0 a Q2))
 Q0-INV ==> v = Ɛ
        ==> ci=a
        ==> |ci| >= 1 && (first ci) = a && (last ci) = a OR b
        ==> Q2-INV

Q1-INV && (Q1 a Q1)
  Q1-INV ==> v = a+
         ==> ci = a+
         ==> Q1-INV

Q2-INV && (Q2 a Q2)
 Q2-INV  ==> v = a(aUb)*
         ==> ci = a(aUb)*a
         ==> (first ci) = a && (last b) = b && all input between first and last are a or b
         ==> Q2-INV

Q2-INV && (Q2 b Q2)
 Q2-INV  ==> v = a(aUb)*
         ==> ci = a(aUb)*b
         ==> (first ci) = a && (last b) = b && all input between first and last are a or b
         ==> Q2-INV

Q2-INV && (Q2 b Q3)
 Q2-INV  ==> v = a(aUb)*
         ==> ci = a(aUb)*b
         ==> (first ci) = a && (last b) = b && all input between first and last are a or b
         ==> Q3-INV


Q.E.D.

L(M) = aa* U ab*

1. w in L(M) <==> w in L

   w in L(M) ==> M ends in A or C
     INVs always hold ==> w = aa* || w = ab^*
                      ==> w in L

   w in L ==> w in L(M)
     w in L ==> w = aa* || w = ab^*
            ==> A-INV || C-INV
     INVs always hold && A-INV || C-INV ==> M ends in A or in C, given all INVs are unique
                                        ==> w in L(M)

2. w not in L(M) <==> w not in L
 
   w not in L(M) ==> M does not end in A or C
     INVs always hold ==> w not in L, given all INVs being unique

   w not in L ==> w not in L(M)

   w not in L ==> w != aa* && w != ab*
   INVs always hold && w != aa* && w != ab* ==> M does not end in A or C
                                            ==> w not in L(M)

Q.E.D.
|#


(test)


             
