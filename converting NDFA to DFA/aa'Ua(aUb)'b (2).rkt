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
                     (or (eq? (last ci) 'a) (eq? (last ci) 'b))))

;checks if the first of the ci is 'a and last of the ci is equal to 'b
(define (Q3-INV ci) (and (>= (length ci) 1) (eq? (car ci) 'a) (eq? (last ci) 'b)))


#|
                a               b
L = {Q0}   M = {Q1, Q2}         ds

   M            M          N = {Q2, Q3}

   N         O = {Q2}           N

   O            O               N


STARTING STATE: L

FINAL STATES: '(M N O)


L-INV = Q0-INV
M-INV = Q1-INV & Q2-INV
N-INV = Q2-INV & Q3-INV
O-INV = Q2-INV
ds-INV: w not in L = aa* U a(aUb)*b


INSIGHT
  If the INV of the destination state is not always true
  we must show there is a different path that ends in a 
  state for which the INV is true.

Proof by induction on |w|

Base |w| = 0

|w| = 0 ==> |w| = Ɛ ==> Q0-INV = L-INV

Inductive Step

Assume: INVs hold for |w| = k
Show: INVs hold for |w| = k+1

|w| = k+1 ==> w = vi, v in (a, b)* and i in (a, b)

For consuming v the INVs hold by IH

Must show for (M i N) that N-INV holds given M-INV
and consuming i:

L-INV && (L a M)
  ==> ci=a ==> Q1-INV
           ==> Q2-INV
           ==> M-INV

L-INV && (L b ds)
  L-INV ==> v = Ɛ
  ==> ci=b 
  ==> w starts with b 
  ==> w not in L = ds-INV [w starts with b]


M-INV && (M a M)
  Q1-INV && Q2-INV ==> v = a
                   ==> ci = aa
                   ==> Q1-INV & Q2-INV
                   ==> M-INV

M-INV && (M b N)
  Q2-INV && Q3-INV ==> v = a
                   ==> ci = ab 
                   ==> Q3-INV
                   ==> N-INV

N-INV && (N a O)
  X-INV ==> v = a* V a*(aUb)*
        ==> ci = a*(aUb)* a
        ==> Q2-INV
        ==> O-INV

N-INV && (N b N)
  N-INV ==> v = a* V a*(aUb)*
        ==> ci = a*(aUb)* b
        ==> Q3-INV
        ==> N-INV

O-INV && (O a O)
  O-INV ==> v = a* V a*(aUb)*
        ==> ci = a*(aUb)* a
        ==> Q2-INV
        ==> O-INV

O-INV && (O b N) 
  N-INV ==> v = a* V a*(aUb)*
        ==> ci = a*(aUb)* b
        ==> Q3-INV
	==> N-INV

Q.E.D.

L(M) =  aa* U a(aUb)*b

1. w in L(M) <==> w in L

   w in L(M) ==> M ends in M, N, or O
   INVs always hold ==> w = aa* V w = a(aUb)*b
                    ==> w in L

   w in L ==> w in L(M)
   w in L ==> w = aa* V w = a(aUb)*b
   INVs always hold ==> M ends in M, N, or O
                    ==> w in L(M)

2. w not in L(M) <==> w not in L
 
   w not in L(M) ==> M does not end in M, N, or O
   INVs always hold ==> w not in L

   w not in L ==> w not in L(M)

   w not in L ==> w != aa* && w != a(aUb)*b
              ==> M does not end in M, N, or O
              ==> w not in L(M)

Q.E.D.
|#



(test)


             
