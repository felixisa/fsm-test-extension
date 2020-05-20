#lang racket
(require fsm test-engine/racket-tests)


#|

L = abab

S: nothing consumed or the last of the input is a or b

A: when the length is greater than or equal to 1 and the last of the input is '(a)

B: when the length is greater than or equal to 2 and the last of the input is '(a b)

C: when the length is greater than or equal to 3 and the last of the input is '(a b a)

D: when the ci contains the pattern abab FINAL

|#

(define (contains? patt w)
  (and (>= (length w) (length patt))
       (or (equal? (take w (length patt)) patt)
           (contains? patt (rest w)))))

(define (S-INV ci)
  (if (empty? ci)
      #t
      (or (equal? (last ci) 'a)
          (equal? (last ci) 'b))))

(define (A-INV ci)
  (and (>= (length ci) 1)
       (eq? (last ci) 'a)))

(define (B-INV ci)
  (and (>= (length ci) 2)
       (equal? '(a b) (take-right ci 2))))
       
(define (C-INV ci)
  (and (>= (length ci) 3)
       (equal? '(a b a) (take-right ci 3))))

(define (D-INV ci)
  (and (>= (length ci) 4)
       (contains? '(a b a b) ci)))


(define abab (make-ndfa '(S A B C D)
                        '(a b)
                        'S
                        '(D)
                        `((S a S)
                          (S a A)
                          (S b S)
                          (A b B)
                          (B a C)
                          (C b D)
                          (D b D)
                          (D a D))
                        'no-dead))

(check-expect (sm-apply abab '(a a b a b))'accept)
(check-expect (sm-apply abab '(a b a b a))'accept)
(check-expect (sm-apply abab '(a a b a b a))'accept)
(check-expect (sm-apply abab '(a a b a b a a b))'accept)
(check-expect (sm-apply abab '(a a a a b a a b))'reject)
(check-expect (sm-apply abab '(a a b a b a b))'accept)


#|
           a                  b
Z={S}    Y = {S, A}           Z 
  Y        Y              X = {S, B}
  X     W = {S, A, C}         Z
  W        Y              V = {S, B, D}
  V   U = {S, A, C, D}     T = {S, D}
  U        U                  V
  T    S = {S, A, D}          T
  S        S                  V   

Z-INV = S-INV
Y-INV = S-INV & A-INV
X-INV = S-INV & B-INV
W-INV = S-INV & A-INV & C-INV
V-INV = S-INV & B-INV & D-INV
U-INV = S-INV & A-INV & C-INV & D-INV
T-INV = S-INV & D-INV
S-INV = S-INV & A-INV & D-INV

|# 


#|
	a		b
Z=S	Y=A,B,C		ds
Y	X=A		W=C
X	X		ds
W	ds		W
ds	ds		ds


Z-INV = S-INV
Y-INV = A-INV && B-INV && C-INV
X-INV = A-INV
W-INV = C-INV
ds-INV: w not in L = (aa* U ab*)


INSIGHT
  If the INV of the destination state is not always true
  we must show there is a different path that ends in a 
  state for which the INV is true.

Proof by induction on |w|

Base |w| = 0

|w| = 0 ==> |w| = Ɛ ==> S-INV = Z-INV

Inductive Step

Assume: INVs hold for |w| = k
Show: INVs hold for |w| = k+1

|w| = k+1 ==> w = vi, v in (a, b)* and i in (a, b)

For consuming v the INVs hold by IH

Must show for (M i N) that N-INV holds given M-INV
and consuming i:

Z-INV && (Z a Y)
  ==> ci=a ==> A-INV
           ==> B-INV
           ==> C-INV
           ==> Y-INV

Z-INV && (Z b ds)
  Z-INV ==> v = Ɛ
  ==> ci=b 
  ==> w starts with b 
  ==> w not in L = ds-INV [w starts with b]


Y-INV && (Y a A)
  A-INV && C-INV ==> v = a
                 ==> ci = aa
                 ==> A-INV

Y-INV && (Y b W)
  A-INV && C-INV ==> v = a
                 ==> ci = ab
                 ==> C-INV = W-INV

X-INV && (X a X)
  X-INV ==> v = a*
        ==> ci = a^+
        ==> A-INV = X-INV

X-INV && (X b ds) 
  X-INV ==>  v = a*
        ==> ci = a*b
  |v| > 1 ==> w not in L = ds-INV
  |v| = 1 ==> ci = ab
          ==> W-INV = C-INV
          ==> A path from S to C consuming ab must exist
          ==> (S ab) |- (B b) |- (C Ɛ)

W-INV && (W a ds)
  W-INV ==> v = ab*
        ==> ci = ab*a
  |v| = 1 ==> ci = aa
          ==> X-INV = A-INV
          ==> A path from S to A consuming aa must exist
          ==> (S aa) |- (A a) |- (A Ɛ)
  |v| > 1 ==> ci = abb*a
          ==> ds-INV [w has an a after a b]

W-INV && (W b W) 
  W-INV ==> v = ab*
        ==> ci = ab^+
	==> W-INV

Q.E.D.

L(M) = aa* U ab*

1. w in L(M) <==> w in L

   w in L(M) ==> M ends in A or C
   INVs always hold ==> w = aa* V w = ab^*
                    ==> w in L

   w in L ==> w in L(M)
   w in L ==> w = aa* V w = ab^*
   INVs always hold ==> M ends in A or C
                    ==> w in L(M)

2. w not in L(M) <==> w not in L
 
   w not in L(M) ==> M does not end in A or C
   INVs always hold ==> w not in L

   w not in L ==> w not in L(M)

   w not in L ==> w != aa* && w != ab*
              ==> M does not end in A or C
              ==> w not in L(M)

Q.E.D.
|#



(test)
