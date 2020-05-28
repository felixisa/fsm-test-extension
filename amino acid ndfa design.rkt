#lang racket
(require fsm test-engine/racket-tests)

;;f is used to represent any amino acid except m 

; STEP 1
; name: detect-motif      Σ: ((f*g)Um)a?dt^+aa(kUsUt)a*
; STEP 2
(check-expect (sm-apply detect-motif '()) 'reject)
(check-expect (sm-apply detect-motif '(m d t t t a a q k)) 'accept)
(check-expect (sm-apply detect-motif '(m a d t t t a a m s)) 'accept)
(check-expect (sm-apply detect-motif '(g d t a a n t)) 'accept)
(check-expect (sm-apply detect-motif '(m d a a g k)) 'reject)
(check-expect (sm-apply detect-motif '(g d t t t t t t t a a n k)) 'accept)
(check-expect (sm-apply detect-motif '(f f f m a d t a a m k)) 'reject)
(check-expect (sm-apply detect-motif '(f g a d t a a m k)) 'accept)
(check-expect (sm-apply detect-motif '(m a d t a a m k)) 'accept)
; STEP 3
#|
Q0: nothing consumed

Q1: ci=(f*g)Um

Q2: ci=((f*g)Um)a?

Q3: ci=((f*g)Um)a?dt*

Q4: ci=((f*g)Um)a?dt^+

Q5: ci=((f*g)Um)a?dt^+a

Q6: ci=((f*g)Um)a?dt^+aa

Q7: ci=((f*g)Um)a?dt^+aa(mUnUq)

Q8: ci=((f*g)Um)a?dt^+aa(kUsUt)a*

Q9: ci=f*
|#
; STEP 4
(define (Q0-INV ci)
  (empty? ci))

(define (Q1-INV ci)
  (if (= (length ci) 1)
      (eq? (first ci) 'm)
      (and (andmap (λ (i) (eq? i 'f)) (take ci (sub1 (length ci))))
           (eq? (last ci) 'g))))

(define (Q2-INV ci)
  (and (Q1-INV (takef ci (λ (i) (not (eq? i 'a)))))
       (or (eq? (last ci) 'g)
           (eq? (last ci) 'm)
           (eq? (last ci) 'a))))

(define (Q3-INV ci)
  (and (Q2-INV (takef ci (λ (i) (not (eq? i 'd)))))
       (not (empty? (dropf ci (λ (i) (not (eq? i 'd))))))
       (andmap (λ (i) (eq? i 't)) (rest (dropf ci (λ (i) (not (eq? i 'd)))))))) 

(define (Q4-INV ci)
  (and (Q3-INV (takef ci (λ (i) (not (eq? i 't)))))
       (not (empty? (takef-right ci (λ (i) (eq? i 't)))))))

(define (Q5-INV ci)
  (and (Q4-INV (take ci (sub1 (length ci))))
       (eq? (last ci) 'a)))

(define (Q6-INV ci)
  (and (Q5-INV (take ci (sub1 (length ci))))
       (eq? (last ci) 'a)))

(define (mUnUq? w)
  (or (eq? w 'm)
      (eq? w 'n)
      (eq? w 'q)))

(define (Q7-INV ci)
  (and (Q6-INV (take ci (sub1 (length ci))))
       (mUnUq? (last ci))))

(define (kUsUt? w)
  (or (eq? w 'k)
      (eq? w 's)
      (eq? w 't)))

(define (Q8-INV ci)
  (and (Q7-INV (dropf-right ci (λ (i) (or (kUsUt? i)
                                          (eq? i 'a)))))
       (not (empty? (takef-right ci (λ (i) (or (kUsUt? i)
                                               (eq? i 'a))))))
       (andmap (λ (i) (eq? i 'a)) (takef-right ci (λ (i) (not (kUsUt? i)))))))

(define (Q9-INV ci)
  (andmap (λ (i) (eq? i 'f)) ci))
; STEPS 5 and 6
(define detect-motif
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9)
             '(a d g f k m n q s t)
             'Q0
             '(Q8)
             `((Q0 ,EMP Q9)
               (Q0 m Q1)
               (Q9 f Q9)
               (Q1 a Q2)
               (Q1 ,EMP Q2)
               (Q2 d Q3)
               (Q3 t Q4)
               (Q3 t Q3)
               (Q4 a Q5)
               (Q5 a Q6)
               (Q6 m Q7)
               (Q6 n Q7)
               (Q6 q Q7)
               (Q7 k Q8)
               (Q7 s Q8)
               (Q7 t Q8)
               (Q8 a Q8)
               (Q9 g Q1))
             'no-dead))
; STEP 7
#|
E(Q0)=Q0,Q9
E(Q1)=Q1,Q2
E(Q2)=Q2
E(Q3)=Q3
E(Q4)=Q4
E(Q5)=Q5
E(Q6)=Q6
E(Q7)=Q7
E(Q8)=Q8
E(Q9)=Q9
|#
; STEP 8
#|
Proof by induction on |w| that INVs always hold

Base |w| = 0
|w| = 0 ==> w = Ɛ
We must show that INVs hold for states in E(Q0) = Q0
|w| = Ɛ ==> Q0-INV

Inductive Step
Assume: INVs hold for |w| = k
Show: INVs hold for |w| = k+1
|w| = k+1 ==> w = vi, v in (a, d, f, g, k, m, n, q, s, t)* and i in (a, d, f, g, k, m, n, q, s, t)

For consuming v the INVs hold by IH
Must show for (p i q) that INVs for states in E(q) hold given p-INV
holds and i is consumed:

Q0-INV && (Q0 Ɛ Q9)
Q0-INV ==> v=empty
       ==> i=Ɛ
       ==> ci=f*
       ==> Q9-INV 

Q0-INV && (Q0 m Q1)
Q0-INV ==> v=empty
       ==> i=m
       ==> ci=m
       ==> ci=(f*g)Um
       ==> Q1-INV

    ci=m ==> i=Ɛ
         ==> ci=((f*g)Um)a?
         ==> Q2-INV

Q1-INV && (Q1 a Q2)
Q1-INV ==> v=(f*g)Um
       ==> i=a
       ==> ci=((f*g)Um)a?
       ==> Q2-INV

Q2-INV && (Q2 d Q3)
Q2-INV ==> v=((f*g)Um)a?
       ==> i=d
       ==> ci=((f*g)Um)a?dt*
       ==> Q3-INV

Q3-INV && (Q3 t Q3)
Q3-INV ==> v=((f*g)Um)a?dt*
       ==> i=t
       ==> ci=((f*g)Um)a?dt*
       ==> Q3-INV

Q3-INV && (Q3 t Q4)
Q3-INV ==> v=((f*g)Um)a?dt*
       ==> i=t
       ==> ci=((f*g)Um)a?dt^+
       ==> Q4-INV

Q4-INV && (Q4 a Q5)
Q4-INV ==> v=((f*g)Um)a?dt^+
       ==> i=a
       ==> ci=((f*g)Um)a?dt^+a
       ==> Q5-INV

Q5-INV && (Q5 a Q6)
Q5-INV ==> v=((f*g)Um)a?dt^+a
       ==> i=a
       ==> ci=((f*g)Um)a?dt^+aa
       ==> Q6-INV

Q6-INV && (Q6 m Q7)
Q6-INV ==> v=((f*g)Um)a?dt^+aa
       ==> i=m
       ==> ci=((f*g)Um)a?dt^+aa(mUnUq)
       ==> Q7-INV

Q6-INV && (Q6 n Q7)
Q6-INV ==> v=((f*g)Um)a?dt^+aa
       ==> i=n
       ==> ci=((f*g)Um)a?dt^+aa(mUnUq)
       ==> Q7-INV

Q6-INV && (Q6 q Q7)
Q6-INV ==> v=((f*g)Um)a?dt^+aa
       ==> i=q
       ==> ci=((f*g)Um)a?dt^+aa(mUnUq)
       ==> Q7-INV

Q7-INV && (Q7 k Q8)
Q7-INV ==> v=((f*g)Um)a?dt^+aa(mUnUq)
       ==> i=k
       ==> ci=((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
       ==> Q8-INV

Q7-INV && (Q7 s Q8)
Q7-INV ==> v=((f*g)Um)a?dt^+aa(mUnUq)
       ==> i=s
       ==> ci=((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
       ==> Q8-INV

Q7-INV && (Q7 t Q8)
Q7-INV ==> v=((f*g)Um)a?dt^+aa(mUnUq)
       ==> i=t
       ==> ci=((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
       ==> Q8-INV

Q8-INV && (Q8 a Q8)
Q8-INV ==> v=((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
       ==> i=a
       ==> ci=((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
       ==> Q8-INV

Q9-INV && (Q9 f Q9)
Q9-INV ==> v=f*
       ==> i=f
       ==> ci=f*
       ==> Q9-INV

Q9-INV && (Q9 g Q1)
Q9-INV ==> v= empty OR f^+
       ==> i=g
       ==> ci=(f*g)Um
       ==> Q1-INV

Q.E.D.


Prove: L(M) <==> L = ((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*

1. w in L(M) <==> w in L

   w in L(M) ==> M ends in Q8
   INVs always hold ==> w = ((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
                    ==> w in L


   w in L ==> w in L(M)
   w in L ==> w = ((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
   INVs always hold ==> M ends in Q8
                    ==> w in L(M)

2. w not in L(M) <==> w not in L

   w not in L(M) ==> M does not end in Q8
   INVs always hold ==> w not in L


   w not in L ==> w not in L(M)
   w not in L ==> w != ((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
   INVs always hold ==> M does not end in Q8
                    ==> w not in L(M)
   
Q.E.D.
|#

(sm-graph detect-motif)

(test)