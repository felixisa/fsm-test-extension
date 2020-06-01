#lang racket
(require fsm test-engine/racket-tests)

;;f is used to represent any amino acid except m 

; STEP 1
; name: detect-motif      Σ: (a, d, f, g, k, m, n, s, t, q)

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
Q0: nothing consumed [START]

Q1: ci=(f*g) U m

Q2: ci=((f*g) U m) U ((f*g) U m)a

Q3: ci=(((f*g) U m) U ((f*g) U m)a)dt*

Q4: ci=(((f*g) U m) U ((f*g) U m)a)dt^+

Q5: ci=(((f*g) U m) U ((f*g) U m)a)dt^+a

Q6: ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa

Q7: ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)

Q8: ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a* [FINAL]

Q9: ci=f*
|#

; STEP 4
; word -> boolean
(define (Q0-INV ci)
  (empty? ci))

; word -> boolean
(define (Q1-INV ci)
  (or (and (= (length ci) 1)
           (eq? (first ci) 'm))
      (and (>= (length ci) 1)
           (andmap (λ (i) (eq? i 'f)) (take ci (sub1 (length ci))))
           (eq? (last ci) 'g))))

; word -> boolean
(define (Q2-INV ci)
  (and (>= (length ci) 1)
       (Q1-INV (takef ci (λ (i) (not (eq? i 'a)))))
       (or (eq? (last ci) 'g)
           (eq? (last ci) 'm)
           (eq? (last ci) 'a))))

; word -> boolean
(define (Q3-INV ci)
  (and (>= (length ci) 2)
       (Q2-INV (takef ci (λ (i) (not (eq? i 'd)))))
       (andmap (λ (i) (eq? i 't)) (rest (dropf ci (λ (i) (not (eq? i 'd)))))))) 

; word -> boolean
(define (Q4-INV ci)
  (and (>= (length ci) 3)
       (Q3-INV (drop-right ci 1))
       (eq? (last ci) 't)))

; word -> boolean
(define (Q5-INV ci)
  (and (>= (length ci) 4)
       (Q4-INV (drop-right ci 1))
       (eq? (last ci) 'a)))

; word -> boolean
(define (Q6-INV ci)
  (and (>= (length ci) 5)
       (Q5-INV (drop-right ci 1))
       (eq? (last ci) 'a)))

; word -> boolean
(define (Q7-INV ci)
  (and (>= (length ci) 6)
       (Q6-INV (drop-right ci 1))
       (not (empty? (member (last ci) '(m n q))))))

; symbol -> boolean
(define (kUsUt? w)
  (or (eq? w 'k)
      (eq? w 's)
      (eq? w 't)))

; word -> boolean
(define (Q8-INV ci)
  (and (>= (length ci) 7)
       (let ((Q7-word (dropf-right ci (λ (i) (or (eq? i 'k)
                                          (eq? i 's)
                                          (eq? i 't)
                                          (eq? i 'a)))))
             (drop-Q7 (takef-right ci (λ (i) (or (eq? i 'k)
                                          (eq? i 's)
                                          (eq? i 't)
                                          (eq? i 'a))))))
       (and (Q7-INV Q7-word)
            (or (eq? (first drop-Q7) 'k) (eq? (first drop-Q7) 's) (eq? (first drop-Q7) 't))
            (andmap (λ (i) (eq? i 'a)) (rest drop-Q7))))))

; word -> boolean
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
State     E(State)
Q0        Q0,Q9
Q1        Q1,Q2
Q2        Q2
Q3        Q3
Q4        Q4
Q5        Q5
Q6        Q6
Q7        Q7
Q8        Q8
Q9        Q9
|#
; STEP 8
#|
Proof by induction on |w| that INVs always hold

Base |w| = 0
|w| = 0 ==> w = Ɛ
We must show that INVs hold for states in E(Q0) = Q0,Q9
w = Ɛ ==> Q0-INV
w = Ɛ ==> Q9-INV

Inductive Step
Assume: INVs hold for |w| = k
Show: INVs hold for |w| = k+1
|w| = k+1 ==> w = vi, v in (a, d, f, g, k, m, n, q, s, t)* and i in (a, d, f, g, k, m, n, q, s, t)

For consuming v the INVs hold by IH
Must show for (p i q) that INVs for states in E(q) hold given p-INV
holds and i is consumed:

Q0-INV && (Q0 m Q1)
Q0-INV ==> v=empty
       ==> i=m
       ==> ci=m
       ==> ci=(f*g)Um
       ==> Q1-INV

ci=(f*g)Um ==> ci=((f*g) U m) U ((f*g) U m)a
           ==> Q2-INV

Q1-INV && (Q1 a Q2)
Q1-INV ==> v=(f*g)Um
       ==> i=a
       ==> ci=((f*g) U m)a
       ==> ci=((f*g) U m) U ((f*g) U m)a
       ==> Q2-INV

Q2-INV && (Q2 d Q3)
Q2-INV ==> v=((f*g) U m) U ((f*g) U m)a
       ==> i=d
       ==> ci=(((f*g) U m) U ((f*g) U m)a)d
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt*
       ==> Q3-INV

Q3-INV && (Q3 t Q3)
Q3-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt*
       ==> i=t
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt*t
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt*
       ==> Q3-INV

Q3-INV && (Q3 t Q4)
Q3-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt*
       ==> i=t
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt*t
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+
       ==> Q4-INV

Q4-INV && (Q4 a Q5)
Q4-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+
       ==> i=a
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+a
       ==> Q5-INV

Q5-INV && (Q5 a Q6)
Q5-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+a
       ==> i=a
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa
       ==> Q6-INV

Q6-INV && (Q6 m Q7)
Q6-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+aa
       ==> i=m
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+m
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)
       ==> Q7-INV

Q6-INV && (Q6 n Q7)
Q6-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+aa
       ==> i=n
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aan
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)
       ==> Q7-INV

Q6-INV && (Q6 q Q7)
Q6-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+aa
       ==> i=q
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aaq
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)
       ==> Q7-INV

Q7-INV && (Q7 k Q8)
Q7-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)
       ==> i=k
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)k
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*
       ==> Q8-INV

Q7-INV && (Q7 s Q8)
Q7-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)
       ==> i=s
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)s
       ==> ci=((f*g)Um)a?dt^+aa(mUnUq)(kUsUt)a*
       ==> Q8-INV

Q7-INV && (Q7 t Q8)
Q7-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)
       ==> i=t
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)t
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*
       ==> Q8-INV

Q8-INV && (Q8 a Q8)
Q8-INV ==> v=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*
       ==> i=a
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*a
       ==> ci=(((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*
       ==> Q8-INV

Q9-INV && (Q9 f Q9)
Q9-INV ==> v=f*
       ==> i=f
       ==> ci=f*f
       ==> ci=f*
       ==> Q9-INV

Q9-INV && (Q9 g Q1)
Q9-INV ==> v=f*
       ==> i=g
       ==> ci=f*g
       ==> ci=(f*g)Um
       ==> Q1-INV

Q.E.D.


Prove: L(M) <==> L = (((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*

1. w in L(M) <==> w in L

   w in L(M) ==> M ends in Q8
   INVs always hold ==> w = (((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*
                    ==> w in L


   w in L ==> w in L(M)
   w in L ==> w = (((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*
   INVs always hold ==> M ends in Q8
                    ==> w in L(M)

2. w not in L(M) <==> w not in L

   w not in L(M) ==> M does not end in Q8
   INVs always hold ==> w not in L


   w not in L ==> w not in L(M)
   w not in L ==> w != (((f*g) U m) U ((f*g) U m)a)dt^+aa(m U n U q)(k U s U t)a*
   INVs always hold ==> M does not end in Q8
                    ==> w not in L(M)
   
Q.E.D.
|#

(sm-graph detect-motif)
(sm-visualize detect-motif
              (list 'Q0 Q0-INV)
              (list 'Q1 Q1-INV)
              (list 'Q2 Q2-INV)
              (list 'Q3 Q3-INV)
              (list 'Q4 Q4-INV)
              (list 'Q5 Q5-INV)
              (list 'Q6 Q6-INV)
              (list 'Q7 Q7-INV)
              (list 'Q8 Q8-INV)
              (list 'Q9 Q9-INV))

(test)