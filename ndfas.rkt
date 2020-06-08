#lang racket
(require fsm)
(provide KLEENESTAR-abUaba
         AT-LEAST-ONE-MISSING
         A
         B
         A*
         AorB
         detect-motif)

(define KLEENESTAR-abUaba
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4 Q5)
             '(a b)
             'Q0
             '(Q0)
             `((Q0 a Q1)
               (Q1 b Q2)
               (Q2 a Q3)
               (Q3 ,EMP Q0)
               (Q0 a Q4)
               (Q4 b Q5)
               (Q5 ,EMP Q0))))



(define AT-LEAST-ONE-MISSING
  (make-ndfa '(A B C D)
             '(a b c)
             'A
             '(B C D)
             `((A ,EMP B)
               (A ,EMP C)
               (A ,EMP D)
               (B b B)
               (B c B)
               (C a C)
               (C c C)
               (D a D)
               (D b D))))

(define A
  (make-ndfa '(Q0 Q1)
             '(a b)
             'Q0
             '(Q1)
             '((Q0 a Q1))))


(define B
  (make-ndfa '(Q0 Q1)
             '(a b)
             'Q0
             '(Q1)
             '((Q0 b Q1))))

(define A*
  (make-ndfa '(Q0 Q1 Q2 Q3)
             '(a)
             'Q0
             '(Q1 Q3)
             `((Q0 a Q1)
               (Q1 ,EMP Q0)
               (Q0 a Q2)
               (Q2 a Q3)
               (Q3 ,EMP Q0))))

(define AorB
  (make-ndfa '(Q0 Q1 Q2)
             '(a b)
             'Q0
             '(Q1 Q2)
             `((Q0 a Q1)
               (Q1 ,EMP Q0)
               (Q0 b Q2)
               (Q2 ,EMP Q0))))

(define detect-motif
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9)
             '(a d g f k m n q s t)
             'Q0
             '(Q8)
             `((Q0 ,EMP Q1)
               (Q0 m Q2)
               (Q1 g Q2)
               (Q1 f Q1)
               (Q2 a Q3)
               (Q2 ,EMP Q3)
               (Q3 d Q4)
               (Q4 t Q5)
               (Q4 t Q4)
               (Q5 a Q6)
               (Q6 a Q7)
               (Q7 m Q8)
               (Q7 n Q8)
               (Q7 q Q8)
               (Q8 k Q9)
               (Q8 s Q9)
               (Q8 t Q9)
               (Q9 a Q9)
               )
             'no-dead))