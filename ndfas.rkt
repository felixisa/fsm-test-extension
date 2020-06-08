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

(define web-or-ebay (make-ndfa
                     '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7)
                     '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                     'Q0
                     '(Q3 Q7)
                     `((Q0 a Q0)
                       (Q0 b Q0)
                       (Q0 c Q0)
                       (Q0 d Q0)
                       (Q0 e Q0)
                       (Q0 f Q0)
                       (Q0 g Q0)
                       (Q0 h Q0)
                       (Q0 i Q0)
                       (Q0 j Q0)
                       (Q0 k Q0)
                       (Q0 l Q0)
                       (Q0 m Q0)
                       (Q0 n Q0)
                       (Q0 o Q0)
                       (Q0 p Q0)
                       (Q0 q Q0)
                       (Q0 r Q0)
                       (Q0 s Q0)
                       (Q0 t Q0)
                       (Q0 u Q0)
                       (Q0 v Q0)
                       (Q0 w Q0)
                       (Q0 x Q0)
                       (Q0 y Q0)
                       (Q0 z Q0) 
                       (Q0 w Q1)
                       (Q1 e Q2)
                       (Q2 b Q3)
                       (Q0 e Q4)
                       (Q4 b Q5)
                       (Q5 a Q6)
                       (Q6 y Q7))
                     'no-dead))