#lang racket
(require fsm)
(provide KLEENESTAR-abUaba
         AT-LEAST-ONE-MISSING
         A
         B
         A*
         AorB)

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