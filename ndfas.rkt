#lang racket
(require fsm)
(provide KLEENESTAR-abUaba)
(provide AT-LEAST-ONE-MISSING)
(provide A)
(provide B)

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
               (B a B)
               (B c B)
               (C b C)
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
