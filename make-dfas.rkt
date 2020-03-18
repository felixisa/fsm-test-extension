#lang racket
(require fsm)
(require test-engine/racket-tests)



(define b*aab
  (make-dfa
   '(Q0 Q1 Q2 Q3 ds)
   '(a b)
   'Q0
   '(Q3)
   '((Q0 a Q1)
     (Q0 b Q0)
     (Q1 a Q2)
     (Q1 b ds)
     (Q2 a ds)
     (Q2 b Q3)
     (Q3 a Q1)
     (Q3 b Q3)
     (ds a ds)
     (ds b ds))))


(define abab*
  (make-dfa
   '(Q0 Q1 Q2 Q3 Q4)
   '(a b)
   'Q0
   '(Q4)
   '((Q0 a Q1)
     (Q0 b Q0)
     (Q1 a Q1)
     (Q1 b Q2)
     (Q2 a Q3)
     (Q2 b Q0)
     (Q3 a Q1)
     (Q3 b Q4)
     (Q4 a Q4)
     (Q4 b Q4))))


(define ab*
  (make-dfa
   '(Q0 Q1 Q2 ds)
   '(a b)
   'Q0
   '(Q1 Q2)
   '((Q0 a Q1)
     (Q0 b Q2)
     (Q1 a ds)
     (Q1 b Q2)
     (Q2 a Q1)
     (Q2 b ds)
     (ds a ds)
     (ds b ds))))


(define a*bb
  (make-dfa
   '(Q0 Q1 Q2 Q3 Q4)
   '(a b)
   'Q0
   '(Q1 Q3)
   '((Q0 a Q1)
     (Q0 b Q2)
     (Q1 a Q0)
     (Q1 b Q2)
     (Q2 a Q4)
     (Q2 b Q3)
     (Q3 a Q1)
     (Q3 b Q2)
     (Q4 a Q2)
     (Q4 b Q3))))


(define abUba
  (make-dfa
   '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8)
   '(a b)
   'Q0
   '(Q4 Q8)
   '((Q0 a Q1)
     (Q0 b Q5)
     (Q1 a Q1)
     (Q1 b Q2)
     (Q2 a Q2)
     (Q2 b Q3)
     (Q3 a Q4)
     (Q3 b Q3)
     (Q4 a Q4)
     (Q4 b Q4)
     (Q5 a Q6)
     (Q5 b Q5)
     (Q6 a Q7)
     (Q6 b Q6)
     (Q7 a Q7)
     (Q7 b Q8)
     (Q8 a Q8)
     (Q8 b Q8))))

