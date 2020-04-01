#lang racket
(require fsm test-engine/racket-tests)
(provide TEST-MACHINE
         a-bc*-d
         STATES
         ALPHA
         bad-or-dad
         b*aab
         abab*
         ab*
         a*bb
         abUba
         EVEN-NUM-B
         NO-ABAA)

(define TEST-MACHINE
  (make-dfa
   '(A B C D)
   '(a b)
   'A
   '(C)
   '((A a B)
     (A b D)
     (B a A)
     (B b C)
     (D a D)
     (D b C)
     (C a C)
     (C b C))))

(define a-bc*-d
  (make-dfa '(Q0 Q1 Q2)
            '(a b c d)
            'Q0
            '(Q2)
            '((Q0 a Q1)
              (Q1 b Q1)
              (Q1 c Q1)
              (Q1 d Q2))))

(define STATES
  (make-dfa
   '(A B C D E)
   '(a b)
   'A
   '(C)
   '((A a B)
     (A b D)
     (B a C)
     (B b B)
     (C a C)
     (C b C)
     (D a D)
     (D b E)
     (E a E)
     (E b C))))

(define ALPHA
  (make-dfa
   '(A B C)
   '(a b c)
   'A
   '(C)
   '((A a A)
     (A b A)
     (A c B)
     (B a B)
     (B b B)
     (B c C)
     (C a C)
     (C b C)
     (C c C))))

(define bad-or-dad
  (make-dfa '(Q0 Q1 Q2 Q3 Q4 Q5 Q6)
            '(a b d)
            'Q0
            '(Q3 Q6)
            '((Q0 a Q0)
              (Q0 b Q4)
              (Q0 d Q1)
              (Q1 a Q2)
              (Q1 b Q4)
              (Q1 d Q1)
              (Q2 a Q0)
              (Q2 b Q4)
              (Q2 d Q3)
              (Q3 a Q3)
              (Q3 b Q3)
              (Q3 d Q3)
              (Q4 a Q5)
              (Q4 b Q4)
              (Q4 d Q1)
              (Q5 a Q0)
              (Q5 b Q4)
              (Q5 d Q6)
              (Q6 a Q6)
              (Q6 b Q6)
              (Q6 d Q6))))

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

(define EVEN-NUM-B
  (make-dfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0)
            '((Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0))))

(define NO-ABAA
  (make-dfa 
   '(Q-0 Q-1 Q-2 Q-3 Q-4)
   '(a b) 
   'Q-0
   '(Q-0 Q-1 Q-2 Q-3)
   '((Q-0 a Q-1)
     (Q-0 b Q-0)
     (Q-1 a Q-1)
     (Q-1 b Q-2)
     (Q-2 a Q-3 )
     (Q-2 b Q-0)
     (Q-3 a Q-4)
     (Q-3 b Q-2)
     (Q-4 a Q-4)
     (Q-4 b Q-4))))