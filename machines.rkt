#lang racket
(require fsm)
(require test-engine/racket-tests)


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

(define (A-INV ci)
  (or (null? ci)
      (even? (length (filter (lambda (i) (equal? i 'a)) ci)))))

(define (B-INV ci)
  (and (andmap (lambda (i) (equal? i 'a)) ci)
       (odd? (length (filter (lambda (i) (equal? i 'a)) ci))))) 

(define (C-INV ci)
  (and (>= (length ci))
       (or (equal? (take-right ci 1) 'a)
           (equal? (take-right ci 1) 'b))))

(define (D-INV ci)
  (or (and (= (length ci) 1)
           (equal? (car ci) 'b))
      (and (equal? (length (filter (lambda (i) (equal? i 'b)) ci)) 1)
           (equal? (take-right ci 1) 'a))))

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

(define DEAD
  (make-dfa
   '(A B C D ds)
   '(a b)
   'A
   '(C)
   '((A a B)
     (A b D)
     (B a C)
     (B b ds)
     (D a ds)
     (D b C)
     (C a C)
     (C b C)
     (ds a ds)
     (ds b ds))))

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


;; valid input: aaabbb 
(define P (make-ndpda '(S F)
                      '(a b)
                      '(c)
                      'S
                      '(F)
                      `(((S ,EMP ,EMP) (F ,EMP))
                        ((F a ,EMP) (F (c)))
                        ((F b (c)) (F ,EMP)))))

; valid input: aabcbaa
(define pda-wcw^r (make-ndpda '(S M N F)                  ;the states
                              '(a b c)                    ;the alphabet
                              '(a b)                      ;the stack alphabet
                              'S                          ;the starting state
                              '(F)                        ;the final state
                              `(((S ,EMP ,EMP) (M ,EMP))  ;the transition relation
                                ((M a ,EMP) (M (a)))
                                ((M b ,EMP) (M (b)))
                                ((M c ,EMP) (N ,EMP))
                                ((N a (a)) (N ,EMP))
                                ((N b (b)) (N ,EMP))
                                ((N ,EMP ,EMP) (F ,EMP)))))

(define pda-numa=numb (make-ndpda '(S M F)
                                  '(a b)
                                  '(a b)
                                  'S
                                  '(F)
                                  `(((S ,EMP ,EMP) (M ,EMP))
                                    ((M ,EMP ,EMP) (F ,EMP))
                                    ((M a ,EMP) (M (a)))
                                    ((M b ,EMP) (M (b)))
                                    ((M a (b)) (M ,EMP))
                                    ((M b (a)) (M ,EMP)))))

(define (S-INV ci s) (and (empty? ci) (empty? s)))

(define (M-INV ci s)
  (and (or (andmap (λ (k) (eq? k 'a)) s)
           (andmap (λ (k) (eq? k 'b)) s))
       (implies (empty? s)
                (= (length (filter (λ (k) (eq? k ‘a)) ci))
                   (length (filter (λ (k) (eq? k ‘b)) ci))))
       (implies (not (empty? s))
                (and (implies (eq? (first s) ‘a)
                              (= (- (length (filter (λ (k) (eq? k ‘a)) ci)) (length s))
                                 (length (filter (λ (k) (eq? k 'b)) ci))))
                     (implies (eq? (first s) ‘b)
                              (= (- (length (filter (λ (k) (eq? k 'b)) ci)) (length s))
                                 (length (filter (λ (k) (eq? k 'a)) ci))))))))

(define (F-INV ci s)
  (= (length (filter (λ (k) (eq? k 'a)) ci))
     (length (filter (λ (k) (eq? k 'b)) ci))))
