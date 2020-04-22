#lang racket
(require fsm test-engine/racket-tests)

#|

Q0 INV
nothing consumed
OR
consumed input contains ab OR contains aba

Q1 INV
if length is 1, equal to a
if greater than 1, 
starts with a AND ends with a AND at least one b

Q2 INV
first is a AND last is b

|#

(define (contains? patt w)
  (and (>= (length w) (length patt))
       (or (equal? (take w (length patt)) patt)
           (contains? patt (rest w)))))

(define Q0-INV
  (lambda (ci) (or (null? ci)
                   (contains? '(a b) ci)
                   (contains? '(a b a) ci))))

(define (Q1-INV ci)
  (if (= (length ci) 1)
      (equal? ci '(a))
      (and (equal? (take ci 1) '(a))
           (equal? (take-right ci 1) '(a))
           (ormap (lambda (i) (equal? i 'b)) ci))))

(define (Q2-INV ci)
  (and (equal? (take ci 1) '(a))
       (equal? (take-right ci 1) '(b))))

;accepts all inputs that only contain the pattern ab and/or aba
(define abUaba*
  (make-ndfa '(Q0 Q1 Q2)
             '(a b)
             'Q0
             '(Q0)
             `((Q0 a Q1)
               (Q1 b Q0)
               (Q1 b Q2)
               (Q2 a Q0))))

(define abUaba*2
  (make-ndfa  '(Q0 Q1 Q2)
              '(a b)
              'Q0
              '(Q0)
              `((Q0 a Q1)
                (Q1 b Q2)
                (Q2 a Q0)
                (Q2 ,EMP Q0))))

(sm-testequiv? abUaba* abUaba*2)

(check-expect (sm-apply abUaba* '()) 'accept)
(check-expect (sm-apply abUaba* '(a)) 'reject)
(check-expect (sm-apply abUaba* '(a b)) 'accept)
(check-expect (sm-apply abUaba* '(a b a)) 'accept)
(check-expect (sm-apply abUaba* '(a b b a)) 'reject)
(check-expect (sm-apply abUaba* '(a b a a a)) 'reject)
(check-expect (sm-apply abUaba* '(a b a a b)) 'accept)
(check-expect (sm-apply abUaba* '(a b a b a)) 'accept)
(check-expect (sm-apply abUaba* '(a b a a b a b a b a a b)) 'accept)


#|

            a              b
S = {Q0}   A = {Q1}   

A                       B = {Q0, Q2} 

B         C = {Q0, Q1}  S = {Q0}

C           A              B

|#


;;INVARIANTS

(define S-INV Q0-INV)

(define A-INV Q1-INV)

(define (B-INV ci)
  (or (Q0-INV ci)
      (Q2-INV ci)))

(define (C-INV ci)
  (or (Q0-INV ci)
      (Q1-INV ci)))

; new theory: DS implies ci is not in language (idk how to write that)
; a implies b -> (or (not a) b)
(define (DS-INV ci)
  (and (not (S-INV ci))
       (not (A-INV ci))
       (not (B-INV ci))
       (not (C-INV))))
  

(define DFA
  (make-dfa '(S A B C)
            '(a b)
            'S
            '(S B C)
            '((S a A)
              (A b B)
              (B a C)
              (B b S)
              (C a A)
              (C b B))))

;(sm-testequiv? abUaba* DFA)                  ;;why are they returning that
;(sm-testequiv? abUaba*2 DFA)


(check-expect (sm-apply DFA '()) 'accept)
(check-expect (sm-apply DFA '(a)) 'reject)
(check-expect (sm-apply DFA '(a b)) 'accept)
(check-expect (sm-apply DFA '(a b a)) 'accept)
(check-expect (sm-apply DFA '(a b b a)) 'reject)
(check-expect (sm-apply DFA '(a b a a a)) 'reject)
(check-expect (sm-apply DFA '(a b a a b)) 'accept)
(check-expect (sm-apply DFA '(a b a b a)) 'accept)
(check-expect (sm-apply DFA '(a b a a b a b a b a a b)) 'accept)


(test)