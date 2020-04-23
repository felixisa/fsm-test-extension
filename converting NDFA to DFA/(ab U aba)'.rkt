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

;;; ci = (ab U aba)*
#;(define Q0-INV
    (lambda (ci) (or (null? ci)
                     (contains? '(a b) ci)
                     (contains? '(a b a) ci)))) ;; abbbbbbbba

; is it empty? yea? then true!
; are the first two ab? yea? then restart the algorithm!
; oh they're not? then dont check the rest. check the first three of the og ci!
; so ur at the point that the first two weren't ab? well are they aba? yeah? then restart the algorithm!
; oh they're not aba? darn. false.

; is it empty? yes? true!
; are first two ab? if yes then restart with dropped first two and add dropped to accum
; if not ab and accum is empty then false
; if not ab and accum is not empty then check last three of appended accum and ci and drop everything in accum
; if it is aba then restart with dropped first three and accum
; else false 

(define (Q0-INV ci)
  
  (define (helper ci accum)
    (cond [(empty? ci) #t]
          [(= (length ci) 1)
           (if (equal? (append (take-right accum 2) ci) '(a b a))
               (helper (drop ci 1) (append accum (take ci 1)))
               #f)]
          [(equal? (take ci 2) '(a b))
           (helper (drop ci 2) (append accum (take ci 2)))]
          [(equal? (take (append (take-right accum 2) ci) 3) '(a b a))
           (helper (drop ci 1) (append accum (take ci 1)))]
          [else #f]))
  
  (or (empty? ci)
      (and (> (length ci) 1)
           (helper ci '()))))



(check-expect (Q0-INV '(a b)) #t)
(check-expect (Q0-INV '(a b a)) #t)
(check-expect (Q0-INV '(a b a b a)) #t)
(check-expect (Q0-INV '(a b a a b a)) #t)
(check-expect (Q0-INV '(a b a a b)) #t)
(check-expect (Q0-INV '(a b b)) #f)
(check-expect (Q0-INV '(a b a a b b)) #f)


;;;; ci = (ab U aba)*a
(define (Q1-INV ci)
  (and (Q0-INV (drop-right ci 1))
       (equal? (take-right ci 1) '(a))))

(check-expect (Q1-INV '(a b)) #f)
(check-expect (Q1-INV '(a b a)) #t)
(check-expect (Q1-INV '(a b a b a)) #t)
(check-expect (Q1-INV '(a b a a b a)) #t)
(check-expect (Q1-INV '(a b b)) #f)
(check-expect (Q1-INV '(a b a a b b)) #f)

;;; ci = (ab U aba)*ab
(define (Q2-INV ci)
  (and (Q0-INV (drop-right ci 2))
       (equal? (take-right ci 2) '(a b))))


(check-expect (Q2-INV '(a b)) #f)
(check-expect (Q2-INV '(a b a b)) #t)
(check-expect (Q2-INV '(a b a b a b)) #t)
(check-expect (Q2-INV '(a b a a b a b)) #t)
(check-expect (Q2-INV '(a b b)) #f)
(check-expect (Q2-INV '(a b a a b b)) #f)



;accepts all inputs that only contain the pattern ab and/or aba
(define abUaba*
  (make-ndfa '(Q0 Q1 Q2)
             '(a b)
             'Q0
             '(Q0)
             `((Q0 a Q1)
               (Q1 b Q0)
               (Q1 b Q2)
               (Q2 a Q0))
             'no-dead))

(define abUaba*2
  (make-ndfa  '(Q0 Q1 Q2)
              '(a b)
              'Q0
              '(Q0)
              `((Q0 a Q1)
                (Q1 b Q2)
                (Q2 a Q0)
                (Q2 ,EMP Q0))
              'no-dead))

;(sm-testequiv? abUaba* abUaba*2)

(check-expect (sm-apply abUaba* '()) 'accept)
(check-expect (sm-apply abUaba* '(a)) 'reject)
(check-expect (sm-apply abUaba* '(a b)) 'accept)
(check-expect (sm-apply abUaba* '(a b a)) 'accept)
(check-expect (sm-apply abUaba* '(a b b a)) 'reject)
(check-expect (sm-apply abUaba* '(a b a a a)) 'reject)
(check-expect (sm-apply abUaba* '(a b a a b)) 'accept)
(check-expect (sm-apply abUaba* '(a b a b a)) 'accept)
(check-expect (sm-apply abUaba* '(a b a a b a b a b a a b)) 'accept)

(sm-visualize abUaba* (list 'Q0 Q0-INV) (list 'Q1 Q1-INV) (list 'Q2 Q2-INV))

#|

            a              b
S = {Q0}   A = {Q1}   

A                       B = {Q0, Q2} 

B         C = {Q0, Q1}  

C           A              B

|#


;;INVARIANTS

;(define S-INV Q0-INV)

;(define A-INV Q1-INV)

#;(define (B-INV ci)
    (or (Q0-INV ci)
        (Q2-INV ci)))

#;(define (C-INV ci)
    (or (Q0-INV ci)
        (Q1-INV ci)))

; new theory: DS implies ci is not in language (idk how to write that)
; a implies b -> (or (not a) b)
#;(define (DS-INV ci)
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
              (C a A)
              (C b B))))

;(sm-testequiv? abUaba* DFA)                  
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