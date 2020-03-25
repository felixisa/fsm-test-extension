#lang racket
(require fsm)
(require test-engine/racket-tests)

; generate-dfa-tests: dfa --> (listof word)
; Purpose: To generate the words needed to test every state of the given dfa
(define (generate-dfa-tests dfa)
  (new-inputs (test-inputs dfa)))

(check-expect (generate-dfa-tests TEST-MACHINE)
              '(() (a a) (b a) (b b) (a b a) (a b b)))
(check-expect (generate-dfa-tests a-bc*-d)
              '(() (c) (d) (a b) (a c) (a a) (b a) (b b) (b c) (b d) (a d a) (a d b) (a d c) (a d d)))
(check-expect (generate-dfa-tests STATES)
              '(() (a b) (b a) (a a a) (a a b) (b b a) (b b b)))
(check-expect (generate-dfa-tests ALPHA)
              '(() (a) (b) (c a) (c b) (c c a) (c c b) (c c c)))
(check-expect (generate-dfa-tests DEAD)
              '(() (b a) (b b) (a a a) (a a b) (a b a) (a b b)))

; test-inputs: machine -> slist
; Purpose: To generate the least amount of input words that test every node of a machine  
(define (test-inputs m)
  ; helper: machine test-words unfinished-words expanded-states
  ; Purpose: To generate the least amount of input words that test every node of a machine 
  ; ACCUM-INVS
  ; t-words: tested words thus far
  ; u-words: unfinished words thus far
  ; e-states: expanded states thus far 
  (define (helper t-words u-words e-states)

    ; new-u-words: slist -> slist
    ; Purpose: To update u-words accumulator
    (define (new-u-words u-words)
    
      ; state (listof rules) -> (listof rules) 
      ; finds all transitions with the given current state
      (define (find-all-trans st)
        (filter (lambda (x) (equal? st (car x))) (sm-getrules m)))
    
      (cond [(member (caar u-words) e-states) (cdr u-words)]
            [else (map (lambda (rule) (list (caddr rule)
                                            (append (cadar u-words) (list (cadr rule)))))
                       (find-all-trans (caar u-words)))]))
    
    (cond [(null? u-words) t-words]
          [(member (caar u-words) e-states) (helper (cons (cadar u-words) t-words) (cdr u-words) e-states)]
          [else (helper (cons (cadar u-words) t-words)
                        (append (cdr u-words) (new-u-words u-words))
                        (cons (caar u-words) e-states))]))

  (reverse (helper '() (list (list (sm-getstart m) '())) '())))

; new-inputs: slist -> slist
; Purpose: To remove input words that are substrings of other input words in the list
;          i.e. remove '(a) because it is a substring of '(a a) and '(a b a)
(define (new-inputs loi)
  (cond [(null? loi) '()]
        [(null? (car loi)) (cons '() (new-inputs (cdr loi)))]
        [(not (ormap (lambda (i) (equal? (car loi) (take i (length (car loi))))) (cdr loi)))
         (cons (car loi) (new-inputs (cdr loi)))]
        [else (new-inputs (cdr loi))]))

(check-expect (new-inputs '(() (a) (b) (a a) (a b) (b a) (b b) (a b a) (a b b)))
              '(() (a a) (b a) (b b) (a b a) (a b b)))
(check-expect (new-inputs (test-inputs a-bc*-d))
              '(() (c) (d) (a b) (a c) (a a) (b a) (b b) (b c) (b d) (a d a) (a d b) (a d c) (a d d)))



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

(test)