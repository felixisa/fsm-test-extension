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
          
; sm-test2: machine -> slist
; mock version of sm-test 
(define (sm-test2 m)
  (define (helper i m)
    (list i (sm-apply m i)))
  (map (lambda (x) (helper x m)) (remove-EMP (new-inputs (test-inputs m))))) 

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

; remove-EMP: (listof inputs) -> (listof inputs)
; Purpose: To remove e from test inputs
(define (remove-EMP loi)
  (map (λ (x) (if (member 'e x) (remove 'e x) x)) loi))

(check-expect (remove-EMP '(() (a e) (a b e))) '(() (a) (a b)))
(check-expect (remove-EMP '(() (a) (b) (a b e))) '(() (a) (b) (a b)))

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

; generate-ndfa-tests: ndfa --> (listof word)
; Purpose: To generate the words needed to test every state of the given dfa
(define (generate-ndfa-tests ndfa)
  (remove-EMP (new-inputs (test-inputs ndfa))))

(check-expect (generate-ndfa-tests KLEENESTAR-abUaba)
              '(() (a b) (a b a)))
(check-expect (generate-ndfa-tests AT-LEAST-ONE-MISSING)
              '(() (c) (a) (b)))
(check-expect (generate-ndfa-tests A)
              '(() (a)))
(check-expect (generate-ndfa-tests B)
              '(() (b)))

;; get-pda-rule: processed-list -> pda-rule
;; Purpose: Determins if the rule to be made should be empty or a real rule
(define (get-pda-rule processed-list)
  (cond
    [(< (length processed-list) 2)  '((empty empty empty) (empty empty))]
    [else (construct-pda-rule processed-list)]))

;; construct-pda-rule: processed-list -> pda-rule
;; Purpose: Constructes a pda rule from the given processed list
(define (construct-pda-rule pl)
  (letrec (
           (next-state (caar pl)) ;; The initial state that the machine is in
           (init-state (caadr pl)) ;; The state that the machien ends in
           (next-input (cadar pl)) ;; The initial state's input
           (init-input (cadadr pl)) ;; The state that the machien ends in input
           (next-stack (caddar pl)) ;; The elemetns that are on the init stack
           (sec (cadr pl))  ;; The second list in the stack
           (init-stack (caddr sec)) ;; The elements that are on the next stack

           ;; take*: Integer List -> List or symbol
           ;; Purpose: functions the same as Racket's take function except if the list
           ;;   result of take is the empty list then 'e is returned instead
           (take* (lambda (num a-list)
                    (let ((t (take a-list num)))
                      (if (empty? t) 'e t))))

           ;; determine-consumed: none -> symbol
           ;; Purpose: determins what the input is that is consumed
           (determin-consumed (lambda ()
                                (cond
                                  ;; If both inputs are equal then nothing was consumed
                                  [(equal? init-input next-input)'e]
                                  [else (car init-input)])))

           ;; determin-pushed: none -> integer
           ;; Purpose: Returns the number of elements that have been pushed on the stack
           (determin-pushed (lambda ()
                              (let ((num (- (length next-stack) (length init-stack))))
                                (if (< num 0) 0 num))))

           ;; determin-poped: none -> integer
           ;; Purpose: Returns the number of elements that have been poped off the stack
           (determin-poped (lambda ()
                             (let ((num (- (length init-stack) (length next-stack))))
                               (if (< num 0) 0 num)))))

    (cond
      ;; If there is less then 2 elements then we are at the ed so return the default
      [(< (length pl) 2) '((empty empty empty) (empty empty))]
      [else
       (list
        (list init-state (determin-consumed) (take* (determin-poped) init-stack))
        (list next-state (take* (determin-pushed) next-stack)))])))

(test)