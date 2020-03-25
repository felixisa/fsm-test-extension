#lang racket
(require fsm)
(require test-engine/racket-tests)

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

; remove-EMP: (listof inputs) -> (listof inputs)
; Purpose: To remove e from test inputs
(define (remove-EMP loi)
  (map (λ (x) (if (member 'e x) (remove 'e x) x)) loi))

(check-expect (remove-EMP '(() (a e) (a b e))) '(() (a) (a b)))
(check-expect (remove-EMP '(() (a) (b) (a b e))) '(() (a) (b) (a b)))

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

(test) 