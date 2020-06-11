#lang racket
(require fsm test-engine/racket-tests "dfas.rkt" "ndfas.rkt" "pdas.rkt") 

; test-inputs: machine -> slist
; Purpose: To generate the least amount of input words that test every node of a machine  
(define (test-inputs m)

  ; find-all-trans: state -> (listof rules) 
  ; Purpose: To find all transitions from the given current state
  (define (find-all-trans st)
    (filter (lambda (x) (equal? st (car x))) (sm-getrules m)))
    
  ; helper: test-words expanded-transitions unfinished-words expanded-states
  ; Purpose: To generate the least amount of input words that test every edge of a machine 
  (define (helper t-words e-trans u-words e-states)

    ; new-u-words: slist -> slist
    ; Purpose: To update u-words accumulator
    (define (new-u-words u-words)
      (cond [(not (false? (member (last (caar u-words)) e-states))) (cdr u-words)]
            [else (map (lambda (rule) (list (append (caar u-words) (list (caddr rule)))
                                            (append (cadar u-words) (list (cadr rule)))))
                       (filter (λ (i) (false? (member i e-trans))) (find-all-trans (last (caar u-words)))))]))

    ; are-these-in-there?: list list -> boolean
    ; Purpose: Determines if all the contents of L1 are in L2 
    (define (are-these-in-there? L1 L2)
      (and (>= (length L2) (length L1))
           (andmap (lambda (i) (not (false? (member i L2)))) L1)))
    
    ; new-e-states: (listof state) -> (listof state)
    ; Purpose: To update e-states accumulator
    (define (new-e-states st)
      (if (are-these-in-there? (find-all-trans st) e-trans)
          (cons st e-states)
          e-states))
    
    (cond [(null? u-words) t-words]
          [(member (last (caar u-words)) e-states) (helper (cons (car u-words) t-words) e-trans (cdr u-words) e-states)]
          [else (helper (cons (car u-words) t-words)
                        (append (find-all-trans (last (caar u-words))) e-trans)
                        (append (cdr u-words) (new-u-words u-words))
                        (new-e-states (last (caar u-words))))]))

  (reverse (helper '() '() (list (list (list (sm-getstart m)) '())) '())))

; new-inputs: (listof path word) -> (listof path word)
; Purpose: To remove redundant test words
(define (new-inputs loi)
  (cond [(null? loi) '()]
        [(null? (cadar loi)) (cons (car loi) (new-inputs (cdr loi)))] ; keeps the empty string
        [(not (ormap (lambda (i) (and (equal? (caar loi) (take (car i) (length (caar loi))))
                                      (equal? (cadar loi) (take (cadr i) (length (cadar loi)))))) (cdr loi)))
         (cons (car loi) (new-inputs (cdr loi)))]
        [else (new-inputs (cdr loi))]))

(check-expect (new-inputs '(((A) (b)) ((A B) (a b)) ((A C) (a c)) ((A B C) (a b c)))) '(((A) (b)) ((A C) (a c)) ((A B C) (a b c))))
(check-expect (new-inputs '(((A) ()) ((A B) (ε)) ((A C) (ε)) ((A D) (ε)) ((A B B) (ε b)) ((A B B) (ε c)) ((A C C) (ε a)) ((A C C) (ε c)) ((A D D) (ε a)) ((A D D) (ε b)))) 
              '(((A) ()) ((A B B) (ε b)) ((A B B) (ε c)) ((A C C) (ε a)) ((A C C) (ε c)) ((A D D) (ε a)) ((A D D) (ε b))))

; testing123: ndfa -> (listof path word)
; Purpose: To generate test words and their paths
(define (testing123 m)
  (new-inputs (test-inputs m)))

#|
(check-expect (testing123 ONE-MISSING)
              '(((S) ())
                ((S A A A) (a a a))
                ((S A A D) (a a b))
                ((S A A E) (a a c))
                ((S A D D) (a b a))
                ((S A D D) (a b b))
                ((S A E E) (a c a))
                ((S A E E) (a c c))
                ((S B D D) (b a a))
                ((S B D D) (b a b))
                ((S B D ds) (b a c))
                ((S B B D) (b b a))
                ((S B B B) (b b b))
                ((S B B F) (b b c))
                ((S B F F) (b c b))
                ((S B F F) (b c c))
                ((S B F ds) (b c a))
                ((S C E E) (c a a))
                ((S C E E) (c a c))
                ((S C E ds) (c a b))
                ((S C F F) (c b b))
                ((S C F F) (c b c))
                ((S C F ds) (c b a))
                ((S C C E) (c c a))
                ((S C C F) (c c b))
                ((S C C C) (c c c))
                ((S A D ds ds) (a b c a))
                ((S A D ds ds) (a b c b))
                ((S A D ds ds) (a b c c))
                ((S A E ds ds) (a c b a))
                ((S A E ds ds) (a c b b))
                ((S A E ds ds) (a c b c))))

(check-expect (testing123 TEST-MACHINE)
              '(((A) ())
                ((A B A) (a a))
                ((A B C C) (a b a))
                ((A B C C) (a b b))
                ((A D D D) (b a a))   
                ((A D D C) (b a b))
                ((A D C C) (b b a))
                ((A D C C) (b b b))))

(check-expect (testing123 a-bc*-d)
              '(((Q0) ())
                ((Q0 ds) (d))
                ((Q0 Q1 Q1) (a c))
                ((Q0 Q1 ds) (a a))
                ((Q0 ds ds) (b a))
                ((Q0 ds ds) (b b))
                ((Q0 ds ds) (b c))
                ((Q0 ds ds) (b d))
                ((Q0 ds ds) (c a))
                ((Q0 ds ds) (c b))
                ((Q0 ds ds) (c c))
                ((Q0 ds ds) (c d))
                ((Q0 Q1 Q1 Q1) (a b b))
                ((Q0 Q1 Q1 Q1) (a b c))
                ((Q0 Q1 Q1 ds) (a b a))
                ((Q0 Q1 Q2 ds) (a d a))
                ((Q0 Q1 Q2 ds) (a d b))
                ((Q0 Q1 Q2 ds) (a d c))
                ((Q0 Q1 Q2 ds) (a d d))
                ((Q0 Q1 Q1 Q2 ds) (a b d a))
                ((Q0 Q1 Q1 Q2 ds) (a b d b))
                ((Q0 Q1 Q1 Q2 ds) (a b d c))
                ((Q0 Q1 Q1 Q2 ds) (a b d d))))

(check-expect (testing123 STATES)
              '(((A) ())
                ((A B C C) (a a b))
                ((A B B C) (a b a))
                ((A B B B) (a b b)) 
                ((A D D D) (b a a))
                ((A D E E) (b b a))
                ((A D E C) (b b b))
                ((A B C C C) (a a a a))
                ((A B C C C) (a a a b))
                ((A D D E E) (b a b a)) 
                ((A D D E C) (b a b b))))

(check-expect (testing123 KLEENESTAR-abUaba)
              '(((Q0) ()) ((Q0 Q4 Q5 Q0) (a b ε)) ((Q0 Q1 Q2 Q3 Q0) (a b a ε))))

(check-expect (testing123 AT-LEAST-ONE-MISSING)
              '(((A) ())
                ((A C C) (ε c))
                ((A B B B) (ε b b))
                ((A B B B) (ε b c))
                ((A C C C) (ε a c))
                ((A D D D) (ε a a))
                ((A D D D) (ε a b))))
|#
(test)