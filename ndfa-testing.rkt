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
                       (find-all-trans (last (caar u-words))))]))

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

  (reverse (helper '() (find-all-trans (sm-getstart m)) (list (list (list (sm-getstart m)) '())) '())))

; new-inputs: (listof path word) -> (listof path word)
; Purpose: To remove redundant test words
(define (new-inputs loi)
  (cond [(null? loi) '()]
        [(null? (cadar loi)) (cons (car loi) (new-inputs (cdr loi)))] ; keeps the empty string
        [(or (not (ormap (lambda (i) (equal? (caar loi) (take (car i) (length (caar loi))))) (cdr loi))) ; the path of the first is not part of any other
             (not (ormap (lambda (i) (equal? (cadar loi) (take (cadr i) (length (cadar loi))))) (cdr loi)))) ; the word of the first is not a substring of any other
         (cons (car loi) (new-inputs (cdr loi)))]
        [else (new-inputs (cdr loi))]))

(check-expect (new-inputs '(((A) (b)) ((A B) (a b)) ((A C) (a c)) ((A B C) (a b c)))) '(((A) (b)) ((A C) (a c)) ((A B C) (a b c))))

; testing123: ndfa -> (listof path word)
; Purpose: To generate test words and their paths
(define (testing123 m)
  (new-inputs (test-inputs m)))

(test)