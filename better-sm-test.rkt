#lang racket
(require fsm test-engine/racket-tests "dfas.rkt" "ndfas.rkt" "pdas.rkt")
(provide test-inputs
         new-inputs
         remove-EMP) 

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

; remove-EMP: (listof inputs) -> (listof inputs)
; Purpose: To remove e from test inputs
(define (remove-EMP loi)
  (map (λ (x) (if (member 'ε x) (remove 'ε x) x)) loi))


(test)