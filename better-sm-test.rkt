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



(define (test-input m)

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
      (cond [(member (caar u-words) e-states) (cdr u-words)]
            [else (map (lambda (rule) (list (caddr rule)
                                            (append (cadar u-words) (list (cadr rule)))))
                       (find-all-trans (caar u-words)))]))
    
    ; new-e-states: (listof state) -> (listof state)
    ; Purpose: To update e-states accumulator
    (define (new-e-states st)

      ; are-these-in-there?: list list -> boolean
      ; Purpose: Determines if all the contents of L1 are in L2 
      (define (are-these-in-there? L1 L2)
        (andmap (lambda (i) (not (false? (member i L2)))) L1))

      (if (are-these-in-there? (find-all-trans st) e-trans)
          (cons st e-states)
          e-states))
    
    (cond [(null? u-words) t-words]
          [(member (caar u-words) e-states) (helper (cons (car u-words) t-words) e-trans (cdr u-words) e-states)]
          [else (helper (cons (car u-words) t-words)
                        (append (find-all-trans (caar u-words)) e-trans)
                        (append (cdr u-words) (new-u-words u-words))
                        (new-e-states (caar u-words)))]))

  (reverse (helper '() (find-all-trans (sm-getstart m)) (list (list (sm-getstart m) '())) '())))


(test)