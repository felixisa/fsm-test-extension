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
          [(member (caar u-words) e-states) (helper (cons (cadar u-words) t-words) e-trans (cdr u-words) e-states)]
          [else (helper (cons (cadar u-words) t-words)
                        (append (find-all-trans (caar u-words)) e-trans)
                        (append (cdr u-words) (new-u-words u-words))
                        (new-e-states (caar u-words)))]))

  (reverse (helper '() (find-all-trans (sm-getstart m)) (list (list (sm-getstart m) '())) '())))




; remove-bad: (listof word) -> (listof word)
; Purpose: To remove words that test transitions more than necessary
(define (remove-bad loi m)

  ; get-path: word -> (listof state)
  ; Purpose: To extract the states in sm-showtransitions
  (define (get-path i)
    (map (λ (x) (cadr x)) (drop-right (sm-showtransitions m i) 1)))
  (cond [(null? loi) '()]
        [(null? (car loi)) (cons '() (remove-bad (cdr loi) m))]
        [(not (ormap (lambda (i) (equal? (get-path (car loi)) (take i (length (get-path (car loi)))))) (map (λ (x) (get-path x)) (cdr loi))))
         (cons (car loi) (remove-bad (cdr loi) m))]
        [else (remove-bad (cdr loi) m)]))

(check-expect (remove-bad '(() (c) (b b) (b c) (a c) (a a) (a b)) AT-LEAST-ONE-MISSING) '(() (b c) (a c) (a b)))



; ndfa-test: machine -> (listof word)
; Purpose: generate ndfa test strings 
(define (ndfa-tests m)

  ; remove-bad: (listof word) -> (listof word)
  ; Purpose: To remove words that test transitions more than necessary
  (define (remove-bad loi)

    ; get-path: word -> (listof state)
    ; Purpose: To extract onle the states in sm-showtransitions
    (define (get-path i)
      (map (λ (x) (cadr x)) (drop-right (sm-showtransitions m i) 1)))
    
    (cond [(null? loi) '()]
          [(null? (car loi)) (cons '() (remove-bad (cdr loi)))]
          [(symbol? (sm-showtransitions m (car loi))) (cons (car loi) (remove-bad (cdr loi)))]
          [(not (ormap (lambda (i) (equal? (get-path (car loi)) (take i (length (get-path (car loi)))))) (map (λ (x) (get-path x)) (cdr loi))))
           (cons (car loi) (remove-bad (cdr loi)))]
          [else (remove-bad (cdr loi))]))

  ; remove-EMP: (listof inputs) -> (listof inputs)
  ; Purpose: To remove e from test inputs
  (define (remove-EMP loi)
    (map (λ (x) (filter (λ (i) (not (eq? i 'ε))) x)) loi))

  ; new-inputs: slist -> slist
  ; Purpose: To remove input words that are substrings of other input words in the list
  ;          i.e. remove '(a) because it is a substring of '(a a) and '(a b a)
  (define (new-inputs loi)
    (cond [(null? loi) '()]
          [(null? (car loi)) (cons '() (new-inputs (cdr loi)))]
          [(not (ormap (lambda (i) (equal? (car loi) (take i (length (car loi))))) (cdr loi)))
           (cons (car loi) (new-inputs (cdr loi)))]
          [else (new-inputs (cdr loi))]))
  
  (remove-EMP
   (remove-bad
    (new-inputs (test-inputs m)))
  )
  )

(define detect-motif
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9)
             '(a d g f k m n q s t)
             'Q0
             '(Q8)
             `((Q0 ,EMP Q9)
               (Q0 m Q1)
               (Q9 f Q9)
               (Q1 a Q2)
               (Q1 ,EMP Q2)
               (Q2 d Q3)
               (Q3 t Q4)
               (Q3 t Q3)
               (Q4 a Q5)
               (Q5 a Q6)
               (Q6 m Q7)
               (Q6 n Q7)
               (Q6 q Q7)
               (Q7 k Q8)
               (Q7 s Q8)
               (Q7 t Q8)
               (Q8 a Q8)
               (Q9 g Q1))
             'no-dead))

 ; remove-EMP: (listof inputs) -> (listof inputs)
  ; Purpose: To remove e from test inputs
  (define (remove-EMP loi)
    (map (λ (x) (if (member 'ε x) (remove 'ε x) x)) loi))


(test)