#lang racket
(require fsm test-engine/racket-tests)

; test-inputs: machine -> slist
; Purpose: To generate the least amount of input words that test every node of a machine  
(define (test-inputs m)

  ; find-all-trans: state -> (listof rules) 
  ; Purpose: To find all transitions from the given current state
  (define (find-all-trans st)
    (filter (lambda (x) (equal? st (car x))) (sm-getrules m)))
    
  ; helper: (list (listof state) word) (listof rule) (list (listof state) word) (listof state) -> (list (listof state) word)
  ; Purpose: To generate the least amount of input words that test every edge of a machine
  ; ACCUM-INVS
  ; t-words: the accumulated test words thus far; on each step, the first u-word is added to t-words
  ; u-words: accumulated "unfinished words"; a word is unfinished if the last state in its path has not been fully expanded,
  ;          leaving more transitions to add onto the word
  ; e-states: accumulated expanded states; a state is added to this accumulator if all its transitions have been expanded
  ; e-trans: accumulated expanded transitions; these transitions are added to the accumulator each time they are used to add onto an unfinished word.
  ;          this accumulator is also used to ensure that transitions are not used more than necessary.
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

; testing123: ndfa -> (listof path word)
; Purpose: To generate test words and their paths
(define (testing123 m)
  (new-inputs (test-inputs m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; invariant-tester: machine, optional arg: predicates -> nothin technically
; Purpose: Tests invariants
(define (invariant-tester m . invs)
  
  ; helper: machine (listof invariants) -> list of stuff
  ; Purpose: Tests invariants 
  (define (helper m invs)

    ; list of test words produced by testing123
    (define lotw (testing123 m))

    ; test-inv: (list path word) -> boolean OR state OR (list state word)
    ; Purpose: Tests invariants by traversing path and word at the same time
    (define (test-inv t-word)
    
      ;find-inv: state -> predicate
      ; Purpose: To find the invariant associated with the given state
      (define (find-inv state)
        (filter (λ (i) (eq? (first i) state)) invs))

      ;test-inv-help: path word number -> boolean OR state OR (list state word)
      ; Purpose: Tests invariants by traversing path and word at the same time
      ; Index is a number used to reference the given word at each step. It increases by one on each recursive call. 
      (define (test-inv-help path word index)
        (cond [(empty? path) '()]
              [(empty? (find-inv (first path))) (cons (first path) (test-inv-help (rest path) word (add1 index)))]
              [(and (>= (length (take word index)) 1) (eq? (last (take word index)) EMP))
               (cond [((last (first (find-inv (first path)))) (filter (λ (i) (not (eq? i EMP))) (take word index))) (cons #t (test-inv-help (rest path) word (add1 index)))]
                     [else (cons (list (first path) (take word index)) (test-inv-help (rest path) word (add1 index)))])]
              [((last (first (find-inv (first path)))) (filter (λ (i) (not (eq? i EMP))) (take word index))) (cons #t (test-inv-help (rest path) word (add1 index)))]
              [else (cons (list (first path) (take word index)) (test-inv-help (rest path) word (add1 index)))]))

      (test-inv-help (car t-word) (cadr t-word) 0))

    (remove-duplicates (append-map test-inv lotw)))

  ; displays failed words
  (define (display-failed L)
      (foldl (lambda (i f) (displayln (format "~s INV failed for ~s" (car i) (cadr i)))) (void) L))

  ; displays untested invs
  (define (display-untested L)
      (foldl (lambda (i f) (displayln (format "~s INV not provided" i))) (void) L))

  (define results (helper m invs))

  (cond [(and (andmap boolean? results)          ; all passed
              (empty? (filter symbol? results))) ; all tested
         (displayln "All invariants are holding strong")]
        [(and (empty? (filter symbol? results))       ; all tested
              (not (empty? (filter list? results))))  ; not all passed
         (display-failed (filter list? results))]
        [(and (not (empty? (filter symbol? results))) ; not all tested
              (empty? (filter list? results)))        ; all passed 
         (display-untested (filter symbol? results))]
        [else (begin (display-failed (filter list? results)) ; not all tested, not all passed 
                     (display-untested (filter symbol? results)))]))


(test)