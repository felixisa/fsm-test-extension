#lang racket
(require fsm test-engine/racket-tests)

;;  machines & invs for testing

; M
(define (S-INV ci)
  (empty? ci))

(define (A-INV ci)
  (and (>= (length ci) 1)
       (andmap (λ (s) (eq? s 'a)) ci)))

(define (B-INV ci)
  (and (= (length ci) 1)
       (eq? (first ci) 'a)))

; wrong invariant for testing purposes
(define (B-INV-wrong ci)
  (and (= (length ci) 1)
       (eq? (first ci) 'b)))

(define (C-INV ci)
  (and (eq? (first ci) 'a)
       (andmap (λ (s) (eq? s 'b))
               (rest ci))))

(define M (make-ndfa '(S A B C)
                     '(a b)
                     'S
                     '(A C)
                     `((S a A)
                       (S a B)
                       (A a A)
                       (B ,EMP C)
                       (C b C))
                     'no-dead))

; NO-ABAA
(define (has? x w)
  (if (< (length w) 4)
      #f (or (equal? x (take w 4)) (has? x (rest w)))))

(define Q-0-INV
  (lambda (ci) (not (has? '(a b a a) ci))))

(define Q-1-INV
  (lambda (ci) (equal? (take-right ci 1) '(a))))

; wrong invariant for testing purposes
(define Q-2-INV-wrong
  (lambda (ci) (equal? (take-right ci 2) '(h i))))

(define Q-2-INV
  (lambda (ci) (equal? (take-right ci 2) '(a b))))

(define Q-3-INV
  (lambda (ci) (equal? (take-right ci 3) '(a b a))))

; wrong invariant for testing purposes
(define Q-4-INV-wrong
  (lambda (ci) (equal? (take-right ci 4) '(a b b a))))

(define Q-4-INV
  (lambda (ci) (and (has? '(a b a a) ci)
                    (or (eq? (last ci) 'a) (eq? (last ci) 'b)))))

(define NO-ABAA
  (make-dfa 
   '(Q-0 Q-1 Q-2 Q-3 Q-4)
   '(a b) 
   'Q-0
   '(Q-0 Q-1 Q-2 Q-3)
   '((Q-0 a Q-1)
     (Q-0 b Q-0)
     (Q-1 a Q-1)
     (Q-1 b Q-2)
     (Q-2 a Q-3 )
     (Q-2 b Q-0)
     (Q-3 a Q-4)
     (Q-3 b Q-2)
     (Q-4 a Q-4)
     (Q-4 b Q-4))
   'nodead))

; EVEN-NUM-B
(define Q0-INV 
  (lambda (ci) (even? (length (filter (λ (a) (eq? a 'b)) ci)))))

(define Q1-INV
  (lambda (ci) (even? (length (filter (λ (a) (eq? a 'b)) ci)))))

(define EVEN-NUM-B
  (make-dfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0)
            '((Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0))
            'nodead))

; detect-motif

(define (L-INV ci)
  (empty? ci))

(define (M-INV ci)
  (or (and (= (length ci) 1)
           (eq? (first ci) 'm))
      (and (>= (length ci) 1)
           (andmap (λ (i) (eq? i 'f)) (take ci (sub1 (length ci))))
           (eq? (last ci) 'g))))

(define (N-INV ci)
  (and (>= (length ci) 1)
       (M-INV (takef ci (λ (i) (not (eq? i 'a)))))
       (or (eq? (last ci) 'g)
           (eq? (last ci) 'm)
           (eq? (last ci) 'a))))

(define (O-INV ci)
  (and (>= (length ci) 2)
       (N-INV (takef ci (λ (i) (not (eq? i 'd)))))
       (andmap (λ (i) (eq? i 't)) (rest (dropf ci (λ (i) (not (eq? i 'd)))))))) 

(define (P-INV ci)
  (and (>= (length ci) 3)
       (O-INV (drop-right ci 1))
       (eq? (last ci) 't)))

(define (Q-INV ci)
  (and (>= (length ci) 4)
       (P-INV (drop-right ci 1))
       (eq? (last ci) 'a)))

(define (R-INV ci)
  (and (>= (length ci) 5)
       (Q-INV (drop-right ci 1))
       (eq? (last ci) 'a)))

(define (Z-INV ci)
  (and (>= (length ci) 6)
       (R-INV (drop-right ci 1))
       (not (empty? (member (last ci) '(m n q))))))

(define (T-INV ci)
  (and (>= (length ci) 7)
       (let ((Q7-word (dropf-right ci (λ (i) (or (eq? i 'k)
                                                 (eq? i 's)
                                                 (eq? i 't)
                                                 (eq? i 'a)))))
             (drop-Q7 (takef-right ci (λ (i) (or (eq? i 'k)
                                                 (eq? i 's)
                                                 (eq? i 't)
                                                 (eq? i 'a))))))
         (and (Z-INV Q7-word)
              (or (eq? (first drop-Q7) 'k) (eq? (first drop-Q7) 's) (eq? (first drop-Q7) 't))
              (andmap (λ (i) (eq? i 'a)) (rest drop-Q7))))))

(define (U-INV ci)
  (andmap (λ (i) (eq? i 'f)) ci))

(define detect-motif
  (make-ndfa '(L M N O P Q R Z T U)
             '(a d g f k m n q s t)
             'L
             '(T)
             `((L ,EMP U)
               (L m M)
               (U f U)
               (M a N)
               (M ,EMP N)
               (N d O)
               (O t P)
               (O t O)
               (P a Q)
               (Q a R)
               (R m Z)
               (R n Z)
               (R q Z)
               (Z k T)
               (Z s T)
               (Z t T)
               (T a T)
               (U g M))
             'no-dead))

; kleenestar abUaba
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

; at least one missing
(define AT-LEAST-ONE-MISSING
  (make-ndfa '(A B C D)
             '(a b c)
             'A
             '(B C D)
             `((A ,EMP B)
               (A ,EMP C)
               (A ,EMP D)
               (B b B)
               (B c B)
               (C a C)
               (C c C)
               (D a D)
               (D b D))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  test word generator function

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
    ; Purpose: To update u-words accumulator; uses all transitions out of the last state in the path of the first u-word
    ;          to creat all possible strings that can come from adding one letter to the first u-word
    (define (new-u-words u-words)
      (cond [(not (false? (member (last (caar u-words)) e-states))) (cdr u-words)] ; if last state in path of first u-word is expanded, rest of u-words
            [else (map (lambda (rule) (list (append (caar u-words) (list (caddr rule))) ; else, generate new u-words by using each possible transition to add a state to the path 
                                            (append (cadar u-words) (list (cadr rule))))) ; and add a letter or empty transition to the string
                       (filter (λ (i) (false? (member i e-trans))) (find-all-trans (last (caar u-words)))))])) ; only use transitions that haven't been expanded yet

    ; are-these-in-there?: list list -> boolean
    ; Purpose: Determines if all the contents of L1 are in L2 
    (define (are-these-in-there? L1 L2)
      (and (>= (length L2) (length L1))
           (andmap (lambda (i) (not (false? (member i L2)))) L1)))
    
    ; new-e-states: (listof state) -> (listof state)
    ; Purpose: To update e-states accumulator; adds the given state to the accumulator only if all the transitions
    ;          out of it have been expanded
    (define (new-e-states st)
      (if (are-these-in-there? (find-all-trans st) e-trans)
          (cons st e-states)
          e-states))
    
    (cond [(null? u-words) t-words] ; when there are no more unfinished words to be expanded, return list of test words
          [(member (last (caar u-words)) e-states) ; if the last state in the path of the first u-word is expanded,
           (helper (cons (car u-words) t-words) e-trans (cdr u-words) e-states)] ; add the first u-word to t-words and proceed with the rest of u-wprds
          [else (helper (cons (car u-words) t-words) ; else, add the first u-word to t-words
                        (append (find-all-trans (last (caar u-words))) e-trans) ; add all transitions out of the last state in the path of the first u-word to e-trans
                        (append (cdr u-words) (new-u-words u-words)) ; update u-words using new-u-words
                        (new-e-states (last (caar u-words))))])) ; update e-states with new-e-states

  (reverse (helper '() '() (list (list (list (sm-getstart m)) '())) '())))

; new-inputs: (listof path word) -> (listof path word)
; Purpose: To remove redundant test words
(define (new-inputs loi)
  (cond [(null? loi) '()]
        [(null? (cadar loi)) (cons (car loi) (new-inputs (cdr loi)))] ; keeps the test word with just the empty string and start state
        [(not (ormap (lambda (i) (and (equal? (caar loi) (take (car i) (length (caar loi)))) ; if the test word's path is not part of any other test word's path and
                                      (equal? (cadar loi) (take (cadr i) (length (cadar loi)))))) (cdr loi))) ; if the test word's word is not part of any other word,
         (cons (car loi) (new-inputs (cdr loi)))] ; keep it in the list of test words
        [else (new-inputs (cdr loi))])) ; else, continue with the rest of the test words

; testing123: ndfa -> (listof path word)
; Purpose: To generate test words and their paths
(define (testing123 m)
  (new-inputs (test-inputs m)))

;; tests for testing123

;(sm-graph M)
(check-expect (testing123 M) '(((S) ())
                               ((S A A) (a a))
                               ((S B C C) (a ε b))))
;(sm-graph NO-ABAA)
(check-expect (testing123 NO-ABAA) '(((Q-0) ())
                                     ((Q-0 Q-0) (b))
                                     ((Q-0 Q-1 Q-1) (a a))
                                     ((Q-0 Q-1 Q-2 Q-0) (a b b))
                                     ((Q-0 Q-1 Q-2 Q-3 Q-2) (a b a b))
                                     ((Q-0 Q-1 Q-2 Q-3 Q-4 Q-4) (a b a a a))
                                     ((Q-0 Q-1 Q-2 Q-3 Q-4 Q-4) (a b a a b))))
;(sm-graph EVEN-NUM-B)
(check-expect (testing123 EVEN-NUM-B) '(((Q0) ())
                                        ((Q0 Q0) (a))
                                        ((Q0 Q1 Q1) (b a))
                                        ((Q0 Q1 Q0) (b b))))
;(sm-graph detect-motif)
(check-expect (testing123 detect-motif) '(((L) ())
                                          ((L U U) (ε f))
                                          ((L U M) (ε g))
                                          ((L M N) (m ε))
                                          ((L M N O O) (m a d t))
                                          ((L M N O P Q R Z) (m a d t a a n))
                                          ((L M N O P Q R Z) (m a d t a a q))
                                          ((L M N O P Q R Z T) (m a d t a a m s))
                                          ((L M N O P Q R Z T) (m a d t a a m t))
                                          ((L M N O P Q R Z T T) (m a d t a a m k a))))
;(sm-graph AT-LEAST-ONE-MISSING)
(check-expect (testing123 AT-LEAST-ONE-MISSING) '(((A) ())
                                                  ((A B B) (ε b))
                                                  ((A B B) (ε c))
                                                  ((A C C) (ε a))
                                                  ((A C C) (ε c))
                                                  ((A D D) (ε a))
                                                  ((A D D) (ε b))))
;(sm-graph KLEENESTAR-abUaba)
(check-expect (testing123 KLEENESTAR-abUaba) '(((Q0) ())
                                               ((Q0 Q4 Q5 Q0) (a b ε))
                                               ((Q0 Q1 Q2 Q3 Q0) (a b a ε))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; invariant tester function

; invariant-tester: machine, optional arg: predicates -> void
; Purpose: Tests invariants
(define (invariant-tester m . invs)
  
  ; helper: machine (listof invariants) -> list of stuff
  ; Purpose: Tests invariants 
  (define (helper m invs)

    ; list of test words produced by testing123
    (define lotw (testing123 m))

    ; test-inv: (list path word) -> boolean OR state OR (list state word)
    ; Purpose: Tests invariants by traversing path and word of a single given test word at the same time
    (define (test-inv t-word)
    
      ;find-inv: state -> predicate
      ; Purpose: To find the invariant associated with the given state
      (define (find-inv state)
        (filter (λ (i) (eq? (first i) state)) invs))

      ;test-inv-help: path word number -> boolean OR state OR (list state word)
      ; Purpose: Tests invariants by traversing path and word at the same time
      ; ACCUM: Index is a number used to reference the given word at each step. It increases by one on each recursive call. 
      (define (test-inv-help path word index)
        (cond [(empty? path) '()] ; if the path is empty, all invariants have been processed 
              [(empty? (find-inv (first path))) ; if there is no invariant provided, 
               (cons (first path) (test-inv-help (rest path) word (add1 index)))] ; add the state to the recursive call
              [((last (first (find-inv (first path)))) (filter (λ (i) (not (eq? i EMP))) (take word index))) ; if the invariant holds, 
               (cons #t (test-inv-help (rest path) word (add1 index)))] ; add true to the recursive call
              [else (cons (list (first path) (take word index)) (test-inv-help (rest path) word (add1 index)))])) ; else, add failed state and string to recursive call

      (test-inv-help (car t-word) (cadr t-word) 0)) ; index starts at 0

    (remove-duplicates (append-map test-inv lotw))) ; appends the results of mapping test-inv on the list of test words generated by testing123

  ; display-failed: (listof X) -> void
  ; Purpose: displays failed words
  (define (display-failed L)
    (foldl (lambda (i f) (displayln (format "~s INV failed for ~s" (car i) (cadr i)))) (void) L))

  ; display-failed: (listof X) -> void
  ; Purpose: displays untested invs
  (define (display-untested L)
    (foldl (lambda (i f) (displayln (format "~s INV not provided" i))) (void) L))

  (define results (helper m invs)) ; list of different results generated by helper
  ; if invariant holds -> #t
  ; if invariant not provided -> state
  ; if invariant failed -> (list state word)

  (cond [(and (andmap boolean? results)          ; all passed
              (empty? (filter symbol? results))) ; all tested
         (displayln "All invariants are holding")]
        [else (begin (display-failed (filter list? results))  ; else, display failed and untested 
                     (display-untested (filter symbol? results)))]))

;; tests for invariant tester

; all invariants provided and correct
(check-expect (invariant-tester M (list 'S S-INV) (list 'A A-INV) (list 'B B-INV) (list 'C C-INV))
              (displayln "All invariants are holding"))
(check-expect (invariant-tester NO-ABAA (list 'Q-0 Q-0-INV) (list 'Q-1 Q-1-INV) (list 'Q-2 Q-2-INV) (list 'Q-3 Q-3-INV) (list 'Q-4 Q-4-INV))
              (displayln "All invariants are holding"))
(check-expect (invariant-tester detect-motif (list 'L L-INV) (list 'M M-INV) (list 'N N-INV) (list 'O O-INV) (list 'P P-INV) (list 'Q Q-INV) (list 'R R-INV) (list 'Z Z-INV) (list 'T T-INV) (list 'U U-INV))
              (displayln "All invariants are holding"))

; some invariants failed
(check-expect (invariant-tester M (list 'S S-INV) (list 'A A-INV) (list 'B B-INV-wrong) (list 'C C-INV))
              (displayln "B INV failed for (a)"))
(check-expect (invariant-tester NO-ABAA (list 'Q-0 Q-0-INV) (list 'Q-1 Q-1-INV) (list 'Q-2 Q-2-INV-wrong) (list 'Q-3 Q-3-INV) (list 'Q-4 Q-4-INV-wrong))
              (begin (displayln "Q-2 INV failed for (a b)")
                     (displayln "Q-2 INV failed for (a b a b)")
                     (displayln "Q-4 INV failed for (a b a a)")
                     (displayln "Q-4 INV failed for (a b a a a)")
                     (displayln "Q-4 INV failed for (a b a a b)")))
(check-expect (invariant-tester detect-motif (list 'L M-INV) (list 'M L-INV) (list 'N N-INV) (list 'O O-INV) (list 'P P-INV) (list 'Q Q-INV) (list 'R R-INV) (list 'Z Z-INV) (list 'T T-INV) (list 'U U-INV))
              (begin (displayln "L INV failed for ()")
                     (displayln "M INV failed for (ε g)")
                     (displayln "M INV failed for (m)")))

; some invariants not provided
(check-expect (invariant-tester M (list 'S S-INV) (list 'A A-INV) (list 'B B-INV))
              (displayln "C INV not provided"))
(check-expect (invariant-tester NO-ABAA)
              (begin (displayln "Q-0 INV not provided")
                     (displayln "Q-1 INV not provided")
                     (displayln "Q-2 INV not provided")
                     (displayln "Q-3 INV not provided")
                     (displayln "Q-4 INV not provided")))
(check-expect (invariant-tester detect-motif (list 'N N-INV) (list 'O O-INV) (list 'P P-INV) (list 'Q Q-INV) (list 'R R-INV) (list 'Z Z-INV) (list 'T T-INV) (list 'U U-INV))
              (begin (displayln "L INV not provided")
                     (displayln "M INV not provided")))

; some invariants failed and some not provided
(check-expect (invariant-tester M (list 'S S-INV) (list 'A A-INV) (list 'B B-INV-wrong))
              (begin (displayln "B INV failed for (a)")
                     (displayln "C INV not provided")))
(check-expect (invariant-tester NO-ABAA (list 'Q-1 Q-1-INV) (list 'Q-2 Q-2-INV-wrong) (list 'Q-3 Q-3-INV) (list 'Q-4 Q-4-INV))
              (begin (displayln "Q-2 INV failed for (a b)")
                     (displayln "Q-2 INV failed for (a b a b)")
                     (displayln "Q-0 INV not provided")))
(check-expect (invariant-tester detect-motif (list 'N U-INV) (list 'O O-INV) (list 'P P-INV) (list 'Q Q-INV) (list 'R R-INV) (list 'Z Z-INV) (list 'T T-INV) (list 'U N-INV))
              (begin (displayln "U INV failed for (ε)")
                     (displayln "U INV failed for (ε f)")
                     (displayln "N INV failed for (m ε)")
                     (displayln "N INV failed for (m a)")
                     (displayln "L INV not provided")
                     (displayln "M INV not provided")))

(test)