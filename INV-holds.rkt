#lang racket
(require fsm
         test-engine/racket-tests
         "better-sm-test.rkt"
         "dfas.rkt"
         "generate-dfa-tests.rkt"
         "ndfas.rkt"
         "generate-ndfa-tests.rkt")

; INVS-HOLD: (listof test words) (listof (state predicate)) machine -> boolean or (list of (string state))
; Purpose: returns true if for all words the predicates hold.
;          Otherwise, it returns the strings and states for which a predicate fails.
(define (INVS-HOLD tw losp m)
  
  (define (only-empty L) (takef L (lambda (i) (null? (car i)))))
  (define (all-but-empty L) (filter (lambda (i) (not (equal? (car i) '()))) L))

  (define lot (filter (lambda (t) (not (symbol? t))) (map (lambda (i) (sm-showtransitions m i)) tw)))
  
  ; l is a (listof (listof transitions)) with the accept or reject symbol removed
  (define l (map (lambda (i) (reverse (cdr (reverse i)))) lot))
  ; loi is a (listof (listof (consumed state)) 
  (define loi (cons (only-empty l) (map (lambda (t) (for/list ([i (map (lambda (i) (take (caar t) (- (length (caar t)) (length (car i))))) t)]
                                                               [j (map (lambda (i) (cadr i)) t)])
                                                      (list i j))) (all-but-empty l))))
  
  ; helper1: (listof (listof transitions)) (listof (listof transitions)) -> boolean or (listof (listof transitions))
  ; Purpose: Traverses a list of lists of transitions and returns true if all invariant held orreturns failed,
  ;          which is the accumulated list of transitions in which an invariant failed to hold
  (define (helper1 l failed)
    ; helper2: (listof transitions) (listof (state predicate)) -> boolean
    ; Purpose: Traverses a list of transitions asking if, for each transition,
    ;          the predicate indicated by the state in the transition holds for the consumed input at that state
    ;          returning #t if it holds and #f otherwise 
    (define (helper2 L losp)
      (cond [(null? L) #t]
            [(not ((cadar (filter (lambda (i) (equal? (car i) (cadar L))) losp)) (caar L)))
             (list (cadar L) (caar L))]
            [else (helper2 (cdr L) losp)]))

    ; display-failed: (listof transitions) -> (void)
    ; Purpose to display all the transitions listed as errors
    (define (display-failed L)
      (foldl (lambda (i f) (displayln (format "~s INV failed for ~s" (car i) (cadr i)))) (void) L)) 
    
    (cond [(and (null? l)
                (null? failed)) #t]
          [(and (null? l)
                (not (null? failed))) ;(display-failed
                                       (remove-duplicates failed)
                                      ; )
                                      ]
          [(boolean? (helper2 (car l) losp)) (helper1 (cdr l) failed)]
          [else (helper1 (cdr l) (cons (helper2 (car l) losp) failed))]))

  (helper1 loi '()))

; D-INV wrong
 (check-expect (INVS-HOLD '((a) (a a) (a b a) (b b b) (b a b a)) TEST-MACHINE-losp TEST-MACHINE) '((D (b))))

; Q1-INV wrong 
 (check-expect (INVS-HOLD (generate-dfa-tests EVEN-NUM-B) EVEN-NUM-B-losp EVEN-NUM-B) '((Q1 (b))))

; no wrong invs
 (check-expect (INVS-HOLD '((a b a) (a b a a) (a a a) (b)) NO-ABAA-losp NO-ABAA) #t)

; ndfa


;---------------------------------------------------------------

(define (has? x w)
  (if (< (length w) 4)
      #f (or (equal? x (take w 4)) (has? x (rest w)))))

(define Q-0-INV
  (lambda (ci) (not (has? '(a b a a) ci))))

(define Q-1-INV
  (lambda (ci) (equal? (take-right ci 1) '(a))))

(define Q-2-INV
  (lambda (ci) (equal? (take-right ci 2) '(a b))))

(define Q-3-INV
  (lambda (ci) (equal? (take-right ci 3) '(a b a))))

(define Q-4-INV
  (lambda (ci) (equal? (take-right ci 4) '(a b a a))))

(define NO-ABAA-losp (list (list 'Q-0 Q-0-INV)
                           (list 'Q-1 Q-1-INV)
                           (list 'Q-2 Q-2-INV)
                           (list 'Q-3 Q-3-INV)
                           (list 'Q-4 Q-4-INV)))

;---------------------------------------------------------------
; EVEN-NUM-B INVARIANTS
; CHANGED Q1 TO EVEN
(define Q0-INV 
  (lambda (ci) (even? (length (filter (λ (a) (eq? a 'b)) ci)))))

(define Q1-INV
  (lambda (ci) (even? (length (filter (λ (a) (eq? a 'b)) ci)))))

(define EVEN-NUM-B-losp (list (list 'Q0 Q0-INV)
                              (list 'Q1 Q1-INV)))

; e, a, ba, bb

; (sm-showtransitions EVEN-NUM-B '())
; '((() Q0) accept)

(define (INVS-HOLD-e)
  (if (Q0-INV '())
      #t
      (error (format "Q0-INV failed for ~s" '()))))

; (sm-showtransitions EVEN-NUM-B '(a))
; '(((a) Q0) (() Q0) accept)

(define (INV-HOLDS-a)
  (cond [(not (Q0-INV '()))
         (error (format "Q0-INV failed for ~s" '()))]
        [(not (Q0-INV '(a)))
         (error (format "Q0-INV failed for ~s" '(a)))]
        [else #t]))

; (sm-showtransitions EVEN-NUM-B '(b a))
; '(((b a) Q0) ((a) Q1) (() Q1) reject)

(define (INVS-HOLD-ba)
  (cond [(not (Q0-INV '()))
         (error (format "Q0-INV failed for ~s" '()))]
        [(not (Q1-INV '(b)))
         (error (format "Q0-INV failed for ~s" '(b)))]
        [(not (Q1-INV '(b a)))
         (error (format "Q0-INV failed for ~s" '(b a)))]
        [else #t]))

; (sm-showtransitions EVEN-NUM-B '(b b))
; '(((b b) Q0) ((b) Q1) (() Q0) accept)

(define (INVS-HOLD-bb)
  (cond [(not (Q0-INV '()))
         (error (format "Q0-INV failed for ~s" '()))]
        [(not (Q1-INV '(b)))
         (error (format "Q0-INV failed for ~s" '(b)))]
        [(not (Q0-INV '(b b)))
         (error (format "Q0-INV failed for ~s" '(b b)))]
        [else #t]))

;------------------------------------------------------ 

; TEST-MACHINE INVARIANTS 
(define A-INV
  (lambda (ci) (even? (length (filter (lambda (i) (equal? i 'a)) ci)))))

(define B-INV
  (lambda (ci) (and (andmap (lambda (i) (equal? i 'a)) ci)
                    (odd? (length (filter (lambda (i) (equal? i 'a)) ci)))))) 

(define C-INV
  (lambda (ci) (and (>= (length ci))
                    (or (equal? (take-right ci 1) '(a))
                        (equal? (take-right ci 1) '(b))))))

(define D-INV
  (lambda (ci) (or (and (= (length ci) 1)
                        (equal? (car ci) 'a)) ; CHANGED TO 'a FROM 'b
                   (and (= (length (filter (lambda (i) (equal? i 'b)) ci)) 1)
                        (equal? (take-right ci 1) '(a))))))

(define TEST-MACHINE-losp (list (list 'A A-INV)
                                (list 'B B-INV)
                                (list 'C C-INV)
                                (list 'D D-INV)))

; '(() (a a) (b a) (b b) (a b a) (a b b))

; (sm-showtransitions TEST-MACHINE '())
; '((() A) reject)

(define (INV-HOLDS-empty)
  (if (A-INV '())
      #t
      (error (format "A-INV failed for ~s" '()))))

; (sm-showtransitions TEST-MACHINE '(a a))
; '(((a a) A) ((a) B) (() A) reject)

(define (INVS-HOLD-aa)
  (cond [(not (A-INV '()))
         (error (format "A-INV failed for ~s" '()))]
        [(not (B-INV '(a)))
         (error (format "B-INV failed for ~s" '(a)))]
        [(not (A-INV '(a a)))
         (error (format "A-INV failed for ~s" '(a a)))]
        [else #t]))

; (sm-showtransitions TEST-MACHINE '(b a))
; '(((b a) A) ((a) D) (() D) reject)

(define (INVS-HOLD-ba2)
  (cond [(not (A-INV '()))
         (error (format "A-INV failed for ~s" '()))]
        [(not (D-INV '(b)))
         (error (format "A-INV failed for ~s" '(b)))]
        [(not (D-INV '(b a)))
         (error (format "D-INV failed for ~s" '(b a)))]
        [else #t]))

; (sm-showtransitions TEST-MACHINE '(b b))
; '(((b b) A) ((b) D) (() C) accept)

(define (INVS-HOLD-bb2)
  (cond [(not (A-INV '()))
         (error (format "A-INV failed for ~s" '()))]
        [(not (D-INV '(b)))
         (error (format "A-INV failed for ~s" '(b)))]
        [(not (C-INV '(b b)))
         (error (format "C-INV failed for ~s" '(b b)))]
        [else #t]))

; (sm-showtransitions TEST-MACHINE '(a b a))
; '(((a b a) A) ((b a) B) ((a) C) (() C) accept)

(define (INVS-HOLD-aba)
  (cond [(not (A-INV '()))
         (error (format "A-INV failed for ~s" '()))]
        [(not (B-INV '(a)))
         (error (format "B-INV failed for ~s" '(a)))]
        [(not (C-INV '(a b)))
         (error (format "C-INV failed for ~s" '(a b)))]
        [(not (C-INV '(a b a)))
         (error (format "C-INV failed for ~s" '(a b a)))]
        [else #t]))

; (sm-showtransitions TEST-MACHINE '(a b b))
; '(((a b b) A) ((b b) B) ((b) C) (() C) accept)
(define (INVS-HOLD-abb)
  (cond [(not (A-INV '()))
         (error (format "A-INV failed for ~s" '()))]
        [(not (B-INV '(a)))
         (error (format "B-INV failed for ~s" '(a)))]
        [(not (C-INV '(a b)))
         (error (format "C-INV failed for ~s" '(a b)))]
        [(not (C-INV '(a b b)))
         (error (format "C-INV failed for ~s" '(a b b)))]
        [else #t]))

(test) 