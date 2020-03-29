#lang racket
(require fsm)
(require test-engine/racket-tests)

(define EVEN-NUM-B
  (make-dfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0)
            '((Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0))))

(define (Q0-INV consumed-input)
  (even? (length (filter (λ (a) (eq? a 'b)) consumed-input))))

(define (Q1-INV consumed-input)
  (odd? (length (filter (λ (a) (eq? a 'b)) consumed-input))))

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
                        (equal? (car ci) 'b))
                   (and (= (length (filter (lambda (i) (equal? i 'b)) ci)) 1)
                        (equal? (take-right ci 1) '(a))))))

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

; takes as input the list of test words, a (listof (state predicate)),
; and a machine and that returns true if for all words the predicates hold.
; Otherwise, it returns the strings and states for which a predicate fails.

(define (INVS-HOLD tw losp m)
  
  (define loi (map (lambda (i) (cdr (reverse (sm-showtransitions m i)))) tw))
  
  (define (helper1 l failed)
    (begin (println failed)
    (define (helper2 L losp)
      (cond [(null? L) #t]
            [(not ((cadar (filter (lambda (i) (equal? (car i) (cadar L))) losp)) (caar L)))
             #f]
            ; (format "~s-INV failed for ~s" (caar (filter (lambda (i) (equal? (car i) (cadar L))) losp)) (caar L))]
            [else (helper2 (cdr L) losp)]))
    
    (cond [(null? l) failed]
          [(helper2 (car l) losp) (helper1 (cdr l) failed)]
          [else (helper1 (cdr l) (cons (car l) failed))])))

  (helper1 loi '())) 

(define L1 (list (list 'A A-INV)
                 (list 'B B-INV)
                 (list 'C C-INV)
                 (list 'D D-INV)))

(define (helper2 L losp)
  (cond [(null? L) #t]
        [(not ((cadar (filter (lambda (i) (equal? (car i) (cadar L))) losp)) (caar L)))
         #f]
        ; (format "~s-INV failed for ~s" (caar (filter (lambda (i) (equal? (car i) (cadar L))) losp)) (caar L))]
        [else (helper2 (cdr L) losp)]))
;--------------------------------------------------------------------------------
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

; generate-dfa-tests: dfa --> (listof word)
; Purpose: To generate the words needed to test every state of the given dfa
(define (generate-dfa-tests dfa)
  (new-inputs (test-inputs dfa)))
(test) 