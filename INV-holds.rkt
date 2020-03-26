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

(define (A-INV ci)
  (even? (length (filter (lambda (i) (equal? i 'a)) ci))))

(define (B-INV ci)
  (and (andmap (lambda (i) (equal? i 'a)) ci)
       (odd? (length (filter (lambda (i) (equal? i 'a)) ci))))) 

(define (C-INV ci)
  (and (>= (length ci))
       (or (equal? (take-right ci 1) '(a))
           (equal? (take-right ci 1) '(b)))))

(define (D-INV ci)
  (or (and (= (length ci) 1)
           (equal? (car ci) 'b))
      (and (= (length (filter (lambda (i) (equal? i 'b)) ci)) 1)
           (equal? (take-right ci 1) '(a)))))

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