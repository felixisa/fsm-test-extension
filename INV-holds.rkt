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

; (sm-showtransitions EVEN-NUM-B '(a))
; '(((a) Q0) (() Q0) accept)

; (sm-showtransitions EVEN-NUM-B '(b a))
; '(((b a) Q0) ((a) Q1) (() Q1) reject)

; (sm-showtransitions EVEN-NUM-B '(b b))
; '(((b b) Q0) ((b) Q1) (() Q0) accept)

(define (INVS-HOLD-e)
  (if (Q0-INV '())
      #t
      (error (format "Q0-INV failed for ~s" '()))))

(define (INVS-HOLD-ba)
  (cond [(not (Q0-INV '()))
         (error (format "Q0-INV failed for ~s" '()))]
        [(not (Q1-INV '(b)))
         (error (format "Q0-INV failed for ~s" '(b)))]
        [(not (Q1-INV '(b a)))
         (error (format "Q0-INV failed for ~s" '(b a)))]
        [else #t])) 

(test) 