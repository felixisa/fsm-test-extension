#lang racket
(require fsm)
(provide P)
(provide pda-wcw^r)
(provide pda-numa=numb)

;; valid input: aaabbb 
(define P (make-ndpda '(S F)
                      '(a b)
                      '(c)
                      'S
                      '(F)
                      `(((S ,EMP ,EMP) (F ,EMP))
                        ((F a ,EMP) (F (c)))
                        ((F b (c)) (F ,EMP)))))

; valid input: aabcbaa
(define pda-wcw^r (make-ndpda '(S M N F)                  ;the states
                              '(a b c)                    ;the alphabet
                              '(a b)                      ;the stack alphabet
                              'S                          ;the starting state
                              '(F)                        ;the final state
                              `(((S ,EMP ,EMP) (M ,EMP))  ;the transition relation
                                ((M a ,EMP) (M (a)))
                                ((M b ,EMP) (M (b)))
                                ((M c ,EMP) (N ,EMP))
                                ((N a (a)) (N ,EMP))
                                ((N b (b)) (N ,EMP))
                                ((N ,EMP ,EMP) (F ,EMP)))))

(define (S-INV ci s)
  (and (empty? ci) (empty? s)))

(define (M1-INV ci s)
  (and (not (member 'c ci)) (equal? ci (reverse s))))

(define (beforec lst)
  (if (eq? (car lst) 'c)
      '()
      (cons (car lst) (beforec (cdr lst)))))

(define (afterc lst)
  (if (eq? (car lst) 'c)
      (cdr lst)
      (afterc (cdr lst))))

(define (M2-INV ci s)
    (and (member 'c ci)                            ; c in ci
       (let [(bc (beforec ci))
             (ac (afterc ci))
             (rs (reverse s))]
         (and (equal? (take bc (length rs)) rs)    ; n stack elements match first n ci elements
              (= (length (append s ac))
                 (length bc))
              (equal? (reverse ac)                 ; popped elements (after c) match the end of ci
                      (take-right bc (length ac)))))))


(define (F-INV ci s)
  (and (member 'c ci)
       (and (equal? (beforec ci) (reverse (afterc ci))))))


(define pda-numa=numb (make-ndpda '(S M F)
                                  '(a b)
                                  '(a b)
                                  'S
                                  '(F)
                                  `(((S ,EMP ,EMP) (M ,EMP))
                                    ((M ,EMP ,EMP) (F ,EMP))
                                    ((M a ,EMP) (M (a)))
                                    ((M b ,EMP) (M (b)))
                                    ((M a (b)) (M ,EMP))
                                    ((M b (a)) (M ,EMP)))))

(define (s-INV ci s) (and (empty? ci) (empty? s)))

(define (M-INV ci s)
  (and (or (andmap (λ (k) (eq? k 'a)) s)
           (andmap (λ (k) (eq? k 'b)) s))
       (implies (empty? s)
                (= (length (filter (λ (k) (eq? k `a)) ci))
                   (length (filter (λ (k) (eq? k `b)) ci))))
       (implies (not (empty? s))
                (and (implies (eq? (first s) `a)
                              (= (- (length (filter (λ (k) (eq? k `a)) ci)) (length s))
                                 (length (filter (λ (k) (eq? k 'b)) ci))))
                     (implies (eq? (first s) `b)
                              (= (- (length (filter (λ (k) (eq? k 'b)) ci)) (length s))
                                 (length (filter (λ (k) (eq? k 'a)) ci))))))))

(define (f-INV ci s)
  (= (length (filter (λ (k) (eq? k 'a)) ci))
     (length (filter (λ (k) (eq? k 'b)) ci))))
