#lang racket
(require fsm)
(require test-engine/racket-tests)

;; get-pda-rule: processed-list -> pda-rule
;; Purpose: Determins if the rule to be made should be empty or a real rule
(define (get-pda-rule processed-list)
  (cond
    [(< (length processed-list) 2)  '((empty empty empty) (empty empty))]
    [else (construct-pda-rule processed-list)]))

;; construct-pda-rule: processed-list -> pda-rule
;; Purpose: Constructes a pda rule from the given processed list
(define (construct-pda-rule pl)
  (letrec (
           (next-state (caar pl)) ;; The initial state that the machine is in
           (init-state (caadr pl)) ;; The state that the machien ends in
           (next-input (cadar pl)) ;; The initial state's input
           (init-input (cadadr pl)) ;; The state that the machien ends in input
           (next-stack (caddar pl)) ;; The elemetns that are on the init stack
           (sec (cadr pl))  ;; The second list in the stack
           (init-stack (caddr sec)) ;; The elements that are on the next stack

           ;; take*: Integer List -> List or symbol
           ;; Purpose: functions the same as Racket's take function except if the list
           ;;   result of take is the empty list then 'e is returned instead
           (take* (lambda (num a-list)
                    (let ((t (take a-list num)))
                      (if (empty? t) 'e t))))

           ;; determine-consumed: none -> symbol
           ;; Purpose: determins what the input is that is consumed
           (determin-consumed (lambda ()
                                (cond
                                  ;; If both inputs are equal then nothing was consumed
                                  [(equal? init-input next-input)'e]
                                  [else (car init-input)])))

           ;; determin-pushed: none -> integer
           ;; Purpose: Returns the number of elements that have been pushed on the stack
           (determin-pushed (lambda ()
                              (let ((num (- (length next-stack) (length init-stack))))
                                (if (< num 0) 0 num))))

           ;; determin-poped: none -> integer
           ;; Purpose: Returns the number of elements that have been poped off the stack
           (determin-poped (lambda ()
                             (let ((num (- (length init-stack) (length next-stack))))
                               (if (< num 0) 0 num)))))

    (cond
      ;; If there is less then 2 elements then we are at the ed so return the default
      [(< (length pl) 2) '((empty empty empty) (empty empty))]
      [else
       (list
        (list init-state (determin-consumed) (take* (determin-poped) init-stack))
        (list next-state (take* (determin-pushed) next-stack)))])))

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


(test)