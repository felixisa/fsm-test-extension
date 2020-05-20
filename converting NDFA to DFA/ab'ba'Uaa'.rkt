#lang racket
(require fsm test-engine/racket-tests)


#|

L = (ab)*(ba)*U aa*

S = nothing consumed OR either pattern has been consumed OR both patterns consumed 

A = length is greater than or equal to 1 and the last is a OR the last three is aba

B = 

C =

D =

E =

F =

|#


;S-INV

;A-INV
;if length = 1 the last of ci=a
;if length >1 last three of ci=ab*a

(define ab*ba*Uaa*
  (make-ndfa '(S A B C D E F)
             '(a b)
             'S
             '(S D F)
             `((S a A)
               (S a E)
               (A b B)
               (B a A)
               (B b C)
               (C a D)
               (D b C)
               (D ,EMP S)
               (E a F)
               (F a F)
               (F ,EMP S))
             'no-dead))

