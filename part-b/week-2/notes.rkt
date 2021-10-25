#lang racket

; Structs
; ----------------------------------------------------------

; Create struct
(struct person (name age rich?) #:transparent)
(define p1 (person "Caue" 26 true))

; Check
(person? p1)

; Consume
(define name (person-name p1))
(define age (person-age p1))
(define rich? (person-rich? p1))


; A simple language
; ----------------------------------------------------------

(struct constant (val) #:transparent)
(struct negation (exp) #:transparent)
(struct addition (exp1 exp2) #:transparent)
(struct multiply (exp1 exp2) #:transparent)

(define (calculator exp)
    (cond [(constant? exp)
           exp]
          
          [(negation? exp)
           (let ([inner-exp (calculator (negation-exp exp))])
             (constant (- (constant-val inner-exp))))]

          [(addition? exp)
           (let ([inner-exp1 (calculator (addition-exp1 exp))]
                 [inner-exp2 (calculator (addition-exp2 exp))])
             (constant (+ (constant-val inner-exp1) (constant-val inner-exp2))))]

          [(multiply? exp)
           (let ([inner-exp1 (calculator (multiply-exp1 exp))]
                 [inner-exp2 (calculator (multiply-exp2 exp))])
             (constant (* (constant-val inner-exp1) (constant-val inner-exp2))))]))

; 2 + 4
(define test1 (addition (constant 2)
                        (constant 4)))

; 4 * 9
(define test2 (multiply (constant 4)
                        (constant 9)))

; 4 * (2 + 3)
(define test3 (multiply (constant 4)
                        (addition (constant 2)
                                  (constant 3))))
             
(calculator test1)
(calculator test2)
(calculator test3)