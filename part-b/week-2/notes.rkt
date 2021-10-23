#lang racket

; Create struct
(struct person (name age rich?) #:transparent)
(define p1 (person "Caue" 26 true))

; Check
(person? p1)

; Consume
(define name (person-name p1))
(define age (person-age p1))
(define rich? (person-rich? p1))