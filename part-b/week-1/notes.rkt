#lang racket
(provide (all-defined-out))

; Definitions
(define name "Caue Queiroz")
(define x 42)
(define y (+ x 1))

; Functions
(define (sum x y)
  (+ x y))

(define (pow x y)
  (if (= y 0) 1 (* x (pow x (- y 1)))))

;; Currying
(define build-multiplication
  (lambda (x)
    (lambda (y)
      (* x y))))

(define square
  (build-multiplication 2))

(define cube
  (build-multiplication 3))

; Working with lists
(define (sum-all xs)
  (if (null? xs)
      0
      (+ (car xs) (sum-all (cdr xs)))))

(define (join-lists l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (join-lists (cdr l1) l2))))

(define (map-all fn xs)
  (if (null? xs)
      null
      (cons (fn (car xs)) (map-all fn (cdr xs)))))

; Conditionals
(define (do-math op x y)
  (cond [(string=? op "sum") (+ x y)]
        [(string=? op "sub") (- x y)]
        [else (* x y)]))

; Local bindings
; let    - normal local bidings
; let*   - local bindings that uses the previous local bindings
; letrec - local bindings that allows mutual recursive calls (only with lambdas)
(define (max-number xs)
  (cond [(null? xs) (error "empty list not allowed")]
        [(null? (cdr xs)) (car xs)]
        [else (let ([tail-max-number (max-number (cdr xs))])
                (if (> tail-max-number (car xs))
                    tail-max-number
                    (car xs)))]))
