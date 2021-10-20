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

; fancy stuff
; (mcons ... ...) create mutable cons structure
; (set! variable-name new-value) to mutate previous variables
; (set-mcar! / set-mcdr! variable-name new-value) update mcons structure values
; (begin e1 e2 ... en) executes each expression and return the last one
; (lambda() exp) creates a thunk

; Stream
; pairs with (value, next-stream)

; 1 1 1 1 ...
(define stream-ones (lambda() (cons 1 stream-ones)))

; 1 2 3 4 ...
(define stream-naturals
  (letrec ([next-natural (lambda (x)(cons x (lambda() (next-natural (+ x 1)))))])
    (lambda() (next-natural 1))))

; 2 4 8 16 ...
(define stream-doubles
  (letrec ([next-double (lambda (n) (cons n (lambda() (next-double (* n 2)))))])
    (lambda () (next-double 2))))

; Consuming a stream
; (how-many-until stream-doubles (lambda (x) (> x 10)))
(define (how-many-until stream condition)
  (letrec ([loop (lambda (stream acc)
                (let* ([unpacked-stream (stream)]
                       [stream-value (car unpacked-stream)]
                       [stream-next (cdr unpacked-stream)])
                  (if (condition stream-value)
                      acc
                      (loop stream-next (+ acc 1)))))])
    (loop stream 1)))