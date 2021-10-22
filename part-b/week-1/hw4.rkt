#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map words suffix)
  (map (lambda (word) (string-append word suffix)) words))

(define (list-nth-mod numbers n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? numbers) (error "list-nth-mod: empty list")]
        [else
         (let ([pos (remainder n (length numbers))])
           (car (list-tail numbers pos)))]))

(define (stream-for-n-steps stream steps)
  (letrec ([create-list (lambda (stream steps acc)
                          (if (= steps 0)
                              acc
                              (let ([unpacked-stream (stream)])
                                (create-list (cdr unpacked-stream)
                                             (- steps 1)
                                             (append acc (list (car unpacked-stream)))))))])
    (create-list stream steps null)))