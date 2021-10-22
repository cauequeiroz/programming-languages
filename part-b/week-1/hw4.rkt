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


(define funny-number-stream
  (letrec ([next-number (lambda (n)
                          (cons (if (= (remainder n 5) 0) (* n -1) n)
                                (lambda () (next-number (+ n 1)))))])
    (lambda () (next-number 1))))


(define dan-then-dog
  (letrec ([next-item (lambda (x)
                        (cons (if x "dan.jpg" "dog.jpg")
                              (lambda () (next-item (not x)))))])
    (lambda () (next-item #t))))


(define (stream-add-zero stream)
  (let ([next-item (lambda (item stream)
                        (cons (cons 0 item)
                              (stream-add-zero stream)))])
    (lambda () (next-item (car (stream)) (cdr (stream))))))


(define (cycle-lists xs ys)
  (letrec ([next-item (lambda (pos)
                        (cons (cons (list-nth-mod xs pos)
                                    (list-nth-mod ys pos))
                              (lambda () (next-item (+ pos 1)))))])
    (lambda () (next-item 0))))


(define (vector-assoc x items)
  (letrec ([loop (lambda (x items count)
                (if (= count (vector-length items))
                    #f
                    (if (and (pair? (vector-ref items count))
                             (not (list? (vector-ref items count)))
                             (equal? x (car (vector-ref items count))))
                        (vector-ref items count)
                        (loop x items (add1 count)))))])
    (loop x items 0)))
  
















  