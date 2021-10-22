#lang racket
(require "hw4.rkt")
(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")
   (check-equal? (sequence 3 11 2) (list 3 5 7 9 11) "Sequence test")
   (check-equal? (sequence 3 8 3) (list 3 6) "Sequence test")
   (check-equal? (sequence 3 2 1) null "Sequence test")

   ; string-append-map test
   (check-equal? (string-append-map null ".jpg") null "string-append-map test")
   (check-equal? (string-append-map (list "Caue") " Queiroz") (list "Caue Queiroz") "string-append-map test")
   (check-equal? (string-append-map (list "dan" "dog" "curry" "dog2") ".jpg")
                 '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")

   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")

   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps ones 0) null "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps ones 1) (list 1) "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
