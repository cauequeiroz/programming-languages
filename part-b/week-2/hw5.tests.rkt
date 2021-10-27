#lang racket
(require "hw5.rkt")
(require rackunit)

(define tests
  (test-suite
   "Complete tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist '()) (aunit) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (int 1))) (apair (int 1) (aunit)) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (int 1) (int 2) (int 3)))
                 (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))) "racketlist->mupllist test")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (aunit)) '() "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 1) (aunit))) (list (int 1)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit))))
                 (list (int 3) (int 4)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 2) (apair (int 3) (apair (int 4) (aunit)))))
                 (list (int 2) (int 3) (int 4)) "racketlist->mupllist test")

   ;; check int
   (check-equal? (eval-exp (int 17)) (int 17))

   ;; check var
   (check-equal? (eval-under-env (var "age") (list (cons "age" (int 18)))) (int 18))

   ;; check add
   (check-equal? (eval-exp (add (int 2) (int 4))) (int 6))

   ;; check ifgreater
   (check-equal? (eval-exp (ifgreater (int 4) (int 3) (int 3) (int 2))) (int 3) "ifgreater test")
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")

   ;; check mlet
   (check-equal? (eval-exp (mlet "age" (int 25) (var "age"))) (int 25) "mlet test")
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")

   ;; check fun
   (check-equal? (eval-exp (fun #f "x" (add (var "x") (int 7))))
                 (closure '() (fun #f "x" (add (var "x") (int 7)))))

   ;; check closure
   (check-equal? (eval-exp (closure '() (fun #f "x" (add (var "x") (int 7)))))
                 (closure '() (fun #f "x" (add (var "x") (int 7)))))

   ;; check call
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")

   ;; check apair
   (check-equal? (eval-exp (apair (int 2) (add (int 1) (int 1)))) (apair (int 2) (int 2)))
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1))
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2))

   ;; check aunit
   (check-equal? (eval-exp (aunit)) (aunit))
   
   ;; check isaunit
   (check-equal? (eval-exp (isaunit (aunit))) (int 1) "isaunit test")
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   (check-equal? (eval-exp (isaunit (call (closure '() (fun #f "x" (aunit))) (int 1)))) (int 1) "isaunit test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)