;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist items)
  (cond [(null? items) (aunit)]
        [else (apair (car items)
                     (racketlist->mupllist (cdr items)))]))

(define (mupllist->racketlist items)
  (cond [(aunit? items) '()]
        [else (cons (apair-e1 items)
                    (mupllist->racketlist (apair-e2 items)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        ;; CHANGE add more cases here
        [(int? e) e]

        [(ifgreater? e)
         (let ([e1 (eval-under-env (ifgreater-e1 e) env)]
               [e2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? e1) (int? e2))
               (if (> (int-num e1) (int-num e2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-numbers")))]

        [(mlet? e)
         (let ([name (mlet-var e)]
               [value (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons name value) env)))]

        [(fun? e) (closure env e)]
        
        [(closure? e) e]

        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [arg-val (eval-under-env (call-actual e) env)])
           (if (not (closure? c))
               (error "First argument should be a closure")
               (eval-under-env (fun-body (closure-fun c))
                               (if (fun-nameopt (closure-fun c))
                                   (cons (cons (fun-nameopt (closure-fun c)) c)
                                         (cons (cons (fun-formal (closure-fun c)) arg-val)
                                               (closure-env c)))
                                   (cons (cons (fun-formal (closure-fun c)) arg-val)
                                         (closure-env c))))))]

        [(apair? e)
         (let ([e1 (eval-under-env (apair-e1 e) env)]
               [e2 (eval-under-env (apair-e2 e) env)])
           (apair e1 e2))]

        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (not (apair? p))
               (error "FST must receive a pair")
               (apair-e1 p)))]
        
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (not (apair? p))
               (error "SND must receive a pair")
               (apair-e2 p)))]

        [(aunit? e) e]
        
        [(isaunit? e)
         (let ([u (eval-under-env (isaunit-e e) env)])
           (if (aunit? u)
               (int 1)
               (int 0)))]

               
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))
