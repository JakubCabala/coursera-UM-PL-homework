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

(define (racketlist->mupllist racket-list)
  (cond [(null? (cdr racket-list)) (apair (car racket-list) (aunit))]
        [#t (apair (car racket-list) (racketlist->mupllist (cdr racket-list)))]))

(define (mupllist->racketlist mulp-list)
  (cond [(aunit? (apair-e2 mulp-list)) (cons (apair-e1 mulp-list) null)]
        [#t (cons (apair-e1 mulp-list) (mupllist->racketlist(apair-e2 mulp-list)))]))
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
        [(int? e) e]
        [(closure? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "first two arguments of ifgreaer must be of type int")))]
        [(mlet? e) 
         (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
        [(call? e)
         (if (closure? (call-funexp e))
         (eval-under-env (fun-body (closure-fun (call-funexp e))) (cons (cons (fun-formal (closure-fun (call-funexp e))) (call-actual e)) (closure-env (call-funexp e))))
         (error "wrong syntax for a functional call"))]
        [(apair? e) e]
        [(fst? e)
         (if (apair? (fst-e e))
             (apair-e1 (fst-e e))
             (error "wrong syntax for pair"))]
        [(snd? e)
         (if (apair? (snd-e e))
             (apair-e2 (snd-e e))
             (error "wrong syntax for pair"))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (= 1 (int-num (eval-exp (isaunit e1)))) e2 e3))

(define (mlet* lstlst e)
  (if (pair? lstlst)
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e))
      e))

(define (ifeq e1 e2 e3 e4)
  (let ([v1 (eval-exp e1)]
        [v2 (eval-exp e2)])
    (if (and (and (int? v1) (int? v2)) (= (int-num v1) (int-num v2)))
        e3
        e4)))

;; Problem 4

(define mupl-map
  (fun #f "fx"
       (fun "map" "lst"
            (ifaunit (var "lst") (var "lst")
                     (apair (call (var "fx") (fst (var "lst"))) (call (var "map") (snd (var "lst"))))))))
  

(define mupl-mapAddN 
  (mlet "map" mupl-map ;map is now in MUPL scope
        (fun #f "i"
             (fun #f "xs"
                  (call (call (var "map") (fun #f "e" (add (var "i") (var "e"))))
                        (var "xs"))))))


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
