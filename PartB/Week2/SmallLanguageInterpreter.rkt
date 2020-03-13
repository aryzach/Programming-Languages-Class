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

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rlst)
  (cond
    [(null? rlst) (aunit)]
    [#t (apair (car rlst) (racketlist->mupllist (cdr rlst)))]))

(define (mupllist->racketlist mlst)
  (cond
    [(equal? mlst (aunit)) null]
    [#t (cons (apair-e1 mlst) (mupllist->racketlist (apair-e2 mlst)))]))

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
  ;;(begin (print "Ex: ") (print e) (print "env: ") (print env)
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
         (let ([e1evaled (eval-under-env (ifgreater-e1 e) env)]
               [e2evaled (eval-under-env (ifgreater-e2 e) env)])
           (cond
             [(and (int? e1evaled) (int? e2evaled))
              (if (> (int-num e1evaled) (int-num e2evaled))
                  (eval-under-env (ifgreater-e3 e) env)
                  (eval-under-env (ifgreater-e4 e) env))]
              [#t (error "ifgreater e1 or e2 not MUPL ints")]))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([clos (eval-under-env (call-funexp e) env)]
               [act (eval-under-env (call-actual e) env)])
           (cond
             [(not (closure? clos)) (error "call on a non-closure")]
             ;; if fun-nameopt if not false, add to env when calling fun, and actual when calling fun body
             [(fun-nameopt (closure-fun clos))
              (let ([env-plus-fun-and-actual-and-cloj-env
                     (append (closure-env clos) (cons (cons (fun-nameopt (closure-fun clos)) clos)
                                   (cons (cons (fun-formal (closure-fun clos)) act) env)))])
                (eval-under-env (fun-body (closure-fun clos)) env-plus-fun-and-actual-and-cloj-env))]
             ;; if fun-nameopt is false, add just actual to env when calling fun body
             [#t (let ([env-plus-actual-and-cloj-env
                        (append (closure-env clos) (cons (cons (fun-formal (closure-fun clos)) act) env))])
                   (eval-under-env (fun-body (closure-fun clos)) env-plus-actual-and-cloj-env))]))]
        [(mlet? e)
         (let ([evaledexpression (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) evaledexpression) env)))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([thepair (eval-under-env (fst-e e) env)])
             (cond
               [(apair? thepair) (apair-e1 thepair)]
               [#t (error "gave non-pair to fst")]))]
        [(snd? e)
         (let ([thepair (eval-under-env (snd-e e) env)])
           (cond
             [(apair? thepair) (apair-e2 thepair)]
             [#t (error "gave non-pair to snd")]))]
        [(aunit? e) e]
        [(isaunit? e) (cond
                        [(aunit? (eval-under-env (isaunit-e e) env)) (int 1)]
                        [#t (int 0)])]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) (cond
                            [(null? lstlst) e2]
                            [#t (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))]))


(define (ifeq e1 e2 e3 e4) (mlet* (list (cons "_x" e1) (cons "_y" e2))
                                  (ifgreater (var "_x") (var "_y") e4
                                             (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "a"
       (fun "f" "mlst" (ifaunit (var "mlst")
                               (aunit)
                               (apair (call (var "a") (fst (var "mlst"))) (call (var "f") (snd (var "mlst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "n" (call (var "map") (fun #f "g" (add (var "g") (var "n")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (struct both (e fvs))
  (letrec ([f (lambda (e)
             (cond [(var? e) (both e (set (var-string e)))]
                   [(add? e)
                    (let ([v1 (f (add-e1 e))]
                          [v2 (f (add-e2 e))])
                      (both (add (both-e v1) (both-e v2))
                            (set-union (both-fvs v1) (both-fvs v2))))]
                   [(int? e) (both e (set))]
                   [(ifgreater? e)
                    (let ([v1 (f (ifgreater-e1 e))]
                          [v2 (f (ifgreater-e2 e))]
                          [v3 (f (ifgreater-e3 e))]
                          [v4 (f (ifgreater-e4 e))])
                      (both (ifgreater (both-e v1) (both-e v2) (both-e v3) (both-e v4))
                            (set-union (both-fvs v1) (both-fvs v2) (both-fvs v3) (both-fvs v4))))]
                   [(fun? e) (both
                              (fun-challenge
                               (fun-nameopt e)
                               (fun-formal e)
                               (both-e (f (fun-body e)))
                               ;; both-e function-challenge-fvs
                               (cond
                                 [(fun-nameopt e) (set-union (both-fvs (f (fun-body e))) (set (fun-nameopt e) (fun-formal e)))]
                                 [#t (set-union (both-fvs (f (fun-body e))) (set (fun-formal e)))]))
                              ;; both-fvs
                              (cond
                                 [(fun-nameopt e) (set-union (both-fvs (f (fun-body e))) (set (fun-nameopt e) (fun-formal e)))]
                                 [#t (set-union (both-fvs (f (fun-body e))) (set (fun-formal e)))]))]
                   [(fun-challenge? e) (both e (fun-challenge-freevars e))]
                   [(call? e)
                    (let ([fxp (f (call-funexp e))]
                          [a (f (call-actual e))])
                      (both (call (both-e fxp) (both-e a)) (set-union (both-fvs fxp) (both-fvs a))))]
                   [(mlet? e)
                    (let ([expr (f (mlet-e e))]
                          [bod (f (mlet-body e))])
                      (both (mlet (mlet-var e) (both-e expr) (both-e bod)) (set-union (both-fvs expr) (both-fvs bod) (set (mlet-var e)))))]
                   [(apair? e)
                    (let ([v1 (f (apair-e1 e))]
                          [v2 (f (apair-e2 e))])
                      (both (apair (both-e v1) (both-e v2)) (set-union (both-fvs v1) (both-fvs v2))))]
                   [(fst? e) (both (fst (both-e (f (fst-e e)))) (both-fvs (f (fst-e e))))]
                   [(snd? e) (both (snd (both-e (f (snd-e e)))) (both-fvs (f (snd-e e))))]
                   [(aunit? e) (both e (set))]
                   [(isaunit? e) (both (isaunit (both-e (f (isaunit-e e)))) (both-fvs (f (isaunit-e e))))]
                   [#t (error (format "bad MUPL expression, cannot compute free vars: ~v" e))]))])
    (both-e (f e))))
           
;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([e1evaled (eval-under-env-c (ifgreater-e1 e) env)]
               [e2evaled (eval-under-env-c (ifgreater-e2 e) env)])
           (cond
             [(and (int? e1evaled) (int? e2evaled))
              (if (> (int-num e1evaled) (int-num e2evaled))
                  (eval-under-env-c (ifgreater-e3 e) env)
                  (eval-under-env-c (ifgreater-e4 e) env))]
              [#t (error "ifgreater e1 or e2 not MUPL ints")]))]
        [(fun? e) (eval-under-env-c (compute-free-vars e) env)]
        [(fun-challenge? e) (closure
                             (build-cloj-env-from-env env (fun-challenge-freevars e))
                             e)]
        [(call? e)
         (let ([clos (eval-under-env-c (call-funexp e) env)]
               [act (eval-under-env-c (call-actual e) env)])
           (cond
             [(not (closure? clos)) (error "call on a non-closure")]
             ;; if fun-nameopt if not false, add to env when calling fun, and actual when calling fun body
             [(fun-challenge-nameopt (closure-fun clos))
              (let ([env-plus-fun-and-actual-and-cloj-env
                     (append (closure-env clos) (cons (cons (fun-challenge-nameopt (closure-fun clos)) clos)
                                   (cons (cons (fun-challenge-formal (closure-fun clos)) act) env)))])
                (eval-under-env-c (fun-challenge-body (closure-fun clos)) env-plus-fun-and-actual-and-cloj-env))]
             ;; if fun-nameopt is false, add just actual to env when calling fun body
             [#t (let ([env-plus-actual-and-cloj-env
                        (append (closure-env clos) (cons (cons (fun-challenge-formal (closure-fun clos)) act) env))])
                   (eval-under-env-c (fun-challenge-body (closure-fun clos)) env-plus-actual-and-cloj-env))]))]
        [(mlet? e)
         (let ([evaledexpression (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e) evaledexpression) env)))]
        [(apair? e) (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([thepair (eval-under-env-c (fst-e e) env)])
             (cond
               [(apair? thepair) (apair-e1 thepair)]
               [#t (error "gave non-pair to fst")]))]
        [(snd? e)
         (let ([thepair (eval-under-env-c (snd-e e) env)])
           (cond
             [(apair? thepair) (apair-e2 thepair)]
             [#t (error "gave non-pair to snd")]))]
        [(aunit? e) e]
        [(isaunit? e) (cond
                        [(equal? (eval-under-env-c (isaunit-e e) env) (aunit)) (int 1)]
                        [#t (int 0)])]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (build-cloj-env-from-env bigenv funset)
  (cond
    [(null? bigenv) null] 
    [(set-member? funset (car (car bigenv))) (cons (car bigenv) (build-cloj-env-from-env (cdr bigenv) funset))]
    [#t (build-cloj-env-from-env (cdr bigenv) funset)]))
         

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
