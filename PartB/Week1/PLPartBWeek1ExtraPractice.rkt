#lang racket

(define (palindromic xs)
  (letrec ([rev (let ([rever (lambda (ys) (append (reverse (cdr ys)) (cons (car ys) null)))])
                  (rever xs))]
           [addlists (lambda (l1 l2)
                       (cond
                         [(null? (or l1 l2)) null]
                         [#t (cons (+ (car l1) (car l2)) (addlists (cdr l1) (cdr l2)))]))])
    (addlists xs rev)))


(define fibonacci
  (letrec ([prev null]
           [f (lambda (n)
                (let ([ans (assoc n prev)])
                  (if ans
                      (cdr ans)
                      (begin
                        (let ([now-ans (cond
                                         [(= n 0) 0]
                                         [(= n 1) 1]
                                         [#t (+ (f (- n 1)) (f (- n 2)))])]) 
                          (set! prev (cons (cons n now-ans) prev))
                          now-ans)))))])
    f))
           
(define (stream-until f s)
  (cond
    [(f (car (s))) (car (s))]
    [#t (stream-until f (cdr (s)))]))

(define ms
  (letrec ([ok (lambda (n) (cons n (lambda () (ok (+ n 1)))))])
    (lambda () (ok 1))))

(define (stream-map f s)
  (lambda () (cons (f (car (s))) (stream-map f (cdr (s))))))

(define (stream-zip s1 s2)
  (lambda () (cons (cons (car (s1)) (car (s2))) (stream-zip (cdr (s1)) (cdr (s2))))))

(define (interleave los)
  (letrec ([f (lambda (sublos other)
                (cond
                  [(null? sublos) (f other null)]
                  [#t (cons (car ((car sublos))) (lambda () (f (cdr sublos) (append other (cons (cdr ((car sublos))) null) ))))]))])
    (lambda () (f los null))))

(define ok (stream-map (lambda (x) (* x -1)) ms))
(define g (stream-zip ms ok))
(define cool (interleave (list ms ok g)))

(define (stream-for-n-steps s n)
  (cond
    [(= n 0) null]
    [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define (pack n s)
  (letrec ([f (lambda (cs)
             (cond
               [(number? (car (cs))) (f (lambda () (cons (cons (car (cs)) null) (cdr (cs)))))] 
               [(= (length (car (cs))) n) (cons (car (cs)) (pack n (cdr (cs))))]
               [#t (f (lambda () (cons (append (car (cs)) (cons (car ((cdr (cs)))) null)) (cdr ((cdr (cs)))))))]))])
    (lambda () (f s))))