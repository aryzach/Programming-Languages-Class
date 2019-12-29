#lang racket
(provide (all-defined-out))

(define xs (list 1 2 3 4))
(define ys (list 1 2 (list 4 5 (list 2 3 (list 9)) (list 7 8)) 3 5))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
                   (+ (car xs) (sum1 (cdr xs)))
                   (+ (sum1 (car xs)) (sum1 (cdr xs))))))

(define (ok n) (cons n (lambda () (ok (+ n 1)))))

(define nats
  (letrec ([ok (lambda (n) (cons n (lambda () (ok (+ n 1)))))])
              (lambda () (ok 1))))

(define (cool num g)
  (letrec ([f (lambda (x) (cons x (lambda () (f (g x num)))))])
    (lambda () (f num))))

;(define (temp num fun)
 ; (lambda (num fun) (cons num (lambda () 

(define nnats (cool 1 +))
(define ntwo (cool 2 *))

(define ob (lambda () (cons 1 ob)))

(define fib3
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (fib3 (- x 1))
                                            (fib3 (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))


(define (p1 low high stride)
  (cond
    [(> low high) null]
    [#t (cons low (p1 (+ low stride) high stride))]))


(define (sap los sfx)
  (map (lambda (s) (string-append s sfx)) los))

(define (lnm xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

(define (sfns s n)
  (cond
    [(= n 0) null]
    [#t (cons (car (s)) (sfns (cdr (s)) (- n 1)))]))