
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond
    [(> low high) null]
    [#t (cons low (sequence (+ low stride) high stride))]))


(define (string-append-map los sfx)
  (map (lambda (s) (string-append s sfx)) los))

(define (list-nth-mod xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (cond
    [(= n 0) null]
    [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define fns
  (letrec ([ok (lambda (x) (cons (if (= (remainder x 5) 0) (* x -1) x) (lambda () (ok (+ x 1)))))])
    (lambda () (ok 1))))