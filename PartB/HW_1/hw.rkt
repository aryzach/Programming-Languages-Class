
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

(define funny-number-stream
  (letrec ([ok (lambda (x) (cons (if (= (remainder x 5) 0) (* x -1) x) (lambda () (ok (+ x 1)))))])
    (lambda () (ok 1))))

(define dan-then-dog
  (letrec ([ok (lambda (d) (cons d (lambda () (ok (if (string=? "dan.jpg" d) "dog.jpg" "dan.jpg")))))])
    (lambda () (ok "dan.jpg"))))

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec ([ok (lambda (sxs sys)
              (cond
                [(null? sxs) (ok xs sys)]
                [(null? sys) (ok sxs ys)]
                [#t (cons (cons (car sxs) (car sys)) (lambda () (ok (cdr sxs) (cdr sys))))]))])
    (lambda () (ok xs ys))))

(define (vector-assoc v vec)
  (letrec ([sub (lambda (pos)
               (cond
                 [(>= pos (vector-length vec)) #f]
                 [(not (pair? (vector-ref vec pos))) (sub (+ pos 1))]
                 [(= (car (vector-ref vec pos)) v) (vector-ref vec pos)]
                 [#t (sub (+ pos 1))]))])
    (sub 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache-pos 0]
           [f (lambda (v)
                (cond
                  [(vector-assoc v cache) (vector-assoc v cache)]
                  [(assoc v xs) (begin
                                  (vector-set! cache cache-pos (assoc v xs))
                                  (print "cache set")
                                  (set! cache-pos (if (= cache-pos (- n 1))
                                                      0
                                                      (+ cache-pos 1)))
                                  (assoc v xs))]
                  [#t #f]))])
    f))

(define cr (cached-assoc (list (cons 1 2) (cons 3 4)) 3))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([num e1])
       (letrec ([f (lambda (ok)
                  (if (<= num (ok))
                      #t
                      (begin
                        (ok)
                        (f ok))))])
         (f (lambda () e2))))]))
               





              