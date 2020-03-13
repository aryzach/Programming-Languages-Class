#lang racket

(struct btree-leaf ())
(struct btree-node (value left right))

(define (longestPath tree)
  (letrec ([lp  (lambda (node todo max curmax)
                (cond
                  [(btree-leaf? node) (cond
                                        [(null? todo) max]
                                        [(> curmax max) (lp (car todo) (cdr todo) curmax 0)]
                                        [#t (lp (car todo) (cdr todo) max 0)])]
                  [(btree-node? node) (lp (btree-node-right node) (cons (btree-node-left node) todo) max (+ 1 curmax))]))])
    (lp tree null 0 0)))
          
(define (fold-tree f start tree)
  (letrec ([ft (lambda (ct curans todo)
                 (cond
                   [(btree-leaf? ct) (cond
                                       [(null? todo) curans]
                                       [#t (ft (car todo) curans (cdr todo))])]
                   [(btree-node? ct) (ft (btree-node-right ct) (f (btree-node-value ct) curans) (cons (btree-node-left ct) todo))]))])
    (ft tree start null)))

(define fold-tree-curried
  (letrec ([ft (lambda (f ct curans todo)
                 (cond
                   [(btree-leaf? ct) (cond
                                       [(null? todo) curans]
                                       [#t (ft f (car todo) curans (cdr todo))])]
                   [(btree-node? ct) (ft f (btree-node-right ct) (f (btree-node-value ct) curans) (cons (btree-node-left ct) todo))]))])
    (lambda (f) (lambda (start) (lambda (tree) (ft f tree start null))))))

(define (crazysum xs)
  (letrec ([doit (lambda (f xs res)
                   (cond
                     [(null? xs) res]
                     [(number? (car xs)) (doit f (cdr xs) (f res (car xs)))]
                     [#t (doit (car xs) (cdr xs) res)]))])
    (doit + xs 0)))

(define (flatten1 xs)
  (letrec ([fltn (lambda (curxs origxs)
                   (cond
                     [(null? curxs) (cond
                                      [(null? origxs) null]
                                      [#t (flatten origxs)])]
                     [(list? (car curxs)) (flatten (append (flatten (car curxs)) (flatten (cdr curxs))))]
                     [#t (cons (car curxs) (append (fltn (cdr curxs) (flatten origxs))))]))])
    (fltn xs null)))

(define (flatten xs)
  (cond
    [(null? xs) null]
    [(list? (car xs)) (append (flatten (car xs)) (flatten (cdr xs)))]
    [#t (cons (car xs) (flatten (cdr xs)))]))
   