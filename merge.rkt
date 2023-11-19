#lang racket/base

(define (primitive-merge left right key)
  (if (equal? left right)
      left
      (error
       (if key
           (format "merge failure in '~a': primitive operands are not equal." key)
           "merge failure: primitive operands are not equal"))))

(define (merge left right [key #f])
  (cond
    [(and (hash? left) (hash? right))
     (for/fold ([result left])
               ([(right-key right-value) right])
       (cond
         [(hash-has-key? left right-key)
          (define left-value (hash-ref left right-key))
          (define merged-value (merge left-value right-value right-key))
          (hash-set result right-key merged-value)]
         [else
          (hash-set result right-key right-value)]))]
    [(and (list? left) (list? right))
     (append left right)]
    [(and (number? left) (number? right))
     (primitive-merge left right key)]
    [(and (string? left) (string? right))
     (primitive-merge left right key)]
    [(and (boolean? left) (boolean? right))
     (primitive-merge left right key)]
    [else
     (error
      (if key
          (format "merge failure in '~a': operands are different types" key)
          "merge failure: operands are different types"))]))
          
(provide merge)