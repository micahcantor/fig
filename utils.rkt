#lang racket/base

(define (identity x) x)

(define (assoc-list->hash lst)
  (for/hash ([pair lst])
    (values (car pair) (cdr pair))))

(provide assoc-list->hash
         identity)