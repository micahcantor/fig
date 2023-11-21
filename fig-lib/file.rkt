#lang racket/base

(provide read-file)

(define (read-file filename [json-mode #f] [env (hash)])
  (cond
    [json-mode
     (define fig->json (dynamic-require filename 'fig->json))
     (fig->json env)]
    [else
     (define fig->hash (dynamic-require filename 'fig->hash))
     (fig->hash env)]))