#lang racket/base

(require "web-service.fig")

#| Run this as `LOCAL=true racket main.rkt` |#

(define local? (equal? (getenv "LOCAL") "true"))

(displayln 
  (fig->json (hash "username" "cat"
                   "email" "cat@email.com"
                   "local" local?)))