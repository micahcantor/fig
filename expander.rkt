#lang racket/base

(require "merge.rkt")

(define-syntax-rule (silver-mb (silver-program STMT ...))
  (#%module-begin
   STMT ...))

(define-syntax-rule (silver-let ID VALUE)
  (define ID VALUE))

(define-syntax-rule (silver-object PAIR ...)
  (let ([pairs (list PAIR ...)])
    (for/hash ([pair pairs])
      (values (car pair) (cdr pair)))))

(define-syntax-rule (silver-list EXPR ...)
  (list EXPR ...))

(define-syntax-rule (silver-kvpair KEY VALUE)
  (cons KEY VALUE))

(define-syntax-rule (silver-merge LEFT RIGHT)
  (merge LEFT RIGHT))

(define-syntax-rule (silver-equal LEFT RIGHT)
  (equal? LEFT RIGHT))

(define-syntax-rule (silver-cond CONDITION CONSEQUENT ALTERNATE)
  (if CONDITION CONSEQUENT ALTERNATE))

(provide (rename-out [silver-mb #%module-begin])
         silver-let
         silver-object
         silver-list
         silver-merge
         silver-equal
         silver-cond
         silver-kvpair
         #%app
         #%datum
         #%top
         #%top-interaction)