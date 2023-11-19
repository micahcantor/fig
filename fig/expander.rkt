#lang racket/base

(require "merge.rkt")

(define-syntax-rule (fig-mb (fig-program STMT ...))
  (#%module-begin
   (require json)
   (define (fig->hash)
     STMT ...)
   (define (fig->json)
     (jsexpr->string (fig->hash)))
   (provide fig->hash fig->json)))

(define-syntax-rule (fig-let ID VALUE)
  (define ID VALUE))

(define-syntax-rule (fig-object PAIR ...)
  (let ([pairs (list PAIR ...)])
    (for/hash ([pair pairs])
      (values (car pair) (cdr pair)))))

(define-syntax-rule (fig-list EXPR ...)
  (list EXPR ...))

(define-syntax-rule (fig-kvpair KEY VALUE)
  (cons (string->symbol KEY) VALUE))

(define-syntax-rule (fig-merge LEFT RIGHT)
  (merge LEFT RIGHT))

(define-syntax-rule (fig-equal LEFT RIGHT)
  (equal? LEFT RIGHT))

(define-syntax-rule (fig-cond CONDITION CONSEQUENT ALTERNATE)
  (if CONDITION CONSEQUENT ALTERNATE))

(provide (rename-out [fig-mb #%module-begin])
         fig-let
         fig-object
         fig-list
         fig-merge
         fig-equal
         fig-cond
         fig-kvpair
         #%app
         #%datum
         #%top
         #%top-interaction)