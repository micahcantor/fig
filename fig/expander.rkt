#lang racket/base

(require "merge.rkt")
(require racket/contract)

(define environment (make-hash))

(define-syntax-rule (fig-mb (fig-program STMT ...))
  (#%module-begin
   (require json)
   (define env/c (hash/c string? any/c))
   (define/contract (fig->hash [env (hash)])
     (->* () (env/c) (hash/c symbol? any/c))
     (for ([(key value) env])
       (hash-set! environment key value))
     STMT ...)
   (define/contract (fig->json [env (hash)])
     (->* () (env/c) string?)
     (jsexpr->string (fig->hash env)))
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

(define-syntax-rule (fig-apply FN ...)
  (FN ...))

(define-syntax-rule (fig-equal LEFT RIGHT)
  (equal? LEFT RIGHT))

(define-syntax-rule (fig-env-ref ENVREF)
  (hash-ref environment ENVREF))

(define-syntax-rule (fig-cond CONDITION CONSEQUENT ALTERNATE)
  (if CONDITION CONSEQUENT ALTERNATE))

(provide (rename-out [fig-mb #%module-begin])
         fig-let
         fig-object
         fig-list
         fig-merge
         fig-apply
         fig-equal
         fig-env-ref
         fig-cond
         fig-kvpair
         #%datum
         #%top
         #%top-interaction)