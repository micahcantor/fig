#lang racket/base

(require "merge.rkt")
(require (for-syntax racket/base)
         racket/contract
         racket/stxparam)

(define-syntax-parameter environment
  (λ (stx)
    (raise-syntax-error
     #f "use of the environment keyword outside of fig" stx)))

(define-syntax-rule (fig-mb (fig-program STMT ...))
  (#%module-begin
   (require json)
   (define env/c (hash/c string? any/c))
   (define/contract (fig->hash [env (hash)])
     (->* () (env/c) any/c)
     (syntax-parameterize ([environment (make-rename-transformer #'env)])
       STMT ...))
   (define/contract (fig->json [env (hash)])
     (->* () (env/c) string?)
     (define result (fig->hash env))
     ; Convert keys to symbols for jsexpr->string.
     (define result-symbol-keys
       (hash-map/copy (fig->hash env) (λ (key value) (values (string->symbol key) value))))
     (jsexpr->string result-symbol-keys))
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
  (cons KEY VALUE))

(define-syntax-rule (fig-merge LEFT RIGHT)
  (merge LEFT RIGHT))

(define-syntax-rule (fig-apply FN ...)
  (FN ...))

(define-syntax-rule (fig-equal LEFT RIGHT)
  (equal? LEFT RIGHT))

(define-syntax-rule (fig-env-ref ENVREF)
  (hash-ref environment
            ENVREF
            (λ () (raise-user-error 'input "Unknown input variable ~a" ENVREF))))

(define-syntax-rule (fig-cond CONDITION CONSEQUENT ALTERNATE)
  (if CONDITION CONSEQUENT ALTERNATE))

(define-syntax-rule (fig-null)
  'NULL)

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
         fig-null
         #%datum
         #%top
         #%top-interaction)