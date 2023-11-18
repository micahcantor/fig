#lang racket/base

(require "utils.rkt")

(define-syntax-rule (silver-mb AST)
  (#%module-begin AST))

(define-syntax-rule (silver-program EXPR ...)
  (for-each displayln (list EXPR ...)))

(define-syntax-rule (silver-expr EXPR) EXPR)

(define-syntax-rule (silver-object PAIR-OR-BRACE ...)
  (let ([pairs (filter identity (list PAIR-OR-BRACE ...))])
    (assoc-list->hash pairs)))

(define-syntax-rule (silver-list EXPR-OR-COMMA ...)
  (let ([exprs (filter identity (list EXPR-OR-COMMA ...))])
    exprs))

(define-syntax-rule (silver-kvpair KEY _ VALUE)
  (cons KEY VALUE))

(define-syntax silver-bool
  (syntax-rules ()
    [(silver-bool TRUE) #t]
    [(silver-bool FALSE) #f]))

(provide (rename-out [silver-mb #%module-begin])
         silver-program
         silver-expr
         silver-object
         silver-list
         silver-kvpair
         silver-bool
         #%app
         #%datum
         #%top
         #%top-interaction)