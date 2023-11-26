#lang racket/base

(require "tokenizer.rkt"
         "parser.rkt")

(define (read-syntax path port)
  (define ast (parse path (make-tokenizer port)))
  (define module-datum `(module fig-module fig/expander
                          ,ast))
  (datum->syntax #f module-datum))

(provide read-syntax)