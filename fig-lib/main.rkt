#lang racket/base

(require racket/cmdline)
(require "file.rkt")

(define json-mode (make-parameter #f))

(define main
  (command-line
   #:program "main"
   
   #:once-each
   [("-j" "--json") "Output as JSON" (json-mode #t)]
   
   #:args (filename . args)

   (define env (apply hash args))
   (read-file filename (json-mode) env)))

main