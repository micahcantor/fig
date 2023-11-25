#lang racket/base

(require racket/cmdline)

(define json-mode (make-parameter #f))

(define main
  (command-line
   #:program "main"
   
   #:once-each
   [("-j" "--json") "Output as JSON" (json-mode #t)]
   
   #:args (filename . args)

   (define env (apply hash args))
   (cond
    [(json-mode)
     (define fig->json (dynamic-require filename 'fig->json))
     (fig->json env)]
    [else
     (define fig->hash (dynamic-require filename 'fig->hash))
     (fig->hash env)])))

main