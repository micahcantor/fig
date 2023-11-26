#lang racket/base

(require rackunit)

(define (run-file filepath [env (hash)])
  (define fig->hash (dynamic-require filepath 'fig->hash))
  (fig->hash env))

(check-equal? (run-file "single.fig")
              42
              "Fig programs may be a single literal.")

(check-equal? (run-file "numbers.fig")
              (hash "int" 42
                    "float 1" 42.0
                    "float 2" 0.1)
              "Fig programs should parse numbers correctly.")

(check-equal? (run-file "strings.fig")
              (hash "empty" ""
                    "normal" "value")
              "Fig programs should parse strings correctly.")

(check-equal? (run-file "booleans.fig")
              (hash "true" #t
                    "false" #f)
              "Fig programs should parse booleans correctly.")

(check-equal? (run-file "null.fig")
              (hash "null" 'NULL)
              "Fig programs should parse null correctly.")

(check-equal? (run-file "single-list.fig")
              '(1 2 3)
              "Fig programs may be a single list.")

(check-equal? (run-file "object-list.fig")
              (list (hash "key" "value") (hash "key" "value"))
              "Fig programs may be a list of objects.")

(check-equal? (run-file "lists.fig")
              (hash "numbers" '(1 2 3)
                    "strings" '("hello" "goodbye")
                    "empty" '()
                    "singleton" '(1))
              "Fig programs should correctly evaluate lists.")

(check-equal? (run-file "conditionals.fig")
              (hash "truthy" "truthy"
                    "falsey" "falsey")
              "Fig programs should correctly evaluate conditionals.")

(check-equal? (run-file "equality.fig")
              (hash "numbers" #t
                    "strings" #f
                    "null" #t
                    "objects" #t
                    "objects not equal" #f
                    "lists" #t
                    "lists not equal" #f)
              "Fig programs should correctly evaluate equality.")

(check-equal? (run-file "top-level-merge.fig")
              (hash "key1" 1
                    "key2" 2)
              "Fig permits merge at the top level.")

(check-equal? (run-file "merge.fig")
              (hash "simple" (hash "key1" 1 "key2" 2)
                    "number conflict" (hash "key" 1)
                    "list conflict" (hash "key" '(1 2 3))
                    "recursive 1" (hash "key1" 1 "key2" (hash "key3" 3 "key4" 4)))
              "Fig should merge objects correctly.")

(define env (hash "number" 1
                  "string" "hello"
                  "boolean" #t
                  "list" '(1 2 3)
                  "object" (hash "key" "value")))

(check-equal? (run-file "input.fig" env)
              env
              "Fig should accept input correctly.")

(check-equal?
 (run-file "apply.fig"
           (hash "append" append
                 "add" +
                 "list" list))
 (hash "append result" '(1 2 3)
       "plus result" 4
       "list result" '(1 2 3))
 "Fig should apply Racket functions correctly.")
