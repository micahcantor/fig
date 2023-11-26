#lang racket/base

(require brag/support)
(require racket/match)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define-lex-abbrev number-token
  (:or (:seq (:? digits) "." digits)
       digits))

(define-lex-abbrev id-token
  (:seq alphabetic (:* (:or alphabetic numeric "-"))))

(define-lex-abbrev simple-token
  (:or "(" ")" "{" "}" "[" "]" "," ":" "=" "&" "==" "if" "then" "else" "let"))

(define-lex-abbrev env-ref-token
  (:seq "@" id-token))

(define (lex-string-lit ip)
  ;; delegate lexing of strings to the default Racket lexer
  (define-values (line-start col-start pos-start) (port-next-location ip))
  (define str (read ip))
  (define-values (line-end col-end pos-end) (port-next-location ip))
  (make-position-token (token 'STRING str)
                       (make-position pos-start line-start col-start)
                       (make-position pos-end line-end col-end)))

(define (make-tokenizer port)
  (define (next-token)
    (define fig-lexer
      (lexer-srcloc
       [(from/to "//" "\n") (next-token)]
       [whitespace (next-token)]
       [simple-token (token lexeme)]
       ["true" (token "true" #t)]
       ["false" (token "false" #f)]
       ["null" (token "null" 'NULL)]
       [env-ref-token (token 'ENVREF (substring lexeme 1))]
       [number-token (token 'NUMBER (string->number lexeme))]
       [id-token (token 'ID (string->symbol lexeme))]
       [(eof) (void)]))
    (match (peek-bytes 1 0 port)
      [#"\"" (lex-string-lit port)]
      [_ (fig-lexer port)]))
  next-token)

(provide make-tokenizer)