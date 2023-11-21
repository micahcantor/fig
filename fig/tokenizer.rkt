#lang racket/base

(require brag/support)

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

(define (make-tokenizer port)
  (define (next-token)
    (define fig-lexer
      (lexer-srcloc
       [(from/to "//" "\n") (next-token)]
       [whitespace (next-token)]
       [simple-token (token lexeme)]
       ["true" (token "true" #t)]
       ["false" (token "false" #f)]
       ["null" (token "null" 'null)]
       [env-ref-token (token 'ENVREF (substring lexeme 1))]
       [number-token (token 'NUMBER (string->number lexeme))]
       [id-token (token 'ID (string->symbol lexeme))]
       [(from/to "\"" "\"")
        (let ([value (substring lexeme 1 (sub1 (string-length lexeme)))])
          (token 'STRING value))]
       [(eof) (void)]))
    (fig-lexer port))
  next-token)

(provide make-tokenizer)