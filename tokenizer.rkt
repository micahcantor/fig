#lang racket/base

(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define-lex-abbrev number-token
  (:or digits
       (:seq (:? digits) "." digits)
       (:seq digits ".")))

(define (make-tokenizer port)
  (define (next-token)
    (define silver-lexer
      (lexer-srcloc
       [(from/to "//" "\n") (next-token)]
       [whitespace (next-token)]
       ["{" (token "{")]
       ["}" (token "}")]
       ["[" (token "[")]
       ["]" (token "]")]
       ["," (token ",")]
       [":" (token ":")]
       ["true" (token "true" 'TRUE)]
       ["false" (token "false" 'FALSE)]
       ["null" (token "null" null)]
       [number-token (token 'NUMBER lexeme)]
       [(from/to "\"" "\"")
        (let ([value (substring lexeme 1 (sub1 (string-length lexeme)))])
          (token 'STRING value))]
       [(eof) (void)]))
    (silver-lexer port))
  next-token)

(provide make-tokenizer)