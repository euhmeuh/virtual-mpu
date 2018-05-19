#lang racket/base

(provide (rename-out [asm-read read]
                     [asm-read-syntax read-syntax]))

(require
  racket/port
  syntax/strip-context
  brag/support
  "asm-parser.rkt")

(define (asm-read in)
  (syntax->datum
    (asm-read-syntax #f in)))

(define (asm-read-syntax src in)
  (with-syntax ([parse-tree (parse src (make-tokenizer in src))])
    (strip-context
      #'(module asm-file "asm-expander.rkt"
          parse-tree))))

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)
  (define (next-token)
    (define bf-lexer
      (lexer-srcloc
        [(eof) (return-without-srcloc eof)]
        [(from/stop-before ";" "\n") (token 'COMMENT (trim-ends ";" lexeme ""))]
        ["\n" (token 'NEWLINE)]
        [(:+ (:& (:~ "\n") whitespace)) (token 'SPACE)]
        [(:+ numeric) (token 'NUMBER lexeme)]
        [(:: "$"
             (:+ (:or numeric
                      (char-range "a" "f")
                      (char-range "A" "F")))) (token 'NUMBER lexeme)]
        [(:or "a" "b" "+" "-" "*" "/" "=" ".data" "#" ",x") lexeme]
        [(:>= 2 alphabetic) (token 'ID (string->symbol lexeme))]
        [(from/to "\"" "\"") (token 'STRING (trim-ends "\"" lexeme "\""))]
        [any-char (next-token)]))
    (bf-lexer port))
  next-token)
