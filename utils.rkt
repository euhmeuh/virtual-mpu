#lang racket/base

(provide
  symbol-append
  format-hex)

(define (symbol-append sym-a sym-b)
  (string->symbol
    (string-append (symbol->string sym-a)
                   (symbol->string sym-b))))

(define (format-hex value)
  (local-require (only-in racket/format ~r))
  (~r #:base '(up 16)
      #:min-width 4
      #:pad-string "0"
      value))
