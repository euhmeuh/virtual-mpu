#lang racket/base

(provide
  symbol-append
  format-hex
  chunk)

(require
  racket/list)

(define (symbol-append sym-a sym-b)
  (string->symbol
    (string-append (symbol->string sym-a)
                   (symbol->string sym-b))))

(define (format-hex value #:min-width [min-width 2])
  (local-require (only-in racket/format ~r))
  (~r #:base '(up 16)
      #:min-width min-width
      #:pad-string "0"
      value))

(define (chunk lst size)
  (cond
    [(and (pair? lst) (>= (length lst) size))
     (cons (take lst size)
           (chunk (drop lst size) size))]
    [(pair? lst) (list lst)]
    [else lst]))
