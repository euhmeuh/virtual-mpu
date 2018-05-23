#lang racket/base

(require
  racket/cmdline
  racket/function
  racket/string
  "assembler.rkt"
  "utils.rkt")

(define-values
  (filepath)
  (command-line
    #:args (file) file))

(displayln
  (string-join
    (map (curry format-hex #:min-width 2)
         (bytes->list (assemble filepath)))))
