#lang racket/base

(require
  racket/cmdline
  "assembler.rkt"
  "s-record.rkt")

(define-values
  (filepath)
  (command-line
    #:args (file) file))

(bytes->s-record (assemble filepath))
