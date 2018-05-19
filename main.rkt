#lang racket/base

(require
  racket/cmdline
  "assembler.rkt")

(define-values
  (filepath)
  (command-line
    #:args (file) file))

(assemble filepath)
