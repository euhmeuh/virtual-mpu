#lang racket/base

(provide
  assemble)

(define (assemble filepath)
  (define instructions (dynamic-require filepath 'instructions))
  (for-each
    (lambda (instr)
      (print instr)
      (newline))
    instructions))
