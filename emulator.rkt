#lang racket/base

(provide
  emulate)

(define (emulate filepath)
  (call-with-input-file filepath
    (lambda (in)
      (write (read in)))))

