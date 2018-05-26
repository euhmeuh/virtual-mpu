#lang racket/base

(provide
  emulate)

(require
  "../utils.rkt")

(define (emulate kernel-filepath)
  (call-with-input-file kernel-filepath
    (lambda (in)
      (for ([byte (in-port read-byte in)])
        (execute byte)))))

(define (execute byte)
  (display (format-hex byte)))
