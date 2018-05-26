#lang racket/base

(provide
  emulate)

(require
  "../op-table.rkt"
  "../utils.rkt")

(define a-register 0)
(define b-register 0)
(define index-register 0)
(define status-register 0)
(define pc 0)
(define sp 0)

(define irq-addr #xFFF8)
(define soft-interrupt-addr #xFFFA)
(define nmi-addr #xFFFC)
(define restart-addr #xFFFE)

(define current-mem-reader (make-parameter #f))
(define current-mem-writer (make-parameter #f))

(define (emulate kernel-filepath mem-reader mem-writer)
  (current-mem-reader mem-reader)
  (current-mem-writer mem-writer)
  (call-with-input-file kernel-filepath init-memory)
  (emulation-loop))

(define (init-memory in)
  (for ([byte (in-port read-byte in)]
        [i (in-naturals)])
    ((current-mem-writer) i (bytes byte))))

(define (emulation-loop)
  (let loop ([result #f])
    (define-values (mnemonic mode next-bytes) (fetch))
    (if (eq? mnemonic 'wai)
        'end-of-emulation
        (loop (execute mnemonic mode next-bytes)))))

(define (fetch)
  ;; we read 3 bytes in advance in case we need operands
  (define bytes ((current-mem-reader) pc 3))
  (define-values (mnemonic mode)
                 (get-mnemonic (bytes-ref bytes 0)))
  (advance-pc! mnemonic mode)
  (values mnemonic mode (subbytes bytes 1)))

(define (advance-pc! mnemonic mode)
  (set! pc (+ 1 pc (get-value-size mnemonic mode))))

(define (execute mnemonic mode next-bytes)
  (displayln
    (format "~a (~a) - ~a"
            mnemonic mode (map format-hex
                               (bytes->list next-bytes)))))
