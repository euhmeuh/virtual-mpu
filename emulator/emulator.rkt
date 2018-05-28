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

(define current-address-decoder (make-parameter #f))

(define (emulate kernel-filepath addr-decoder)
  (current-address-decoder addr-decoder)
  (call-with-input-file kernel-filepath init-memory)
  (emulation-loop))

(define (init-memory in)
  (for ([byte (in-port read-byte in)]
        [i (in-naturals)])
    (memory-write! i (bytes byte))))

(define (emulation-loop)
  (let loop ([result #f])
    (define-values (mnemonic mode next-bytes) (fetch))
    (if (eq? mnemonic 'wai)
        'end-of-emulation
        (loop (execute mnemonic mode next-bytes)))))

(define (fetch)
  ;; we read 3 bytes in advance in case we need operands
  (define bytes (memory-read pc 3))
  (define-values (mnemonic mode)
                 (get-mnemonic (bytes-ref bytes 0)))
  (advance-pc! mnemonic mode)
  (values mnemonic mode (subbytes bytes 1)))

(define (advance-pc! mnemonic mode)
  (set! pc (+ 1 pc (get-value-size mnemonic mode))))

(define (execute mnemonic mode next-bytes)
  (define operand (parse-operand mnemonic mode next-bytes))
  (display (format "~a (~a)" mnemonic mode))
  (when operand
    (display (format " - ~a" (format-hex operand))))
  (newline))

(define (parse-operand mnemonic mode bytes)
  (define value-size (get-value-size mnemonic mode))
  (define first-byte (and (> (bytes-length bytes) 0)
                          (bytes-ref bytes 0)))
  (define second-byte (and (> (bytes-length bytes) 1)
                           (bytes-ref bytes 1)))
  (cond
    [(= value-size 2)
     (+ (arithmetic-shift first-byte 8)
        second-byte)]
    [(and (= value-size 1)
          (eq? mode 'rel))
     (7bit-signed->number first-byte)]
    [(= value-size 1) first-byte]
    [else #f]))

(define (memory-read addr [size 1])
  (define device ((current-address-decoder) addr))
  (define len (bytes-length device))
  (let loop ([result (make-bytes 0)]
             [size size])
    (if (> size 0)
        (loop (if (> (+ addr size) len)
                  result
                  (bytes-append (bytes (bytes-ref device (+ size addr -1)))
                                result))
              (- size 1))
        result)))

(define (memory-write! addr bytes)
  (bytes-copy! ((current-address-decoder) addr) addr bytes))
