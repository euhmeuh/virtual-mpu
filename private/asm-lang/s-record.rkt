#lang racket/base

(provide
  bytes->s-record)

(require
  racket/string
  virtual-mpu/utils)

(define (bytes->s-record bytes #:header [head-string #f])
  (when head-string
    (displayln (header-record (bytes->list (string->bytes/utf-8 head-string)))))
  (define parts (chunk (bytes->list bytes) 32))
  (define offset 0)
  (for ([part parts])
    (displayln (data-record part offset))
    (set! offset (+ offset (length part))))
  (displayln (count-record (length parts)))
  (displayln (termination-record)))

(define (header-record byte-list)
  (format-record "S0" byte-list 0))

(define (data-record byte-list offset)
  (format-record "S1" byte-list offset))

(define (count-record n)
  (format-record "S5" '() n))

(define (termination-record)
  (format-record "S9" '() 0))

(define (format-record type byte-list offset)
  (define count (+ 3 (length byte-list)))
  (string-append
    type
    (format-hex count)
    (format-hex offset #:min-width 4)
    (string-join (map format-hex byte-list) "")
    (format-hex (checksum count offset byte-list))))

(define (checksum count addr data)
  (bitwise-xor
    (bitwise-and (foldl + 0 (cons count (cons addr data)))
                 #xFF)
    #xFF))
