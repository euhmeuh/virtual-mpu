#lang racket/base

(provide
  symbol-append
  format-dec
  format-hex
  format-bin
  chunk
  split-into-bytes
  number->7bit-signed
  7bit-signed->number)

(require
  racket/list)

(define (symbol-append sym-a sym-b)
  (string->symbol
    (string-append (symbol->string sym-a)
                   (symbol->string sym-b))))

(define (format-dec value #:min-width [min-width 1])
  (format-num value 10 min-width))

(define (format-hex value #:min-width [min-width 2])
  (format-num value '(up 16) min-width))

(define (format-bin value #:min-width [min-width 8])
  (format-num value 2 min-width))

(define (format-num value base min-width)
  (local-require (only-in racket/format ~r))
  (~r #:base base
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

(define (split-into-bytes result value [size #f])
    (if (if size (<= size 1)
                 (<= value #xFF))
        (cons value result)
        (split-into-bytes (cons (bitwise-and value #xFF) result)
                          (arithmetic-shift value -8)
                          (and size (- size 1)))))

(define (number->7bit-signed value)
  (if (or (> value 127) (< value -128))
      (error 'relative-value "outside of range (-128, 127): ~a" value)
      (if (>= value 0) value (+ 256 value))))

(define (7bit-signed->number value)
  (bitwise-and #xFF (- value 1)))
