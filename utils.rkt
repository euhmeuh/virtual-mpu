#lang racket/base

(provide
  symbol-append
  format-dec
  format-hex
  format-bin
  chunk
  split-into-bytes
  number->7bit-signed
  7bit-signed->number
  neg
  16neg
  high
  low
  nib-high
  nib-low
  high+low
  nib+nib
  16bit+
  16bit-
  8bit+
  8bit-
  4bit+
  shift-left
  arithmetic-shift-right
  logical-shift-right)

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

(define-syntax define/rollover
  (syntax-rules ()
    [(_ limit (name arg ...) body)
     (define (name arg ...) (remainder body limit))]
    [(_ limit (name . args) body)
     (define (name . args) (remainder body limit))]))

(define (number->7bit-signed value)
  (remainder (if (>= value 0) value (+ 256 value)) 256))

(define (7bit-signed->number value)
  (if (> value 127) (- value 256) value))

(define (neg val)
  (bitwise-xor #xFF (8bit+ val #xFF)))

(define (16neg val)
  (bitwise-xor #xFFFF (8bit+ val #xFFFF)))

(define (high val)
  (bitwise-and (arithmetic-shift val -8) #xFF))

(define (low val)
  (bitwise-and val #xFF))

(define (nib-high val)
  (bitwise-and (arithmetic-shift val -4) #x0F))

(define (nib-low val)
  (bitwise-and val #x0F))

(define (high+low high low)
  (bitwise-ior (arithmetic-shift high 8) low))

(define (nib+nib high low)
  (bitwise-ior (arithmetic-shift high 4) low))

(define/rollover 65536 (16bit+ . values) (apply + values))
(define/rollover 256 (8bit+ . values) (apply + values))
(define/rollover 16 (4bit+ . values) (apply + values))

(define (8bit- . values)
  (apply 8bit+ (cons (car values)
                     (map neg (cdr values)))))

(define (16bit- . values)
  (apply 16bit+ (cons (car values)
                      (map 16neg (cdr values)))))

(define/rollover 256 (shift-left value)
  (arithmetic-shift value 1))

(define (arithmetic-shift-right value)
  (bitwise-ior (bitwise-and value #b10000000)
               (arithmetic-shift value -1)))

(define (logical-shift-right value)
  (arithmetic-shift value -1))
