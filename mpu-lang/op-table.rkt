#lang racket/base

(provide
  current-op-table
  op-exists?
  relative-op?
  16bit-opcode?
  extended-only-op?
  get-op-code
  get-mnemonic
  get-value-size)

(require
  racket/list
  racket/match)

; first list index is least significant byte
; second list index is most significant byte
; inh: inherent       rel: relative
; imm: immediate      dir: direct
; ext: extended       idx: indexed
(define current-op-table (make-parameter '([])))

(define (op-exists? mnemonic mode)
  (for/or ([line (current-op-table)])
    (findf (op-predicate mnemonic mode) line)))

(define (relative-op? mnemonic)
  (op-exists? mnemonic 'rel))

(define (16bit-opcode? mnemonic)
  (memq mnemonic '(lds ldx cpx)))

(define (extended-only-op? mnemonic)
  (and (not (op-exists? mnemonic 'dir))
       (op-exists? mnemonic 'ext)))

(define (get-value-size mnemonic mode)
  (cond
    [(eq? mode 'inh) 0]
    [(eq? mode 'dir) 1]
    [(eq? mode 'idx) 1]
    [(eq? mode 'rel) 1]
    [(eq? mode 'ext) 2]
    [(and (eq? mode 'imm)
          (16bit-opcode? mnemonic)) 2]
    [(eq? mode 'imm) 1]))

(define (get-op-code mnemonic mode)
  (define cell&line
    (for/or ([current-line (current-op-table)])
      (let ([found (findf (op-predicate mnemonic mode) current-line)])
        (and found (cons found current-line)))))
  (if cell&line
      (let* ([cell (car cell&line)]
             [line (cdr cell&line)]
             [lsb (index-of (current-op-table) line)]
             [msb (index-of line cell)])
        (+ (arithmetic-shift msb 4) lsb))
      (error 'opcode-not-found
             "The given operation was not found: ~a in ~a mode"
             mnemonic (format-mode mode))))

(define (get-mnemonic op-code)
  (define msb (arithmetic-shift op-code -4))
  (define lsb (bitwise-and op-code #x0F))
  (apply values (list-ref (list-ref (current-op-table) lsb) msb)))

(define (op-predicate mnemonic mode)
  (lambda (elt)
    (and (pair? elt)
         (eq? (car elt) mnemonic)
         (eq? (cadr elt) mode))))

(define (format-mode mode)
  (match mode
    ['inh "inherent"]
    ['rel "relative"]
    ['imm "immediate"]
    ['dir "direct"]
    ['ext "extended"]
    ['idx "indexed"]))
