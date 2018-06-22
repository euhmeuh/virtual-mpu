#lang racket/base

(provide
  assemble
  (struct-out program)
  (struct-out line)
  (struct-out instruction)
  (struct-out register)
  (struct-out variable)
  (struct-out assignment)
  (struct-out data)
  number
  modifier
  current-value-table
  resolve-value)

(require
  racket/string
  racket/list
  racket/match
  (only-in racket/contract/base >/c)
  anaphoric
  virtual-mpu/op-table
  virtual-mpu/utils)

(struct program (source-tree expressions) #:transparent)
(struct line (expression comment) #:transparent)
(struct instruction (tag mnemonic operands) #:transparent)
(struct register (value) #:transparent)
(struct variable (value immediate? indexed?) #:transparent)
(struct assignment (name value) #:transparent)
(struct data (tag values) #:transparent)

(define current-pc (make-parameter 0))
(define current-value-table (make-parameter (make-hash)))
(define current-generated-block-size (make-parameter 32))

(define (assemble filepath)
  (define program (dynamic-require filepath 'program))
  (build-bytestring
    (resolve-relative-branches
      (walk-expressions (program-expressions program)))))

(define (walk-expressions expressions)
  (filter values
    (for/list ([expr expressions])
      (cond
        [(assignment? expr)
         (add-variable! (assignment-name expr)
                        (assignment-value expr))
         #f]
        [(or (instruction? expr)
             (data? expr))
         (handle-tag! expr)
         (let ([binary (expression->binary expr)]
               [pc (current-pc)])
           (advance-pc! (length binary))
           (cons pc binary))]))))

(define (resolve-value val)
  (cond
    [(symbol? val) (hash-ref (current-value-table) val val)]
    [(procedure? val) (val)]
    [else val]))

(define (modifier func value amount)
  (lambda ()
    (func (resolve-value value)
          (resolve-value amount))))

(define (add-variable! name value)
  (hash-set! (current-value-table) name value))

(define (handle-tag! expr)
  (define tag (if (data? expr)
                  (data-tag expr)
                  (instruction-tag expr)))
  (cond
    [(symbol? tag) (add-variable! tag (current-pc))]
    [(number? tag) (current-pc tag)]))

(define (advance-pc! amount)
  (current-pc (+ (current-pc) amount)))

(define (expression->binary expr)
  (if (data? expr)
      (data->binary expr)
      (instruction->binary expr)))

(define (data->binary data)
  (for/fold ([result '()])
            ([value (data-values data)])
    (append result
            (cond
              [(string? value)
               (bytes->list (string->bytes/utf-8 value))]
              [else
               (split-into-bytes '() value)]))))

(define (instruction->binary instr)
  (define-values (mnemonic operands) (resolve-operands instr))
  (define var (try-get-variable operands))
  (define value (and var (resolve-value (variable-value var))))
  (when (and value
             (symbol? value)
             (not (relative-op? mnemonic)))
    (error 'variable-not-found "The given variable was not declared: ~a" value))
  (define mode
    (if var
        (cond
          [(relative-op? mnemonic) 'rel]
          [(variable-indexed? var) 'idx]
          [(variable-immediate? var) 'imm]
          [(> value #xFF) 'ext]
          [(extended-only-op? mnemonic) 'ext]
          [else 'dir])
        'inh))
  (define opcode (and (not (pseudo-op? mnemonic))
                      (get-op-code mnemonic mode)))
  (define final-values (and value (format-value value mnemonic mode)))
  (when final-values
    (awhen (findf (>/c #xFF) final-values)
      (error 'value-too-large
             "The given value is larger than a byte: ~a"
             (format-hex it))))
  (cond
    [(not opcode) final-values]
    [(not final-values) (list opcode)]
    [else (cons opcode final-values)]))

(define (resolve-operands instr)
  (define operands (instruction-operands instr))
  (define mnemonic (instruction-mnemonic instr))
  (when (and (pair? operands)
             (register? (car operands)))
    (set! mnemonic
          (symbol-append mnemonic
                         (register-value (car operands))))
    (set! operands (drop operands 1)))
  (values mnemonic operands))

(define (try-get-variable operands)
  (and (pair? operands)
       (variable? (car operands))
       (car operands)))

(define (pseudo-op? mnemonic)
  (memq mnemonic '(db dw)))

(define (format-value value mnemonic mode)
  (if (eq? mode 'rel)
      (list (cons 'relative value)) ; delay the value for second pass processing
      (split-into-bytes '() value (if (pseudo-op? mnemonic)
                                      (if (eq? mnemonic 'dw) 2 1)
                                      (get-value-size mnemonic mode)))))

(define (resolve-relative-branches pos&bytes)
  (for/list ([pos&byte pos&bytes])
    (define pos (car pos&byte))
    (define bytes (cdr pos&byte))
    (cons pos
          (map (match-lambda
                 [(cons 'relative value)
                  (number->7bit-signed
                     (- (resolve-value value) (+ pos 2)))]
                 [byte byte])
               bytes))))

(define (build-bytestring pos&bytes)
  (for/fold ([bytestring (make-bytes (current-generated-block-size))])
            ([pos&byte pos&bytes])
    (define pos (car pos&byte))
    (define bytes (cdr pos&byte))
    (for ([byte bytes] [i (in-naturals)])
      (when (<= (bytes-length bytestring) (+ pos i))
        (set! bytestring
              (bytes-append bytestring
                            (make-bytes (* (current-generated-block-size)
                                           (/ (+ pos i) (current-generated-block-size)))))))
      (bytes-set! bytestring (+ pos i) byte))
    bytestring))

(define (number str)
  (if (string-prefix? str "$")
      (string->number (substring str 1) 16)
      (string->number str)))
