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
  current-variable-table
  get-variable-value)

(struct program (source-tree expressions) #:transparent)
(struct line (expression comment) #:transparent)
(struct instruction (tag mnemonic operands) #:transparent)
(struct register (value) #:transparent)
(struct variable (value immediate? indexed?) #:transparent)
(struct assignment (name value) #:transparent)
(struct data (tag values) #:transparent)

(define current-pc (make-parameter 0))

(define (assemble filepath [header #f])
  (define program (dynamic-require filepath 'program))
  (binary->s-record
    #:header header
    (for/list ([expr (filter values (program-expressions program))])
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

(define current-variable-table (make-parameter (make-hash)))
(define (get-variable-value var)
  (let ([val (variable-value var)])
    (if (symbol? val)
        (hash-ref (current-variable-table) val)
        val)))

(define (add-variable! name value)
  (hash-set! (current-variable-table) name value))

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
  (list #xDA #xDA))

(define (instruction->binary instr)
  (list #xAB #xCD #xEF))

(define (binary->s-record bytes #:header [head-string #f])
  bytes)
