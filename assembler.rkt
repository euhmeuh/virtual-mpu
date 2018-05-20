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

(struct program (source-tree instructions) #:transparent)
(struct line (instruction comment) #:transparent)
(struct instruction (tag mnemonic operands) #:transparent)
(struct register (value) #:transparent)
(struct variable (value immediate? indexed?) #:transparent)
(struct assignment (name value) #:transparent)
(struct data (tag values) #:transparent)

(define (assemble filepath)
  (define program (dynamic-require filepath 'program))
  (for/list ([instr (filter values (program-instructions program))])
    instr))

(define current-variable-table (make-parameter (make-hash)))
(define (get-variable-value var)
  (let ([val (variable-value var)])
    (if (symbol? val)
        (hash-ref (current-variable-table) val 1)
        val)))
