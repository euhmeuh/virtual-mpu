#lang racket/base

(provide
  (except-out (all-from-out racket/base) #%module-begin)
  (rename-out [module-begin #%module-begin])
  mpu
  operations
  ->
  branch
  push!
  pull!
  high
  low
  ref
  reverse-args
  arithmetic-shift-right
  (all-from-out racket/bool))

(require
  racket/class
  racket/function
  racket/bool
  racket/stxparam
  (for-syntax
    racket/base
    racket/class
    racket/list
    racket/syntax
    syntax/parse
    syntax/stx)
  (only-in "emulator.rkt"
    memory-read
    memory-write!))

(struct status-info (register bits) #:transparent)
(struct reg-info (name size) #:transparent)
(struct op-info (name desc proc) #:transparent)

(define-syntax-rule (module-begin expr)
  (#%module-begin
    (provide mpu%)
    (define mpu% expr)))

(begin-for-syntax
  (define-syntax-class register
    (pattern (name size))
    (pattern name #:with size #'8)))

(define-syntax-parameter ->
  (lambda (stx)
    (raise-syntax-error '-> "can only be used inside of mpu definition" stx)))

(define-syntax-parameter branch
  (lambda (stx)
    (raise-syntax-error 'branch "can only be used inside of mpu definition" stx)))

(define-syntax-parameter push!
  (lambda (stx)
    (raise-syntax-error 'push! "can only be used inside of mpu definition" stx)))

(define-syntax-parameter pull!
  (lambda (stx)
    (raise-syntax-error 'pull! "can only be used inside of mpu definition" stx)))

(define-syntax (mpu stx)
  (syntax-parse stx
    #:datum-literals (registers status interrupts)
    [(_ name (registers (r:register ...))
             (status status-reg (bit ...))
             (interrupts [int-name int-value] ...)
             ops)
     #:with bits #'(bit ...)
     #:with ((bit? pos) ...) (stx-map (lambda (id)
                                        #`(#,(format-id id "~a?" id)
                                           #,(expt 2 (index-of (syntax->list #'bits) id))))
                                      #'bits)
     #'(class object%
         (super-new)
         (field [r.name 0] ...
                [bit (case-lambda [() (read-flag status-reg pos)]
                                  [(bool) (set! status-reg (if bool
                                                               (set-flag status-reg pos)
                                                               (clear-flag status-reg pos)))])] ...
                [bit? (thunk (= 0 (read-flag status-reg pos)))] ...
                [registers (make-hasheq `([r.name . ,(reg-info 'r.name r.size)] ...))]
                [status (status-info 'status-reg 'bits)]
                [interrupts (make-hasheq '([int-name . int-value] ...))]
                [operations
                 (syntax-parameterize
                   ([-> (lambda (stx)
                          (syntax-case stx () [(_ val dest) (if (member (syntax->datum #'dest)
                                                                        (syntax->datum #'(r.name ...)))
                                                                #'(set! dest val)
                                                                #'(memory-set! dest val))]))]
                    [branch (syntax-rules () [(_ pc condition rel)
                                              (begin (when condition
                                                       ((+ pc rel) . -> . pc)))])]
                    [push! (syntax-rules () [(_ reg value)
                                             (begin (value . -> . (ref reg))
                                                    ((- reg 1) . -> . reg))])]
                    [pull! (syntax-rules () [(_ reg dest)
                                             (begin ((+ reg 1) . -> . reg)
                                                    ((ref reg) . -> . dest))])])
                   ops)])

         (define/public (call op . operands)
           (define found-op (hash-ref operations op #f))
           (when found-op
             (displayln (apply format (op-info-desc found-op) operands))
             (apply (op-info-proc found-op) operands))))]))

(define-syntax (operations stx)
  (syntax-parse stx
    [(_ (name desc (arg ...) body ...) ...)
     #'(make-hasheq `([name . ,(op-info 'name
                                        'desc
                                        (lambda (arg ...) body ...))] ...))]))

(define ((reverse-args dual-arity-proc) a b)
  (dual-arity-proc b a))

(define (high reg)
  (bitwise-and (arithmetic-shift reg -8) #xFF))

(define (low reg)
  (bitwise-and reg #xFF))

(define (ref addr)
  (memory-read addr))

(define (memory-set! addr value)
  (memory-write! addr (bytes value)))

(define (arithmetic-shift-right value)
  (bitwise-ior (bitwise-and value #b10000000)
               (arithmetic-shift value -1)))

(define (read-flag value flag)
  (bitwise-and value flag))

(define (set-flag value flag)
  (bitwise-ior value flag))

(define (clear-flag value flag)
  (bitwise-and value (bitwise-not flag)))

(module+ test
  (require
    racket/base
    rackunit
    (only-in "emulator.rkt" current-address-decoder))

  (define mpu% (dynamic-require "../mpus/6802.mpu" 'mpu%))
  (define the-mpu (new mpu%))
  (define memory (make-bytes 32))
  (current-address-decoder (lambda (addr) memory))

  (test-case "Load data"
    (send the-mpu call 'ldaa #x08)
    (send the-mpu call 'ldab #x10)
    (send the-mpu call 'lds #x04)
    (send the-mpu call 'ldx #xA0)
    (check-equal? (get-field a the-mpu) #x08)
    (check-equal? (get-field b the-mpu) #x10)
    (check-equal? (get-field sp the-mpu) #x04)
    (check-equal? (get-field ix the-mpu) #xA0))

  (test-case "Store data"
    (set-field! a the-mpu #x0A)
    (set-field! b the-mpu #x0B)
    (set-field! sp the-mpu #x0C)
    (set-field! ix the-mpu #x0D)
    (send the-mpu call 'staa 0)
    (send the-mpu call 'stab 1)
    (send the-mpu call 'sts 2)
    (send the-mpu call 'stx 3)
    (check-equal? (subbytes memory 0 4)
                  (bytes #x0A #x0B #x0C #x0D)))
  )
