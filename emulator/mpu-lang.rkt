#lang racket/base

(provide
  (except-out (all-from-out racket/base) #%module-begin)
  (rename-out [module-begin #%module-begin])
  mpu
  ->
  ~>
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

(define-syntax-rule (module-begin expr)
  (#%module-begin
    (provide mpu%)
    (define mpu% expr)))

(begin-for-syntax
  (define-syntax-class register
    (pattern (name size))
    (pattern name #:with size #'8))

  (define-syntax-class operation
    (pattern (name desc (arg ...) body ...)
      #:with proc #'(lambda (arg ...) body ...)))

  (define-syntax-class alias
    (pattern (name (arg ...) body ...)
      #:with proc #'(lambda (arg ...) body ...))))

(define-syntax-rule (-> val dest) (set! dest val))
(define-syntax-rule (~> val dest) (memory-set! dest val))

(define-syntax (mpu stx)
  (syntax-parse stx
    #:datum-literals (registers status interrupts aliases)
    [(_ name (registers (r:register ...))
             (status status-reg (bit ...))
             (interrupts [int-name int-value] ...)
             (operations (aliases a:alias ...) op:operation ...))
     #:with bits #'(bit ...)
     #:with ((bit? pos) ...) (stx-map (lambda (id)
                                        #`(#,(format-id id "~a?" id)
                                           #,(expt 2 (index-of (syntax->list #'bits) id))))
                                      #'bits)
     #'(class object%
         (super-new)
         (field [r.name 0] ...
                [registers (make-hasheq `([r.name . ,(reg-info 'r.name r.size)] ...))]
                [status (status-info 'status-reg 'bits)]
                [interrupts (make-hasheq '([int-name . int-value] ...))]
                [operations (make-hasheq '([op.name . op.desc] ...))])

         (define/public bit
           (case-lambda [() (read-flag status-reg pos)]
                        [(bool) (set! status-reg (if bool
                                                     (set-flag status-reg pos)
                                                     (clear-flag status-reg pos)))])) ...

         (define/public (bit?)
           (not (= 0 (read-flag status-reg pos)))) ...

         (define/private a.name a.proc) ...

         (define/public (op.name . operands)
           (displayln (apply format op.desc operands))
           (apply op.proc operands)) ...

         )]))

(define ((reverse-args dual-arity-proc) a b)
  (dual-arity-proc b a))

(define (high reg)
  (bitwise-and (arithmetic-shift reg -8) #xFF))

(define (low reg)
  (bitwise-and reg #xFF))

(define (ref addr)
  (car (bytes->list (memory-read addr))))

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
    (bytes-fill! memory 0)
    (send the-mpu ldaa #x08)
    (send the-mpu ldab #x10)
    (send the-mpu lds #x04)
    (send the-mpu ldx #xA0)
    (check-equal? (get-field a the-mpu) #x08)
    (check-equal? (get-field b the-mpu) #x10)
    (check-equal? (get-field sp the-mpu) #x04)
    (check-equal? (get-field ix the-mpu) #xA0))

  (test-case "Store data"
    (bytes-fill! memory 0)
    (set-field! a the-mpu #x0A)
    (set-field! b the-mpu #x0B)
    (set-field! sp the-mpu #x0C)
    (set-field! ix the-mpu #x0D)
    (send the-mpu staa 0)
    (send the-mpu stab 1)
    (send the-mpu sts 2)
    (send the-mpu stx 3)
    (check-equal? (subbytes memory 0 4)
                  (bytes #x0A #x0B #x0C #x0D)))

  (test-case "Branch"
    (send the-mpu bra 4)
    (check-equal? (get-field pc the-mpu) 4)

    (send the-mpu sign #f)
    (send the-mpu bmi -4)
    (check-equal? (get-field pc the-mpu) 4)
    (send the-mpu bpl -4)
    (check-equal? (get-field pc the-mpu) 0)

    (send the-mpu sign #t)
    (send the-mpu bpl 4)
    (check-equal? (get-field pc the-mpu) 0)
    (send the-mpu bmi 4)
    (check-equal? (get-field pc the-mpu) 4))

  (test-case "Stack"
    (bytes-fill! memory 0)
    (set-field! sp the-mpu 31)
    (send the-mpu ldaa 42)
    (send the-mpu psha)
    (send the-mpu ldab 20)
    (send the-mpu pshb)
    (check-equal? (get-field sp the-mpu) 29)
    (check-equal? (subbytes memory 30)
                  (bytes 20 42)))
  )
