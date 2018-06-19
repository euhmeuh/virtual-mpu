#lang racket/base

(provide
  (except-out (all-from-out racket/base) #%module-begin)
  (rename-out [module-begin #%module-begin])
  mpu
  ->
  ~>
  ref
  wait-for-interrupt
  ;; binary operations
  neg
  16neg
  high
  low
  high+low
  nib-high
  nib-low
  nib+nib
  16bit+
  16bit-
  8bit+
  8bit-
  4bit+
  shift-left
  arithmetic-shift-right
  logical-shift-right
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
    memory-write!)
  "../utils.rkt")

(struct reg-info (name size) #:transparent)
(struct status-info (register bits) #:transparent)
(struct interrupts-info (bit interrupts) #:transparent)

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
             (interrupts interrupt-bit [int-name int-value] ...)
             (operations (aliases a:alias ...) op:operation ...))
     #:with bits #'(bit ...)
     #:with ((bit? pos) ...) (stx-map (lambda (id)
                                        #`(#,(format-id id "~a?" id)
                                           #,(expt 2 (index-of (syntax->list #'bits) id))))
                                      #'bits)
     #'(class object%
         (super-new)
         (field [r.name 0] ...
                [int-name int-value] ...
                [registers (make-hasheq `([r.name . ,(reg-info 'r.name r.size)] ...))]
                [status (status-info 'status-reg 'bits)]
                [interrupts (interrupts-info 'interrupt-bit (make-hasheq '([int-name . int-value] ...)))]
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

         (define/public (reset)
           (0 . -> . r.name) ...)

         )]))

(define (ref addr)
  (car (bytes->list (memory-read addr))))

(define (wait-for-interrupt)
  (void))

(define (memory-set! addr value)
  (memory-write! addr (bytes value)))

(define (read-flag value flag)
  (bitwise-and value flag))

(define (set-flag value flag)
  (bitwise-ior value flag))

(define (clear-flag value flag)
  (bitwise-and value (bitwise-not flag)))

(module+ test
  (require
    racket/base
    racket/format
    rackunit
    rackunit/text-ui
    (only-in "emulator.rkt" current-address-decoder))

  (define mpu% (dynamic-require "../mpus/6802.mpu" 'mpu%))
  (define the-mpu (new mpu%))
  (define memory (make-bytes 32))
  (current-address-decoder (lambda (addr) memory))

  (define (reset)
    (bytes-fill! memory 0)
    (send the-mpu reset))

  (define (boolean-bits->number values)
    (for/fold ([bin-num #b0])
              ([value values] [i (in-naturals)])
      (if value
          (bitwise-ior bin-num (expt 2 i))
          bin-num)))

  (define-syntax-rule (check-field-equal? field value)
    (check-equal? (get-field field the-mpu) value))

  (define-syntax-rule (check-status? bit ...)
    (check-field-equal? sr
      (boolean-bits->number
        (for/list ([status-bit (status-info-bits (get-field status the-mpu))])
          (memq status-bit '(bit ...))))))

  (define-test-suite mpu-instructions

    (test-case "Load immediate data"
      (reset)
      (send the-mpu ldaa #x08)
      (send the-mpu ldab #x10)
      (send the-mpu lds #x04)
      (send the-mpu ldx #xA0)
      (check-field-equal? a #x08)
      (check-field-equal? b #x10)
      (check-field-equal? sp #x04)
      (check-field-equal? ix #xA0))

    (test-case "Load relative data"
      (reset)
      (bytes-copy! memory 0 (bytes #x02 #x03 #x05 #x08))
      (send the-mpu ldaa (ref 0))
      (send the-mpu ldab (ref 1))
      (send the-mpu lds (ref 2))
      (send the-mpu ldx (ref 3))
      (check-field-equal? a #x02)
      (check-field-equal? b #x03)
      (check-field-equal? sp #x05)
      (check-field-equal? ix #x08))

    (test-case "Store data"
      (reset)
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
      (reset)
      (send the-mpu bra 4)
      (check-field-equal? pc 4)

      (send the-mpu sign #f)
      (send the-mpu bmi -4)
      (check-field-equal? pc 4)
      (send the-mpu bpl -4)
      (check-field-equal? pc 0)

      (send the-mpu sign #t)
      (send the-mpu bpl 4)
      (check-field-equal? pc 0)
      (send the-mpu bmi 4)
      (check-field-equal? pc 4))

    (test-case "Stack"
      (reset)
      (set-field! sp the-mpu 31)
      (send the-mpu ldaa 42)
      (send the-mpu psha)
      (send the-mpu ldab 20)
      (send the-mpu pshb)
      (check-field-equal? sp 29)
      (check-equal? (subbytes memory 30)
                    (bytes 20 42))
      (send the-mpu pula)
      (send the-mpu pulb)
      (check-field-equal? a 20)
      (check-field-equal? b 42)
      (check-field-equal? sp 31)
      (send the-mpu des)
      (check-field-equal? sp 30)
      (send the-mpu ins)
      (check-field-equal? sp 31))

    (test-case "Simple additions"
      (reset)
      (send the-mpu ldaa #x28)
      (send the-mpu ldab #x14)
      (send the-mpu aba)
      (check-field-equal? a #x3C)
      (check-status?)

      (send the-mpu adca #xFF) ;; equivalent to -1
      (check-field-equal? a #x3B)
      (check-status? carry half)

      (send the-mpu adcb 1) ;; with carry should be +2
      (check-field-equal? b #x16)
      (check-status?)

      (send the-mpu carry #t) ;; force carry
      (send the-mpu adda 1) ;; should not be concerned with carry
      (check-field-equal? a #x3C)
      (check-status?)

      (send the-mpu carry #t) ;; force carry
      (send the-mpu addb 1) ;; should not be concerned with carry
      (check-field-equal? b #x17)
      (check-status?))

    (test-case "Overflow addition"
      (reset)
      (send the-mpu ldaa 127)
      (send the-mpu adda 127)
      (check-field-equal? a 254)
      (check-status? sign overflow half)))

  (run-tests mpu-instructions))
