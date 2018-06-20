#lang racket/base

(provide
  (except-out (all-from-out racket/base) #%module-begin)
  (rename-out [module-begin #%module-begin])
  mpu
  ->
  ~>
  with
  ref
  wait-for-interrupt
  ;; binary operations
  8neg
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
  "mpu-structs.rkt"
  "../utils.rkt")

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

(define-syntax-rule (with id value body ...)
  (let ([id value]) body ...))

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

(define (memory-set! addr value)
  (memory-write! addr (bytes value)))

(define (wait-for-interrupt)
  (void))
