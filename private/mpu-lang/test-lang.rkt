#lang racket/base

(provide
  (except-out (all-from-out racket/base) #%module-begin)
  (rename-out [module-begin #%module-begin])
  (all-from-out rackunit)
  (all-from-out racket/class)
  (all-from-out virtual-mpu/utils)
  (all-from-out "structs.rkt")
  current-mpu
  current-address-decoder
  ref
  check-register-equal?
  check-status?
  set-register!
  set-status!
  run)

(require
  racket/class
  rackunit
  (only-in virtual-mpu/private/emulator/emulator
           current-address-decoder)
  virtual-mpu/utils
  (only-in "lang.rkt" ref)
  "structs.rkt")

(define current-mpu (make-parameter #f))

(define-syntax module-begin
  (syntax-rules (load-mpu initialize before-each test-case)
    [(_ (load-mpu mpu-file)
        (initialize init ...)
        (before-each bef ...)
        (test-case name expr ...) ...)
     (#%module-begin
       (define mpu% (dynamic-require mpu-file 'mpu%))
       (current-mpu (new mpu%))
       init ...
       (define (before-thunk) bef ...)
       (define/provide-test-suite suite
         (test-case name
           (before-thunk)
           expr ...) ...))]))

(define-syntax-rule (check-register-equal? register value)
  (check-equal? (get-field register (current-mpu)) value))

(define-syntax-rule (check-status? bit ...)
  (check-register-equal? sr
    (boolean-bits->number
      (for/list ([status-bit (status-info-bits (get-field status (current-mpu)))])
        (memq status-bit '(bit ...))))))

(define-syntax-rule (set-register! register value)
  (set-field! register (current-mpu) value))

(define-syntax-rule (set-status! bit bool)
  (send (current-mpu) bit bool))

(define-syntax-rule (run instruction arg ...)
  (send (current-mpu) instruction arg ...))
