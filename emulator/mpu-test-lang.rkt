#lang racket/base

(provide
  (except-out (all-from-out racket/base) #%module-begin)
  (rename-out [module-begin #%module-begin])
  (all-from-out rackunit)
  (all-from-out racket/class)
  (all-from-out "../utils.rkt")
  (all-from-out "../emulator/mpu-structs.rkt")
  current-mpu
  current-address-decoder
  ref
  check-field-equal?
  check-status?)

(require
  racket/class
  rackunit
  "../emulator/mpu-structs.rkt"
  (only-in "../emulator/emulator.rkt" current-address-decoder)
  (only-in "../emulator/mpu-lang.rkt" ref)
  "../utils.rkt")

(define current-mpu (make-parameter #f))

(define-syntax module-begin
  (syntax-rules ()
    [(_ (load-mpu mpu-file) expr ...)
     (#%module-begin
       (define mpu% (dynamic-require mpu-file 'mpu%))
       (current-mpu (new mpu%))
       expr ...)]))

(define-syntax-rule (check-field-equal? field value)
  (check-equal? (get-field field (current-mpu)) value))

(define-syntax-rule (check-status? bit ...)
  (check-field-equal? sr
    (boolean-bits->number
      (for/list ([status-bit (status-info-bits (get-field status (current-mpu)))])
        (memq status-bit '(bit ...))))))
