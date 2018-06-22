#lang racket/base

(provide
  mpu
  address-decoder)

(require
  virtual-mpu/utils)

(define mpu "6802")

;; RIL011W memory map:
;; [0x0000...0x0FFF][0x1000 0x1001][0x1002...0X1FFF][0x2000...0xFFF7]
;;        ROM             ACIA           Unused            RAM
;;
;; [0xFFF8 0xFFF9][0xFFFA 0xFFFB][0xFFFC 0xFFFD][0xFFFE 0xFFFF]
;;       IRQ          Soft IR          NMI          Restart

(define rom (make-bytes (* 8 1024)))
(define acia (make-bytes 2))
(define ram (make-bytes (* 56 1024)))

(define (address-decoder addr)
  (cond
    [(<= addr #x0FFF) rom]
    [(<= addr #x1FFF) acia]
    [(<= addr #xFFFF) ram]
    [else (error 'out-of-range
                 "The given address is out of range: ~a"
                 (format-hex addr))]))
