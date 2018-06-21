#!/usr/bin/env racket
#lang racket/base

(require
  racket/cmdline
  racket/function
  racket/string
  virtual-mpu/assemble
  virtual-mpu/s-record
  virtual-mpu/op-table
  virtual-mpu/emulate
  virtual-mpu/utils
  command-tree)

(current-op-table (call-with-input-file "mpus/6802.tab"
                    (lambda (in) (read in))))

(define (assemble-to-binary filepath)
  (display (assemble filepath)))

(define (assemble-to-hex filepath)
  (displayln
    (string-join
      (map (curry format-hex)
           (bytes->list (assemble filepath))))))

(define (assemble-to-s-record filepath [header #f])
  (bytes->s-record (assemble filepath)
                   #:header header))

(command-tree
  `([assemble (to-binary ,assemble-to-binary)
              (to-hex ,assemble-to-hex)
              (to-s-record ,assemble-to-s-record)]
    [emulate  ,(lambda (kernel)
                 (emulate kernel
                          (dynamic-require "machines/ril011w.rkt"
                                           'ril011w-address-decode)))])
  (current-command-line-arguments))
