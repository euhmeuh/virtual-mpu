#!/usr/bin/env racket
#lang racket/base

(require
  racket/cmdline
  racket/function
  racket/string
  virtual-mpu
  command-tree)

(define (assemble-to-binary mpu assembly)
  (display (assemble assembly)))

(define (assemble-to-hex mpu assembly)
  (displayln
    (string-join
      (map (curry format-hex)
           (bytes->list (assemble assembly))))))

(define (assemble-to-s-record mpu assembly [header #f])
  (bytes->s-record (assemble assembly)
                   #:header header))

(define (emulate-machine machine kernel)
  (emulate machine kernel))

(define (test-mpu mpu)
  (local-require rackunit/text-ui)
  (run-tests (dynamic-require (format "mpus/~a.test" mpu) 'suite)))

(command-tree
  `([assemble (to-binary ,assemble-to-binary)
              (to-hex ,assemble-to-hex)
              (to-s-record ,assemble-to-s-record)]
    [emulate ,emulate-machine]
    [test ,test-mpu])
  (current-command-line-arguments))
