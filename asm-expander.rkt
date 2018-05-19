#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin tree)
  (#%module-begin
    (provide instructions)
    (define instructions 'tree)))

(struct instruction (tag mnemonic operands comment))
