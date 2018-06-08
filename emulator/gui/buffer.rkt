#lang racket/base

(provide
  (except-out (struct-out buffer) buffer)
  (rename-out [make-buffer buffer]))

(require
  racket/contract/base
  charterm
  "private/base.rkt")

(define buffer-mode/c (symbols 'full 'stack))

(struct buffer element (title mode)
  #:methods gen:displayable
  [(define (display area displayable)
     (display-area area "b"))])

(define (make-buffer #:name [name #f]
                     #:show? [show? #t]
                     #:title [title #f]
                     #:mode [mode 'full])
  (buffer name show? title mode))
