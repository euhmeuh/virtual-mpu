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
     (charterm-inverse)
     (for ([y (in-range (area-y area)
                        (+ (area-y area) (area-h area)))])
       (display-line (list (area-x area) y)
                     (list (+ (area-x area) (area-w area) -1) y) "b"))
     (charterm-normal))])

(define (make-buffer #:name [name #f]
                     #:show? [show? #t]
                     #:title [title #f]
                     #:mode [mode 'full])
  (buffer name show? title mode))
