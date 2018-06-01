#lang racket/base

(provide
  (struct-out _buffer)
  buffer)

(require
  racket/contract/base
  charterm
  "private/base.rkt")

(define buffer-mode/c (symbols 'full 'stack))

(struct buffer element (title mode)
  #:name _buffer
  #:constructor-name make-buffer
  #:methods gen:displayable
  [(define (display area displayable)
     (charterm-inverse)
     (for ([y (in-range (area-y area)
                        (+ (area-y area) (area-h area)))])
       (display-line (list (area-x area) y)
                     (list (+ (area-x area) (area-w area) -1) y) "b"))
     (charterm-normal))])

(define (buffer #:name [name #f]
                #:show? [show? #t]
                #:title [title #f]
                #:mode [mode 'full])
  (make-buffer name show? title mode))
