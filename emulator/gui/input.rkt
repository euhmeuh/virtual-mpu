#lang racket/base

(provide
  (struct-out _input)
  input)

(require
  racket/contract/base
  charterm
  "private/base.rkt")

(define input-mode/c (symbols 'str 'dec 'hex 'bin))

(struct input element (label mode length)
  #:name _input
  #:constructor-name make-input
  #:methods gen:displayable
  [(define (display area displayable)
     (charterm-inverse)
     (for ([y (in-range (area-y area)
                        (+ (area-y area) (area-h area)))])
       (display-line (list (area-x area) y)
                     (list (+ (area-x area) (area-w area) -1) y) "i"))
     (charterm-normal))])

(define (input #:name [name #f]
               #:show? [show? #t]
               #:label [label #f]
               #:mode [mode 'str]
               #:length [length 8])
  (make-input name show? label mode length))
