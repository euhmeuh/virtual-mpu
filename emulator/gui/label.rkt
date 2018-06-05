#lang racket/base

(provide
  (struct-out _label)
  label)

(require
  charterm
  "private/base.rkt")

(struct label element (text)
  #:name _label
  #:constructor-name make-label
  #:methods gen:displayable
  [(define (display area displayable)
     (charterm-inverse)
     (for ([y (in-range (area-y area)
                        (+ (area-y area) (area-h area)))])
       (display-line (list (area-x area) y)
                     (list (+ (area-x area) (area-w area) -1) y) " "))
     (apply charterm-cursor (map add1 (area-top-left area)))
     (charterm-display (or (label-text displayable) " "))
     (charterm-normal))])

(define (label #:name [name #f] #:show? [show? #t] text)
  (make-label name show? text))
