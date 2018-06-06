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
     (apply charterm-cursor (map add1 (area-top-left area)))
     (charterm-display (or (label-text displayable) "")))])

(define (label #:name [name #f] #:show? [show? #t] text)
  (make-label name show? text))
