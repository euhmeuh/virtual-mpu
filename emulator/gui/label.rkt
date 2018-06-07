#lang racket/base

(provide
  (except-out (struct-out label) label)
  (rename-out [make-label label]))

(require
  charterm
  "private/base.rkt")

(struct label element (text)
  #:methods gen:displayable
  [(define (display area displayable)
     (apply charterm-cursor (map add1 (area-top-left area)))
     (charterm-display (or (label-text displayable) "")))])

(define (make-label #:name [name #f] #:show? [show? #t] text)
  (label name show? text))
