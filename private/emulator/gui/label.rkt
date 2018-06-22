#lang racket/base

(provide
  (except-out (struct-out label) label)
  (rename-out [make-label label]))

(require
  "area.rkt"
  "element.rkt"
  "display.rkt")

(struct label element (text)
  #:methods gen:displayable
  [(define (display area displayable)
     (display-set-cursor (area-top-left area))
     (display-text (or (label-text displayable) "")))])

(define (make-label #:name [name #f] #:show? [show? #t] text)
  (label name show? text))
