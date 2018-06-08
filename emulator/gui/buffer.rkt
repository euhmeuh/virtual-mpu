#lang racket/base

(provide
  (except-out (struct-out buffer) buffer)
  (rename-out [make-buffer buffer]))

(require
  racket/contract/base
  racket/match
  charterm
  "private/base.rkt")

(struct buffer element (title text-provider)
  #:methods gen:displayable
  [(define (display area displayable)
     (apply charterm-cursor (map + (area-top-left area) '(2 0)))
     (charterm-display (buffer-title displayable))
     (fit-text-in-area area ((buffer-text-provider displayable) area)))])

(define (make-buffer #:name [name #f]
                     #:show? [show? #t]
                     #:title [title #f]
                     text-provider)
  (buffer name show? title text-provider))

(define (fit-text-in-area area input)
  (match-define (list base-x base-y) (map add1 (area-top-left area)))
  (for ([line (in-lines input)]
        [y (in-naturals 0)]
        #:break (>= y (area-h area)))
    (charterm-cursor base-x (+ base-y y))
    (charterm-display (substring line 0 (min (string-length line)
                                             (area-w area))))))
