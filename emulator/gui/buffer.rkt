#lang racket/base

(provide
  (except-out (struct-out buffer) buffer)
  (rename-out [make-buffer buffer]))

(require
  racket/contract/base
  racket/match
  "area.rkt"
  "element.rkt"
  "display.rkt")

(struct buffer element (title text-provider)
  #:methods gen:displayable
  [(define (display area displayable)
     (display-set-cursor (map + (area-top-left area) '(1 -1)))
     (display-text (buffer-title displayable))
     (fit-text-in-area area ((buffer-text-provider displayable) area)))])

(define (make-buffer #:name [name #f]
                     #:show? [show? #t]
                     #:title [title #f]
                     text-provider)
  (buffer name show? title text-provider))

(define (fit-text-in-area area input)
  (match-define (list base-x base-y) (area-top-left area))
  (for ([line (in-lines input)]
        [y (in-naturals 0)]
        #:break (>= y (area-h area)))
    (display-set-cursor (list base-x (+ base-y y)))
    (display-text (substring line 0 (min (string-length line)
                                         (area-w area))))))
