#lang racket/base

(provide
  (except-out (struct-out input) input)
  (rename-out [make-input input]))

(require
  racket/list
  racket/match
  racket/contract/base
  "area.rkt"
  "element.rkt"
  "display.rkt"
  "../../utils.rkt")

(define input-mode/c (symbols 'str 'dec 'hex 'bin))

(struct input element (label mode length [value #:mutable])
  #:methods gen:displayable
  [(define (display area displayable)
     (display-set-cursor (area-top-left area))
     (display-text (or (input-label displayable) ""))
     (when (input-value displayable)
       (define len (input-length displayable))
       (define value (format-input-value (input-mode displayable)
                                         (input-value displayable)
                                         len))
       (with-underline
         (display-set-cursor (match (area-top-right area)
                                  [(list x y)
                                   (list (+ 1 (- x (string-length value)))
                                         y)]))
         (display-text value))))])

(define (make-input #:name [name #f]
                    #:show? [show? #t]
                    #:label [label #f]
                    #:mode [mode 'str]
                    #:length [length 8]
                    [value #f])
  (input name show? label mode length value))

(define (format-input-value mode value len)
  (cond
    [(eq? mode 'str) (substring value 0 (min (string-length value) len))]
    [(eq? mode 'dec) (format-dec value #:min-width len)]
    [(eq? mode 'hex) (format-hex value #:min-width len)]
    [(eq? mode 'bin) (format-bin value #:min-width len)]
    [else ""]))
