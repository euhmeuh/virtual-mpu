#lang racket/base

(provide
  (except-out (struct-out input) input)
  (rename-out [make-input input]))

(require
  racket/list
  racket/match
  racket/contract/base
  charterm
  "private/base.rkt"
  "../../utils.rkt")

(define input-mode/c (symbols 'str 'dec 'hex 'bin))

(struct input element (label mode length [value #:mutable])
  #:methods gen:displayable
  [(define (display area displayable)
     (apply charterm-cursor (map add1 (area-top-left area)))
     (charterm-display (or (input-label displayable) ""))
     (when (input-value displayable)
       (define len (input-length displayable))
       (charterm-underline)
       (apply charterm-cursor (match (area-top-right area)
                                [(list x y) (list (+ 1 (- x len)) (+ 1 y))]))
       (charterm-display (format-input-value (input-mode displayable)
                                             (input-value displayable)
                                             len))
       (charterm-normal)))])

(define (make-input #:name [name #f]
                    #:show? [show? #t]
                    #:label [label #f]
                    #:mode [mode 'str]
                    #:length [length 8])
  (input name show? label mode length #f))

(define (format-input-value mode value len)
  (cond
    [(eq? mode 'str) (substring value 0 len)]
    [(eq? mode 'dec) (format-dec value #:min-width len)]
    [(eq? mode 'hex) (format-hex value #:min-width len)]
    [(eq? mode 'bin) (format-bin value #:min-width len)]
    [else ""]))
