#lang racket/base

(provide
  (struct-out _screen)
  screen)

(require
  racket/generic
  racket/function
  "private/base.rkt")

(struct screen container ()
  #:name _screen
  #:constructor-name make-screen
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area displayable))
     (display-borders area)
     (for-each (curry base-display area) (get-children displayable)))])

(define (screen #:name [name #f]
                #:show? [show? #t]
                #:size [size 'auto]
                #:padding [padding '(0 0 0 0)]
                . elements)
  (make-screen name show? size padding elements))
