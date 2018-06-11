#lang racket/base

(provide
  (except-out (struct-out screen) screen)
  (rename-out [make-screen screen]))

(require
  racket/generic
  racket/function
  "area.rkt"
  "element.rkt"
  "display.rkt")

(struct screen container ()
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area (container-size displayable)))
     (display-area area)
     (display-borders area)
     (for-each (curry base-display area) (get-children displayable)))])

(define (make-screen #:name [name #f]
                     #:show? [show? #t]
                     #:size [size 'auto]
                     #:padding [padding '(0 0 0 0)]
                     . elements)
  (screen name show? size padding 0 elements))
