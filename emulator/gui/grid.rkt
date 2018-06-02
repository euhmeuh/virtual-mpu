#lang racket/base

(provide
  (struct-out _grid)
  grid)

(require
  racket/generic
  racket/list
  "private/base.rkt")

(struct grid container (dimensions)
  #:name _grid
  #:constructor-name make-grid
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area displayable))
     (define padding (container-padding displayable))
     (define dimensions (grid-dimensions displayable))
     (define areas (for/list ([x-area (split-balanced-area area 'horizontal (first dimensions))])
                     (split-balanced-area x-area 'vertical (second dimensions))))
     (for ([kv (in-hash-pairs (container-elements displayable))])
       (define-values (x y) (apply values (car kv)))
       (define child (cdr kv))
       (define child-area (list-ref (list-ref areas x) y))
       (base-display (pad-area child-area
                               (if (container? child)
                                   padding
                                   (map + padding '(1 1 1 1))))
                     child)))]
   #:methods gen:parent
   [(define (get-children parent)
      (hash-values (container-elements parent)))])

(define (grid #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:dimensions [dimensions '(2 2)]
              elements-hash)
  (make-grid name show? size padding elements-hash dimensions))
