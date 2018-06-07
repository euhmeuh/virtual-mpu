#lang racket/base

(provide
  (except-out (struct-out grid) grid)
  (rename-out [make-grid grid]))

(require
  racket/match
  racket/generic
  racket/list
  "private/base.rkt")

(struct grid container (dimensions)
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area displayable))
     (define padding (container-padding displayable))
     (define dimensions (grid-dimensions displayable))
     (define areas (for/list ([x-area (split-balanced-area
                                        (pad-area area (map + padding '(1 1 1 1)))
                                        'horizontal
                                        (first dimensions)
                                        #:spacing (container-spacing displayable))])
                     (split-balanced-area x-area
                                          'vertical
                                          (second dimensions)
                                          #:spacing (container-spacing displayable))))
     (for ([(pos child) (in-hash (container-elements displayable))])
       (match-define (list x y) pos)
       (define child-area (list-ref (list-ref areas x) y))
       (base-display child-area child)))]
   #:methods gen:parent
   [(define (get-children parent)
      (hash-values (container-elements parent)))])

(define (make-grid #:name [name #f]
                   #:show? [show? #t]
                   #:size [size 'auto]
                   #:padding [padding '(0 0 0 0)]
                   #:spacing [spacing -1]
                   #:dimensions [dimensions '(2 2)]
                   elements-hash)
  (grid name show? size padding spacing elements-hash dimensions))
