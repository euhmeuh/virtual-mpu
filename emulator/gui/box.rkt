#lang racket/base

(provide
  (except-out (struct-out box) box)
  hbox
  vbox)

(require
  racket/contract/base
  racket/generic
  "private/base.rkt")

(define box-orientation/c (symbols 'horizontal 'vertical))
(define box-mode/c (symbols 'fit 'balanced))

(struct box container (orientation mode separator)
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area displayable))
     (define padding (container-padding displayable))
     (define orientation (box-orientation displayable))
     (define mode (box-mode displayable))
     (define children (get-children displayable))
     (unless (= (length children) 0)
       (define children-areas
         (if (eq? mode 'balanced)
             (split-balanced-area area orientation (length children))
             (split-fit-area area orientation (resolve-sizes children))))
       (for ([child children]
             [child-area children-areas])
         (if (eq? orientation 'horizontal)
             (display-line (area-top-right child-area) (area-bottom-right child-area) "|"  #:head "+" #:tail "+")
             (display-line (area-bottom-left child-area) (area-bottom-right child-area) "-"  #:head "+" #:tail "+"))
         (base-display
           (pad-area child-area
                     (if (container? child)
                         padding
                         (map + padding '(1 1 1 1))))
           child))))])

(define (hbox #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:spacing [spacing -1]
              #:mode [mode 'balanced]
              #:separator [separator 'line]
              . elements)
  (box name show? size padding spacing elements 'horizontal mode separator))

(define (vbox #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:spacing [spacing -1]
              #:mode [mode 'balanced]
              #:separator [separator 'line]
              . elements)
  (box name show? size padding spacing elements 'vertical mode separator))
