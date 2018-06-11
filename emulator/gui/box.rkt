#lang racket/base

(provide
  (except-out (struct-out box) box)
  hbox
  vbox)

(require
  racket/contract/base
  racket/generic
  "area.rkt"
  "element.rkt"
  "display.rkt")

(define box-orientation/c (symbols 'horizontal 'vertical))
(define box-mode/c (symbols 'fit 'balanced))

(struct box container (orientation mode separator)
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area (container-size displayable)))
     (define padding (container-padding displayable))
     (define spacing (container-spacing displayable))
     (define orientation (box-orientation displayable))
     (define separator (box-separator displayable))
     (define mode (box-mode displayable))
     (define children (get-children displayable))
     (unless (= (length children) 0)
       (define children-areas
         (if (eq? mode 'balanced)
             (split-balanced-area area orientation (length children) #:spacing spacing)
             (split-fit-area area orientation (resolve-sizes children) #:spacing spacing)))
       (for ([child children]
             [child-area children-areas])
         (unless (eq? separator 'space)
           (if (eq? orientation 'horizontal)
               (display-line (area-top-left child-area) (area-bottom-left child-area) "|"  #:head "+" #:tail "+")
               (display-line (area-top-left child-area) (area-top-right child-area) "-"  #:head "+" #:tail "+")))
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
