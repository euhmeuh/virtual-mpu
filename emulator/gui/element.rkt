#lang racket/base

(provide
  (struct-out element)
  (struct-out container)
  separator/c
  gen:displayable
  displayable?
  display
  gen:parent
  parent?
  get-children
  add-child!
  find-element
  resolve-sizes)

(require
  racket/generic
  racket/list
  racket/contract/base)

(define separator/c (symbols 'line 'double 'dash 'space))

(define-generics displayable
  (display area displayable)
  #:fallbacks
  [(define (display area displayable) (void))])

(define-generics parent
  (get-children parent)
  (add-child! parent child [pos])
  #:fallbacks
  [(define (get-children parent)
     (container-elements parent))
   (define (add-child! parent child [pos 0])
     (define children (container-elements parent))
     (set-container-elements! parent (append (take children pos)
                                             (list child)
                                             (drop children pos))))])

(struct element (name show?) #:methods gen:displayable [])
(struct container element (size padding spacing [elements #:mutable]) #:methods gen:parent [])

(define (find-element container name)
  (let loop [(elements (cons container (get-children container)))]
    (if (pair? elements)
        (let ([elt (car elements)])
          (if (eq? (element-name elt) name)
              elt
              (loop (append (if (container? elt)
                                (get-children elt)
                                '())
                            (cdr elements)))))
        #f)))

(define (resolve-sizes elements)
  (for/list ([element elements])
    (if (container? element)
      (let ([size (container-size element)])
        (if (eq? size 'auto)
            '(auto auto)
            size))
      '(3 3))))
