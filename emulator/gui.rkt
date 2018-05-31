#lang racket/base

(provide
  (struct-out element)
  (struct-out container)
  (struct-out box)
  (struct-out _screen)
  (struct-out _grid)
  (struct-out _label)
  (struct-out _input)
  (struct-out _buffer)
  gen:displayable
  displayable?
  display
  hbox
  vbox
  screen
  grid
  label
  input
  buffer)

(require
  racket/generic
  racket/list
  racket/function
  racket/match
  charterm)

(define (box-orientation/c value) (memq value '(horizontal vertical)))
(define (box-mode/c value) (memq value '(fit balanced)))
(define (input-mode/c value) (memq value '(str dec hex bin)))
(define (buffer-mode/c value) (memq value '(full stack)))
(define (separator/c value) (memq value '(line double dash space)))

(define-generics displayable
  (display area displayable)
  #:fallbacks
  [(define (display area displayable) (void))])

(struct element (name show?) #:methods gen:displayable [])
(struct container element (size padding elements))

(struct box container (orientation mode separator)
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area displayable))
     (define orientation (box-orientation displayable))
     (define children (container-elements displayable))
     (define-values
       (first-area second-area)
       (split-area orientation area))
     (if (eq? orientation 'horizontal)
         (display-line (list (area-w first-area) (area-y first-area))
                       (list (area-w first-area) (area-h first-area)) "|"  #:head "+" #:tail "+")
         (display-line (list (area-x first-area) (area-h first-area))
                       (list (area-w first-area) (area-h first-area)) "-"  #:head "+" #:tail "+"))
     (base-display first-area (first children))
     (base-display second-area (second children)))])

(define (hbox #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:mode [mode 'fit]
              #:separator [separator 'line]
              . elements)
  (box name show? size padding elements 'horizontal mode separator))

(define (vbox #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:mode [mode 'fit]
              #:separator [separator 'line]
              . elements)
  (box name show? size padding elements 'vertical mode separator))

(struct screen container ()
  #:name _screen
  #:constructor-name make-screen
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area displayable))
     (display-borders area)
     (for-each (curry base-display area) (container-elements displayable)))])

(define (screen #:name [name #f]
                #:show? [show? #t]
                #:size [size 'auto]
                #:padding [padding '(0 0 0 0)]
                . elements)
  (make-screen name show? size padding elements))

(struct grid container (dimensions)
  #:name _grid
  #:constructor-name make-grid
  #:methods gen:displayable
  [(define (display area displayable) (void))])

(define (grid #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:dimensions [dimensions '(2 2)]
              elements-hash)
  (make-grid name show? size padding elements-hash dimensions))

(struct label element (text) #:name _label #:constructor-name make-label)

(define (label #:name [name #f] #:show? [show? #t] text)
  (make-label name show? text))

(struct input element (label mode length) #:name _input #:constructor-name make-input)

(define (input #:name [name #f]
               #:show? [show? #t]
               #:label [label #f]
               #:mode [mode 'str]
               #:length [length 8])
  (make-input name show? label mode length))

(struct buffer element (title mode) #:name _buffer #:constructor-name make-buffer)

(define (buffer #:name [name #f]
                #:show? [show? #t]
                #:title [title #f]
                #:mode [mode 'full])
  (make-buffer name show? title mode))

(define (get-display-area area container)
  (define size (container-size container))
  (if (eq? 'auto size)
      area
      (list (area-x area)
            (area-y area)
            (min (area-w area)
                 (first size))
            (min (area-h area)
                 (second size)))))

(define (display-borders area)
  (define-values
    (base-x base-y end-x end-y)
    (apply values area))
  (display-line (list base-x base-y) (list end-x base-y) "-" #:head "+" #:tail "+")
  (display-line (list base-x base-y) (list base-x end-y) "|" #:head "+" #:tail "+")
  (display-line (list base-x end-y)  (list end-x end-y)  "-" #:head "+" #:tail "+")
  (display-line (list end-x base-y)  (list end-x end-y)  "|" #:head "+" #:tail "+"))

(define (display-line start-pos end-pos char
                      #:head [head-char #f]
                      #:tail [tail-char #f])
  (define-values
    (base-x base-y end-x end-y)
    (apply values (append start-pos end-pos)))
  (for ([x (in-range base-x (add1 end-x))])
    (for ([y (in-range base-y (add1 end-y))])
      (charterm-cursor x y)
      (match (list x y)
        [(list (== base-x) (== base-y)) (charterm-display (or head-char char))]
        [(list (== end-x) (== end-y)) (charterm-display (or tail-char char))]
        [_ (charterm-display char)]))))

(define (split-area orientation area)
  (define first-area
    (match area
      [(list x y w h) #:when (eq? orientation 'horizontal)
       (list x y (/ w 2) h)]
      [(list x y w h) #:when (eq? orientation 'vertical)
       (list x y w (/ h 2))]))
  (define second-area
    (match area
      [(list x y w h) #:when (eq? orientation 'horizontal)
       (list (+ 1 (/ w 2)) y w h)]
      [(list x y w h) #:when (eq? orientation 'vertical)
       (list x (+ 1 (/ h 2)) w h)]))
  (values first-area second-area))

(define (area-x lst) (first lst))
(define (area-y lst) (second lst))
(define (area-w lst) (third lst))
(define (area-h lst) (fourth lst))
