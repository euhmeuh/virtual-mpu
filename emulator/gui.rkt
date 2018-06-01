#lang racket/base

(provide
  (struct-out area)
  area-top-left
  area-top-right
  area-top-left
  area-bottom-right
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

(define-generics parent
  (get-children parent)
  #:fallbacks
  [(define (get-children parent)
    (container-elements parent))])

(struct element (name show?) #:methods gen:displayable [])
(struct container element (size padding elements) #:methods gen:parent [])

(struct box container (orientation mode separator)
  #:methods gen:displayable
  [(define/generic base-display display)
   (define (display area displayable)
     (set! area (get-display-area area displayable))
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
         (if (container? child)
             (base-display child-area child)
             (base-display (pad-area child-area '(1 1 1 1)) child)))))])

(define (hbox #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:mode [mode 'balanced]
              #:separator [separator 'line]
              . elements)
  (box name show? size padding elements 'horizontal mode separator))

(define (vbox #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:mode [mode 'balanced]
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
     (for-each (curry base-display area) (get-children displayable)))])

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
  [(define/generic base-display display)
   (define (display area displayable)
     (define dimensions (grid-dimensions displayable))
     (define areas (for/list ([x-area (split-balanced-area area 'horizontal (first dimensions))])
                     (split-balanced-area x-area 'vertical (second dimensions))))
     (for ([kv (in-hash-pairs (container-elements displayable))])
       (define-values (x y) (apply values (car kv)))
       (define child (cdr kv))
       (define child-area (list-ref (list-ref areas x) y))
       (base-display (if (container? child)
                         child-area
                         (pad-area child-area '(1 1 1 1)))
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

(struct label element (text)
  #:name _label
  #:constructor-name make-label
  #:methods gen:displayable
  [(define (display area displayable)
     (charterm-inverse)
     (for ([y (in-range (area-y area)
                        (+ (area-y area) (area-h area)))])
       (display-line (list (area-x area) y)
                     (list (+ (area-x area) (area-w area) -1) y) "l"))
     (charterm-normal))])

(define (label #:name [name #f] #:show? [show? #t] text)
  (make-label name show? text))

(struct input element (label mode length)
  #:name _input
  #:constructor-name make-input
  #:methods gen:displayable
  [(define (display area displayable)
     (charterm-inverse)
     (for ([y (in-range (area-y area)
                        (+ (area-y area) (area-h area)))])
       (display-line (list (area-x area) y)
                     (list (+ (area-x area) (area-w area) -1) y) "i"))
     (charterm-normal))])

(define (input #:name [name #f]
               #:show? [show? #t]
               #:label [label #f]
               #:mode [mode 'str]
               #:length [length 8])
  (make-input name show? label mode length))

(struct buffer element (title mode)
  #:name _buffer
  #:constructor-name make-buffer
  #:methods gen:displayable
  [(define (display area displayable)
     (charterm-inverse)
     (for ([y (in-range (area-y area)
                        (+ (area-y area) (area-h area)))])
       (display-line (list (area-x area) y)
                     (list (+ (area-x area) (area-w area) -1) y) "b"))
     (charterm-normal))])

(define (buffer #:name [name #f]
                #:show? [show? #t]
                #:title [title #f]
                #:mode [mode 'full])
  (make-buffer name show? title mode))

(define (resolve-sizes elements)
  (for/list ([element elements] #:when (container? element))
    (define size (container-size element))
    (if (and (not (eq? size 'auto))
             (not (memq 'auto size)))
        size
        (fold-max-size (cons size (resolve-sizes (get-children element)))))))

(define (fold-max-size sizes)
  (for/fold ([result '(auto auto)])
            ([size sizes])
    (when (not (eq? size 'auto))
      (set! result (list (max-size (car result) (car size))
                         (max-size (cadr result) (cadr size)))))
    result))

(define (max-size a b)
  (cond
    [(eq? a 'auto) b]
    [(eq? b 'auto) a]
    [else (max a b)]))

(define (get-display-area an-area container)
  (define size (container-size container))
  (if (eq? 'auto size)
      an-area
      (let ([size-w (first size)]
            [size-h (second size)]
            [w (area-w an-area)]
            [h (area-h an-area)])
        (struct-copy area an-area
          [w (if (eq? size-w 'auto) w (min w size-w))]
          [h (if (eq? size-h 'auto) h (min h size-h))]))))

(define (display-borders an-area)
  (display-line (area-top-left an-area)    (area-top-right an-area)    "-" #:head "+" #:tail "+")
  (display-line (area-top-left an-area)    (area-bottom-left an-area)  "|" #:head "+" #:tail "+")
  (display-line (area-bottom-left an-area) (area-bottom-right an-area) "-" #:head "+" #:tail "+")
  (display-line (area-top-right an-area)   (area-bottom-right an-area) "|" #:head "+" #:tail "+"))

(define (display-line start-pos end-pos char
                      #:head [head-char #f]
                      #:tail [tail-char #f])
  (define-values
    (base-x base-y end-x end-y)
    (apply values (append start-pos end-pos)))
  (for ([x (in-range base-x (add1 end-x))])
    (for ([y (in-range base-y (add1 end-y))])
      (charterm-cursor (+ 1 x) (+ 1 y))
      (match (list x y)
        [(== start-pos) (charterm-display (or head-char char))]
        [(== end-pos) (charterm-display (or tail-char char))]
        [_ (charterm-display char)]))))

(define (split-balanced-area an-area orientation n)
  (define-values (new-w rem-w) (quotient/remainder (area-w an-area) n))
  (define-values (new-h rem-h) (quotient/remainder (area-h an-area) n))
  (for/list ([i n])
    (define pad (if (= (+ i 1) n) ;; last element?
                    (if (eq? orientation 'horizontal) rem-w rem-h) ;; take rem
                    1)) ;; keep room for the separator
    (match an-area
      [(area x y w h) #:when (eq? orientation 'horizontal)
       (area (+ x (* i new-w)) y (+ new-w pad) h)]
      [(area x y w h) #:when (eq? orientation 'vertical)
       (area x (+ y (* i new-h)) w (+ new-h pad))])))

(define (split-fit-area an-area orientation sizes)
  (match-define (area x y w h) an-area)
  (define last-pos (list x y))
  (for/list ([size sizes])
    (if (eq? orientation 'horizontal)
        (area x y w h) ; TODO
        (area x y w h))))

(define (pad-area an-area padding)
  (define-values (t b l r) (apply values padding))
  (match an-area
    [(area x y w h)
     (area (+ x l)
           (+ y t)
           (- w (+ l r))
           (- h (+ t b)))]))

(struct area (x y w h) #:transparent)

(define (area-top-left an-area)
  (list (area-x an-area) (area-y an-area)))

(define (area-top-right an-area)
  (list (+ (area-x an-area)
           (area-w an-area)
           -1)
        (area-y an-area)))

(define (area-bottom-left an-area)
  (list (area-x an-area)
        (+ (area-y an-area)
           (area-h an-area)
           -1)))

(define (area-bottom-right an-area)
  (list (+ (area-x an-area)
           (area-w an-area)
           -1)
        (+ (area-y an-area)
           (area-h an-area)
           -1)))
