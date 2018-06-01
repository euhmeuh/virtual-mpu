#lang racket/base

(provide
  (struct-out area)
  area-top-left
  area-top-right
  area-bottom-left
  area-bottom-right
  (struct-out element)
  (struct-out container)
  separator/c
  gen:displayable
  displayable?
  display
  gen:parent
  parent?
  get-children
  get-display-area
  pad-area
  split-balanced-area
  split-fit-area
  resolve-sizes
  display-line
  display-borders)

(require
  racket/generic
  racket/list
  racket/match
  racket/contract/base
  charterm)

(define separator/c (symbols 'line 'double 'dash 'space))

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

(define (min-size a b)
  (cond
    [(eq? a 'auto) b]
    [(eq? b 'auto) a]
    [else (min a b)]))

(define (get-display-area an-area container)
  (define size (container-size container))
  (if (eq? 'auto size)
      an-area
      (struct-copy area an-area
        [w (min-size (area-w an-area) (first size))]
        [h (min-size (area-h an-area) (second size))])))

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
