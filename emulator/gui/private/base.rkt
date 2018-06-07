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
  add-child!
  find-element
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
      '(1 1))))

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

(define (split-balanced-area base-area orientation n #:spacing [spacing -1])
  (define-values (new-w rem-w) (quotient/remainder (area-w base-area) n))
  (define-values (new-h rem-h) (quotient/remainder (area-h base-area) n))
  (for/list ([i n])
    (define pad (if (= (+ i 1) n) ;; last element?
                    (if (eq? orientation 'horizontal) rem-w rem-h) ;; take rem
                    (- spacing)))
    (match base-area
      [(area x y w h) #:when (eq? orientation 'horizontal)
       (area (+ x (* i new-w)) y (+ new-w pad) h)]
      [(area x y w h) #:when (eq? orientation 'vertical)
       (area x (+ y (* i new-h)) w (+ new-h pad))])))

(define (split-fit-area base-area orientation sizes #:spacing [spacing -1])
  (match-define (area base-x base-y base-w base-h) base-area)
  (define to-be-reduced '()) ;; indexes of auto sized elements
  ;; we start with a fake area that gives us the starting position,
  ;; we get rid of it at the end
  (for/fold ([areas (list (area base-x base-y 1 1))]
             #:result
             (let ([areas (drop (reverse areas) 1)])
               (reduce-to-fit
                 base-area
                 orientation
                 areas
                 to-be-reduced
                 (if (< spacing 0)
                     (* (- (length areas) 1) (abs spacing))
                     0))))
            ([size sizes]
             [i (in-naturals)])
    (match-define (list size-w size-h) size)
    (cons
      (if (eq? orientation 'horizontal)
        (area (+ (first (area-top-right (first areas))) 1 spacing)
              base-y
              (if (eq? size-w 'auto)
                  (begin
                    ;; we temporarily set auto elements to the biggest available size
                    ;; but we register them as "to be reduced later"
                    (set! to-be-reduced (cons i to-be-reduced))
                    base-w)
                  size-w)
              base-h)
        (area base-x
              (+ (second (area-bottom-left (first areas))) 1 spacing)
              base-w
              (if (eq? size-h 'auto)
                  (begin
                    (set! to-be-reduced (cons i to-be-reduced))
                    base-h)
                  size-h)))
      areas)))

(define (reduce-to-fit base-area orientation areas to-be-reduced total-overlap)
  (define-values (base get) (if (eq? orientation 'horizontal)
                                (values (area-w base-area) area-w)
                                (values (area-h base-area) area-h)))
  (define total (foldl (lambda (a b) (+ (get a) b)) 0 areas))
  (define overflow (- total base total-overlap))
  (define reduction (if (and (> overflow 0) (pair? to-be-reduced))
                        (quotient overflow (length to-be-reduced))
                        0))
  (define total-reduction 0)
  (for/list ([an-area areas]
             [i (in-naturals)])
    (define the-reduc (if (member i to-be-reduced) reduction 0))
    (define new-area
      (match an-area
        [(area x y w h) #:when (eq? orientation 'horizontal)
         (area (- x total-reduction) y (- w the-reduc) h)]
        [(area x y w h) #:when (eq? orientation 'vertical)
         (area x (- y total-reduction) w (- h the-reduc))]))
    (when (member i to-be-reduced)
      (set! total-reduction (+ total-reduction the-reduc)))
    new-area))

(define (pad-area an-area padding)
  (match-define (list t b l r) padding)
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
