#lang racket/base

(provide
  (struct-out area)
  area-top-left
  area-top-right
  area-bottom-left
  area-bottom-right
  get-display-area
  split-balanced-area
  split-fit-area
  pad-area)

(require
  racket/list
  racket/match)

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

(define (get-display-area an-area size)
  (if (eq? 'auto size)
      an-area
      (struct-copy area an-area
        [w (min-size (area-w an-area) (first size))]
        [h (min-size (area-h an-area) (second size))])))

(define (min-size a b)
  (cond
    [(eq? a 'auto) b]
    [(eq? b 'auto) a]
    [else (min a b)]))

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
  (for/fold ([areas (list (area base-x base-y 0 0))]
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
    (define pad (if (= i 0) 0 spacing)) ;; we don't apply spacing to the first element
    (cons
      (if (eq? orientation 'horizontal)
        (area (+ (first (area-top-right (first areas))) 1 pad)
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
              (+ (second (area-bottom-left (first areas))) 1 pad)
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
