#lang racket/base

(provide
  current-display-mode
  display-borders
  display-line
  display-area
  display-text
  display-clear
  display-set-cursor
  with-underline)

(require
  racket/match
  charterm
  "area.rkt")

(define current-display-mode (make-parameter 'normal))

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
        [(== start-pos) (display-text (or head-char char))]
        [(== end-pos) (display-text (or tail-char char))]
        [_ (display-text char)]))))

(define (display-area area [char " "])
  (for ([y (in-range (area-y area)
                     (+ (area-y area) (area-h area)))])
    (display-line (list (area-x area) y)
                  (list (+ (area-x area) (area-w area) -1) y) char)))

(define (display-text text)
  (display-reverse)
  (charterm-display text))

(define (display-clear)
  (charterm-clear-screen))

(define (display-set-cursor pos)
  (apply charterm-cursor (map add1 pos)))

(define (display-reverse)
  (if (eq? (current-display-mode) 'reverse)
      (charterm-display "\x1B[7m")
      (charterm-display "\x1B[27m")))

(define-syntax-rule (with-underline body ...)
  (begin
    (charterm-underline)
    body ...
    (charterm-normal)))
