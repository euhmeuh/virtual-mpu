#lang racket/base

(require charterm)

#| Mockup 1: Main view
┌──────────────────╥─Main program──────────────────────────────────────────────┐
│ A    00  B    00 ║0000╎ lda a $80                                            │
│ IX 0000  PC 0004 ║0001╎ psha                                                 │
│ SP 0004          ║0002╎ lda a $00                                            │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║0003╎ psha                                                 │
│ H  I  N  Z  V  C ║0004┝╸jsr memcpy                                           │
│ 0  0  0  0  0  0 ║0005╎                                                      │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║0006╎                                                      │
│ Machine  RIL011W ║0007╎                                                      │
│ MPU         6802 ║0008╎                                                      │
│ ROM    0000-0FFF ║0009╎                                                      │
│ ACIA   1000-1001 ║000A╎                                                      │
│ RAM    2000-FFFF ║000B╎                                                      │
┝╸Stack       FFFF ║000C╎                                                      │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║000D╎                                                      │
│ Pause      space ║000E╎                                                      │
│ Step           s ║000F╎                                                      │
│ Interrupt      i ╟─Stack─────────────────────────────────────────────────────┤
│ Exit          ^D ║ 00 01 02 03 04 00 00 00 00 00 00 00 00 00 00 00 00 00 00  │
└──────────────────╨──────────────┸────────────────────────────────────────────┘
|#

#| Mockup 2: With interrupt sideview
┌──────────────────╥─Main program────────────────┬─Interrupt───────────────────┐
│ A    00  B    00 ║0000╎ lda a $80              │00A4╎ ldab #1                │
│ IX 0000  PC 00A5 ║0001╎ psha                   │00A5┝╸sbc                    │
│ SP 0004          ║0002╎ lda a $00              │00A6╎ bnc                    │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║0003╎ psha                   │00A7╎                        │
│ H  I  N  Z  V  C ║0004┝╸jsr memcpy             │00A8╎                        │
│ 0  0  0  0  0  0 ║0005╎                        │00A9╎                        │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║0006╎                        │00AA╎                        │
│ Machine  RIL011W ║0007╎                        │00AB╎                        │
│ MPU         6802 ║0008╎                        │00AC╎                        │
│ ROM    0000-0FFF ║0009╎                        │00AD╎                        │
│ ACIA   1000-1001 ║000A╎                        │00AE╎                        │
│ RAM    2000-FFFF ║000B╎                        │00AF╎                        │
┝╸Stack       FFFF ║000C╎                        │00B0╎                        │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║000D╎                        │00B1╎                        │
│ Pause      space ║000E╎                        │00B2╎                        │
│ Step           s ║000F╎                        │00B3╎                        │
│ Interrupt      i ╟─Stack─────────────────────────────────────────────────────┤
│ Exit          ^D ║ 00 01 02 03 04 00 00 00 00 00 00 00 00 00 00 00 00 00 00  │
└──────────────────╨──────────────┸────────────────────────────────────────────┘
|#

#| Mockup 3: With memory view
┌──────────────────╥─Main program──────────────────────────────────────────────┐
│ A    00  B    00 ║0000╎ lda a $80                                            │
│ IX 0000  PC 0004 ║0001╎ psha                                                 │
│ SP 0004          ║0002╎ lda a $00                                            │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║0003╎ psha                                                 │
│ H  I  N  Z  V  C ║0004┝╸jsr memcpy                                           │
│ 0  0  0  0  0  0 ║0005╎                                                      │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║0006╎                                                      │
│ Machine  RIL011W ║0007╎                                                      │
│ MPU         6802 ║0008╎                                                      │
┝╸ROM    0000-0FFF ╟─ROM───────────────────────────────────────────────────────┤
│ ACIA   1000-1001 ║0000╎ 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00      │
│ RAM    2000-FFFF ║0010╎ 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00      │
│ Stack       FFFF ║0020╎ 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00      │
│╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌║0030╎ 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00      │
│ Pause      space ║0040╎ 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00      │
│ Step           s ║0050╎ 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00      │
│ Interrupt      i ║0060╎ 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00      │
│ Exit          ^D ║0070╎ 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00      │
└──────────────────╨───────────────────────────────────────────────────────────┘
|#

(define (box-mode/c value) (memq value '(fit balanced)))
(define (input-mode/c value) (memq value '(str dec hex bin)))
(define (buffer-mode/c value) (memq value '(full stack)))
(define (separator/c value) (memq value '(line double dash space)))

(struct element (name show?))
(struct container element (size padding elements))
(struct box container (orientation mode separator))

(struct screen container () #:name _screen #:constructor-name make-screen)
(struct grid container (dimensions) #:name _grid #:constructor-name make-grid)
(struct label element (text) #:name _label #:constructor-name make-label)
(struct input element (label mode length) #:name _input #:constructor-name make-input)
(struct buffer element (title mode) #:name _buffer #:constructor-name make-buffer)

(define (screen #:name [name #f]
                #:show? [show? #t]
                #:size [size 'auto]
                #:padding [padding '(0 0 0 0)]
                . elements)
  (make-screen name show? size padding elements))

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

(define (grid #:name [name #f]
              #:show? [show? #t]
              #:size [size 'auto]
              #:padding [padding '(0 0 0 0)]
              #:dimensions [dimensions '(2 2)]
              elements-hash)
  (make-grid name show? size padding elements-hash dimensions))

(define (label #:name [name #f] #:show? [show? #t] text)
  (make-label name show? text))

(define (input #:name [name #f]
               #:show? [show? #t]
               #:label [label #f]
               #:mode [mode 'str]
               #:length [length 8])
  (make-input name show? label mode length))

(define (buffer #:name [name #f]
                #:show? [show? #t]
                #:title [title #f]
                #:mode [mode 'full])
  (make-buffer name show? title mode))

(define (start ui)
  (unless (screen? ui)
    ;; default screen
    (set! ui (screen #:size 'auto ui)))
  (with-charterm
   (charterm-clear-screen)
   (charterm-cursor 0 0)
   (charterm-display "Hello, you.")
   (charterm-display "Press a key...")
   (let loop ([key (charterm-read-key)])
     (charterm-cursor 1 1)
     (charterm-clear-line)
     (printf "You pressed: ~S\r\n" key)
     (unless (eq? key 'ctrl-d)
       (loop (charterm-read-key))))))

(define ui
  (screen #:size '(80 20)
    (hbox #:mode 'fit
          #:separator 'double
      (vbox #:name 'side-view
            #:separator 'dash
            #:padding '(0 0 1 1)
        (grid #:dimensions '(2 3)
              #:name 'registers
          #hash([(0 0) . (input #:name 'a-input #:label "A" #:mode 'hex #:length 2)]
                [(1 0) . (input #:name 'b-input #:label "B" #:mode 'hex #:length 2)]
                [(0 1) . (input #:name 'ix-input #:label "IX" #:mode 'hex #:length 4)]
                [(1 1) . (input #:name 'pc-input #:label "PC" #:mode 'hex #:length 4)]
                [(0 2) . (input #:name 'sp-input #:label "SP" #:mode 'hex #:length 4)]))
        (grid #:dimensions '(6 2)
              #:name 'status
          #hash([(0 0) . (label "H")]
                [(0 1) . (input #:name 'h-input #:mode 'bin #:length 1)]
                [(1 0) . (label "I")]
                [(1 1) . (input #:name 'i-input #:mode 'bin #:length 1)]
                [(2 0) . (label "N")]
                [(2 1) . (input #:name 'n-input #:mode 'bin #:length 1)]
                [(3 0) . (label "Z")]
                [(3 1) . (input #:name 'z-input #:mode 'bin #:length 1)]
                [(4 0) . (label "V")]
                [(4 1) . (input #:name 'v-input #:mode 'bin #:length 1)]
                [(5 0) . (label "C")]
                [(5 1) . (input #:name 'c-input #:mode 'bin #:length 1)]))
        (vbox #:name 'machine)
        (vbox #:name 'commands))
      (vbox #:name 'main-view
            #:mode 'balanced
        (hbox #:mode 'balanced
          (buffer #:name 'main-buffer
                  #:title "Main program")
          (buffer #:name 'interrupt-buffer
                  #:title "Interrupt"
                  #:show? #f))
        (buffer #:name 'memory-buffer
                #:title "Stack")))))

(start ui)
