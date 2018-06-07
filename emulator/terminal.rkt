#lang racket/base

(require
  charterm
  "gui/base.rkt")

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

(define (start ui)
  (unless (screen? ui)
    ;; default screen
    (set! ui (screen #:size 'auto ui)))
  (with-charterm
   (define-values (width height) (charterm-screen-size))
   (charterm-clear-screen)
   (charterm-cursor 1 1)
   (let loop ([key #f])
     (display (area 0 0 width height) ui)
     (unless (eq? key 'ctrl-d)
       (loop (charterm-read-key)))))
  (newline)
  (displayln "Goodbye!"))

(define ui
  (screen #:size '(80 24)
    (hbox #:mode 'fit
          #:separator 'double
      (vbox #:name 'side-view
            #:separator 'dash
            #:size '(20 auto)
            #:mode 'fit
        (grid #:name 'registers
              #:dimensions '(2 3)
              #:size '(auto 5)
              #:padding '(0 0 1 0)
              #:spacing 0
          (make-hash
            `([(0 0) . ,(input #:name 'a-input #:label "A" #:mode 'hex #:length 2)]
              [(1 0) . ,(input #:name 'b-input #:label "B" #:mode 'hex #:length 2)]
              [(0 1) . ,(input #:name 'ix-input #:label "IX" #:mode 'hex #:length 4)]
              [(1 1) . ,(input #:name 'pc-input #:label "PC" #:mode 'hex #:length 4)]
              [(0 2) . ,(input #:name 'sp-input #:label "SP" #:mode 'hex #:length 4)])))
        (grid #:name 'status
              #:dimensions '(6 2)
              #:size '(auto 4)
              #:spacing 0
          (make-hash
            `([(0 0) . ,(label " H ")]
              [(0 1) . ,(input #:name 'h-input #:mode 'bin #:length 1)]
              [(1 0) . ,(label " I ")]
              [(1 1) . ,(input #:name 'i-input #:mode 'bin #:length 1)]
              [(2 0) . ,(label " N ")]
              [(2 1) . ,(input #:name 'n-input #:mode 'bin #:length 1)]
              [(3 0) . ,(label " Z ")]
              [(3 1) . ,(input #:name 'z-input #:mode 'bin #:length 1)]
              [(4 0) . ,(label " V ")]
              [(4 1) . ,(input #:name 'v-input #:mode 'bin #:length 1)]
              [(5 0) . ,(label " C ")]
              [(5 1) . ,(input #:name 'c-input #:mode 'bin #:length 1)])))
        (vbox #:name 'machine
              #:spacing 0
              #:padding '(0 0 1 1))
        (vbox #:name 'commands
              #:spacing 0
              #:padding '(0 0 1 1)))
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

(set-input-value! (find-element ui 'a-input) 12)
(set-input-value! (find-element ui 'b-input) 42)
(set-input-value! (find-element ui 'ix-input) 1234)
(set-input-value! (find-element ui 'pc-input) #x8080)
(set-input-value! (find-element ui 'sp-input) #xABCD)
(set-input-value! (find-element ui 'h-input) 0)
(set-input-value! (find-element ui 'i-input) 1)
(set-input-value! (find-element ui 'n-input) 0)
(set-input-value! (find-element ui 'z-input) 1)
(set-input-value! (find-element ui 'v-input) 0)
(set-input-value! (find-element ui 'c-input) 1)
(let ([commands (find-element ui 'commands)])
  (add-child! commands (label "Pause      space") 0)
  (add-child! commands (label "Step           s") 1)
  (add-child! commands (label "Interrupt      i") 2)
  (add-child! commands (label "Exit          ^D") 3))

(start ui)
