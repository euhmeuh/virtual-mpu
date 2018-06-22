#lang info

(define collection "virtual-mpu")

(define deps '("base"
               "brag"
               "anaphoric"
               "reprovide-lang"))

(define build-deps '("rackunit"))

(define racket-launcher-names '("virtual-mpu"))
(define racket-launcher-libraries '("cli"))

(define pkg-desc "Old Microprocessor Emulator and Assembler")
(define pkg-authors '(euhmeuh))
