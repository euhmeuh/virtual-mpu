#lang info

(define collection "virtual-mpu")

(define deps '("base"
               "brag"
               "br-parser-tools-lib"
               "anaphoric"
               "reprovide-lang"
               "command-tree"
               "rackunit-lib"
               "charterm"))

(define build-deps '())

(define racket-launcher-names '("virtual-mpu"))
(define racket-launcher-libraries '("cli"))

(define pkg-desc "Old Microprocessor Emulator and Assembler")
(define pkg-authors '(euhmeuh))
