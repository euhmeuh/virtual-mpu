#lang virtual-mpu/asm
; Hello world program in 6802 assembly
; by Jérôme Martin (euhmeuh)
; Copyright © 2018 All rights reserved
; This program is released under the terms of the GNU General Public License v3

scradd = $2000 ; address of the screen display
scrlen = 32    ; number of characters on screen


; memcpy  ( -- )
; copy a block of memory from one location to another
cnt         dw      $0000       ; number of bytes to copy
src         dw      $0000       ; address of source data block
dst         dw      $0000       ; address of target data block

memcpy      lda b   cnt+1       ; cnt.L -> B
            beq     check       ; if 0, check higher bytes

loop        ldx     src
            lda a   0,x         ; (src) -> A
            inx
            stx     src         ; src+1 -> src

            ldx     dst
            staa    0,x         ; A -> (dst)
            inx
            stx     dst         ; dst+1 -> dst

            decb
            bne     loop        ; until B=0

            stab    cnt+1       ; 0 -> cnt.L

check       tst     cnt+0
            beq     done        ; if cnt.H=0 then quit

            dec     cnt+0       ; cnt.H-1 -> cnt.H
            bra     loop
done        rts


; clear  ( -- )
; reset screen
clear       rts


; type  ( addr.L addr.H size.L size.H -- )
; display characters at the given address
; and of the given size to the screen
type        pula
            staa    cnt         ; size.H -> cnt.H
            pula
            staa    cnt+1       ; size.L -> cnt.L

            pula
            staa    src         ; addr.H -> src.H
            pula
            staa    src+1       ; addr.L -> src.L

            ldaa    scradd
            staa    dst         ; scradd.H -> dst.H
            ldaa    scradd+1
            staa    dst         ; scradd.L -> dst.L

            jsr     memcpy
            rts


start       jsr     clear
            ldaa    #$80
            ldab    #$00
            psha
            pshb
            ldaa    #$12
            ldab    #$00
            psha
            pshb
            jsr     type


$80         .data "Hello world!"
