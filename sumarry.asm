#lang reader "asm-reader.rkt"
array = $20
sum     db 0
sumarry ldx #array  ;X = array
        clra        ;A = 0
        clrb        ;B = 0
loop    adda 0,x    ;A += array[0]
        inx         ;next item
        tst  0,x    ;check item
        bne  loop   ;repeat if B!=0
        staa sum    ;sum = A

$20     .data 1 2 3
