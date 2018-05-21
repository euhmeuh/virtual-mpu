#lang reader "asm-reader.rkt"
color = 41
screen = $ABCD
start sta a color+1,x
      tst a
      beq start
      bra done
out   ldx #$8080
done  rts
