#lang virtual-mpu/asm
color = 41
screen = $ABCD
start sta a color+1,x
      tst a
      beq start
      bra done
out   ldx #$8080
done  rts

      .data 12 "Hello world!" $0102 $03 $04 "abcd"
