#lang virtual-mpu/asm
$16 nop
; mul  ( y x -- product )
valx    db 0
valy    db 0
result  db 0
mul  pula
     staa valx
     pula
     staa valy   ; get args from stack
loop ldaa valy
     sbca #1     ; valy--
     bcs  add    ; if valy > 0 go to add
end  ldaa result
     psha
     rts
add  staa valy
     ldaa result
     adda valx
     staa result ; res += valx
     bra loop

;; start
$00   ldaa #6
      psha
      ldaa #7
      psha
      jsr mul
      wai
