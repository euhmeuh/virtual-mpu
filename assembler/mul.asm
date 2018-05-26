#lang reader "asm-reader.rkt"
valx    db 0
valy    db 0
result  db 0

; mul  ( y x -- product )
mul  pula
     staa valx
     pula
     staa valy   ; get args from stack
loop ldaa valy
     suba #1     ; valy--
     bcs  add    ; if valy > 0 go to add
end  ldaa result
     psha
     rts
add  staa valy
     ldaa result
     adda valx
     staa result ; res += valx
     bra loop
