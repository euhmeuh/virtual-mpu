#lang racket/base

(provide
  op-table
  op-exists?
  relative-op?
  16bit-opcode?
  extended-only-op?
  get-op-code
  get-mnemonic
  get-value-size)

(require
  racket/list
  racket/match)

; first list index is least significant byte
; second list index is most significant byte
; inh: inherent       rel: relative
; imm: immediate      dir: direct
; ext: extended       idx: indexed
(define op-table
  '([(        ) (sba  inh) (bra  rel) (tsx  inh)
     (nega inh) (negb inh) (neg  idx) (neg  ext)
     (suba imm) (suba dir) (suba idx) (suba ext)
     (subb imm) (subb dir) (subb idx) (subb ext)]
    [(nop  inh) (cba  inh) (        ) (ins  inh)
     (        ) (        ) (        ) (        )
     (cmpa imm) (cmpa dir) (cmpa idx) (cmpa ext)
     (cmpb imm) (cmpb dir) (cmpb idx) (cmpb ext)]
    [(        ) (        ) (bhi  rel) (pula inh)
     (        ) (        ) (        ) (        )
     (sbca imm) (sbca dir) (sbca idx) (sbca ext)
     (sbcb imm) (sbcb dir) (sbcb idx) (sbcb ext)]
    [(        ) (        ) (bls  rel) (pulb inh)
     (coma inh) (comb inh) (com  idx) (com  ext)
     (        ) (        ) (        ) (        )
     (        ) (        ) (        ) (        )]
    [(        ) (        ) (bcc  rel) (des  inh)
     (lsra inh) (lsrb inh) (lsr  idx) (lsr  ext)
     (anda imm) (anda dir) (anda idx) (anda ext)
     (andb imm) (andb dir) (andb idx) (andb ext)]
    [(        ) (        ) (bcs  rel) (txs  inh)
     (        ) (        ) (        ) (        )
     (bita imm) (bita dir) (bita idx) (bita ext)
     (bitb imm) (bitb dir) (bitb idx) (bitb ext)]
    [(tap  inh) (tab  inh) (bne  rel) (psha inh)
     (rora inh) (rorb inh) (ror  idx) (ror  ext)
     (ldaa imm) (ldaa dir) (ldaa idx) (ldaa ext)
     (ldab imm) (ldab dir) (ldab idx) (ldab ext)]
    [(tpa  inh) (tba  inh) (beq  rel) (pshb inh)
     (asra inh) (asrb inh) (asr  idx) (asr  ext)
     (        ) (staa dir) (staa idx) (staa ext)
     (        ) (stab dir) (stab idx) (stab ext)]
    [(inx  inh) (        ) (bvc  rel) (        )
     (asla inh) (aslb inh) (asl  idx) (asl  ext)
     (eora imm) (eora dir) (eora idx) (eora ext)
     (eorb imm) (eorb dir) (eorb idx) (eorb ext)]
    [(dex  inh) (daa  inh) (bvs  rel) (rts  inh)
     (rola inh) (rolb inh) (rol  idx) (rol  ext)
     (adca imm) (adca dir) (adca idx) (adca ext)
     (adcb imm) (adcb dir) (adcb idx) (adcb ext)]
    [(clv  inh) (        ) (bpl  rel) (        )
     (deca inh) (decb inh) (dec  idx) (dec  ext)
     (oraa imm) (oraa dir) (oraa idx) (oraa ext)
     (orab imm) (orab dir) (orab idx) (orab ext)]
    [(sev  inh) (aba  inh) (bmi  rel) (rti  inh)
     (        ) (        ) (        ) (        )
     (adda imm) (adda dir) (adda idx) (adda ext)
     (addb imm) (addb dir) (addb idx) (addb ext)]
    [(clc  inh) (        ) (bge  rel) (        )
     (inca inh) (incb inh) (inc  idx) (inc  ext)
     (cpx  imm) (cpx  dir) (cpx  idx) (cpx  ext)
     (        ) (        ) (        ) (        )]
    [(sec  inh) (        ) (blt  rel) (        )
     (tsta inh) (tstb inh) (tst  idx) (tst  ext)
     (bsr  rel) (        ) (jsr  idx) (jsr  ext)
     (        ) (        ) (        ) (        )]
    [(cli  inh) (        ) (bgt  rel) (wai  inh)
     (        ) (        ) (jmp  idx) (jmp  ext)
     (lds  imm) (lds  dir) (lds  idx) (lds  ext)
     (ldx  imm) (ldx  dir) (ldx  idx) (ldx  ext)]
    [(sei  inh) (        ) (ble  rel) (swi  inh)
     (clra inh) (clrb inh) (clr  idx) (clr  ext)
     (        ) (sts  dir) (sts  idx) (sts  ext)
     (        ) (stx  dir) (stx  idx) (stx  ext)]))

; There are hidden "undocumented" instructions on the 6800:
;   0x14:  NBA   inherent   Store A & B in A
;   0x87:  STAA  immediate  Store immediately the content of A in PC+2
;   0xC7:  STAB  immediate  Store immediately the content of B in PC+2
;   0xDD:  HCF   inherent   "Halt and Catch Fire": Cycle through all memory until powered off
;                           (there's also an HCF hidden in 0x9D but let's not use it)
;   0x8F:  STS   immediate  Store immediately the Stack Pointer in PC+2 and PC+3
;   0xCF:  STX   immediate  Store immediately the Index Register in PC+2 and PC+3
; The immediate stores are weird because they actually don't care about the given operand, but still require one.
; You don't want to use them in production, because they're not garanted to have
; been implemented in a given chip if the manufacturer just decided to ignore them.
(define undocumented-op-codes
  #hash([0x14 . (nba  inh)]
        [0x87 . (staa imm)]
        [0xC7 . (stab imm)]
        [0xDD . (hcf  inh)]
        [0x8F . (sts  imm)]
        [0xCF . (stx  imm)]))

(define (op-exists? mnemonic mode)
  (for/or ([line op-table])
    (findf (op-predicate mnemonic mode) line)))

(define (relative-op? mnemonic)
  (op-exists? mnemonic 'rel))

(define (16bit-opcode? mnemonic)
  (memq mnemonic '(lds ldx cpx)))

(define (extended-only-op? mnemonic)
  (and (not (op-exists? mnemonic 'dir))
       (op-exists? mnemonic 'ext)))

(define (get-value-size mnemonic mode)
  (cond
    [(eq? mode 'inh) 0]
    [(eq? mode 'dir) 1]
    [(eq? mode 'idx) 1]
    [(eq? mode 'rel) 1]
    [(eq? mode 'ext) 2]
    [(and (eq? mode 'imm)
          (16bit-opcode? mnemonic)) 2]
    [(eq? mode 'imm) 1]))

(define (get-op-code mnemonic mode)
  (define cell&line
    (for/or ([current-line op-table])
      (let ([found (findf (op-predicate mnemonic mode) current-line)])
        (and found (cons found current-line)))))
  (if cell&line
      (let* ([cell (car cell&line)]
             [line (cdr cell&line)]
             [lsb (index-of op-table line)]
             [msb (index-of line cell)])
        (+ (arithmetic-shift msb 4) lsb))
      (error 'opcode-not-found
             "The given operation was not found: ~a in ~a mode"
             mnemonic (format-mode mode))))

(define (get-mnemonic op-code)
  (define msb (arithmetic-shift op-code -4))
  (define lsb (bitwise-and op-code #x0F))
  (apply values (list-ref (list-ref op-table lsb) msb)))

(define (op-predicate mnemonic mode)
  (lambda (elt)
    (and (pair? elt)
         (eq? (car elt) mnemonic)
         (eq? (cadr elt) mode))))

(define (format-mode mode)
  (match mode
    ['inh "inherent"]
    ['rel "relative"]
    ['imm "immediate"]
    ['dir "direct"]
    ['ext "extended"]
    ['idx "indexed"]))
