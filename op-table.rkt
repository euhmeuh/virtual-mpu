#lang racket/base

(provide
  op-table
  op-exists?
  get-op-code)

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

(define (op-exists? mnemonic mode)
  (for/or ([line op-table])
    (findf (op-predicate mnemonic mode) line)))

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
