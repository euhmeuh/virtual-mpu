#lang racket/base

(provide
  assemble
  (struct-out program)
  (struct-out line)
  (struct-out instruction)
  (struct-out register)
  (struct-out variable)
  (struct-out assignment)
  (struct-out data)
  number
  modifier
  current-value-table
  resolve-value)

(require
  racket/string
  racket/list
  racket/match
  (only-in racket/contract/base >/c)
  anaphoric
  "utils.rkt")

(struct program (source-tree expressions) #:transparent)
(struct line (expression comment) #:transparent)
(struct instruction (tag mnemonic operands) #:transparent)
(struct register (value) #:transparent)
(struct variable (value immediate? indexed?) #:transparent)
(struct assignment (name value) #:transparent)
(struct data (tag values) #:transparent)

; first list index is least significant byte
; second list index is most significant byte
; inh: inherent       rel: relative
; imm: immediate      dir: direct
; ext: extended       idx: indexed
(define code-table
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

(define current-pc (make-parameter 0))
(define current-value-table (make-parameter (make-hash)))
(define current-generated-block-size (make-parameter 32))

(define (assemble filepath)
  (define program (dynamic-require filepath 'program))
  (build-bytestring
    (resolve-relative-branches
      (walk-expressions (filter values (program-expressions program))))))

(define (walk-expressions expressions)
  (filter values
    (for/list ([expr expressions])
      (cond
        [(assignment? expr)
         (add-variable! (assignment-name expr)
                        (assignment-value expr))
         #f]
        [(or (instruction? expr)
             (data? expr))
         (handle-tag! expr)
         (let ([binary (expression->binary expr)]
               [pc (current-pc)])
           (advance-pc! (length binary))
           (cons pc binary))]))))

(define (resolve-value val)
  (cond
    [(symbol? val) (hash-ref (current-value-table) val val)]
    [(procedure? val) (val)]
    [else val]))

(define (modifier func value amount)
  (lambda ()
    (func (resolve-value value)
          (resolve-value amount))))

(define (add-variable! name value)
  (hash-set! (current-value-table) name value))

(define (handle-tag! expr)
  (define tag (if (data? expr)
                  (data-tag expr)
                  (instruction-tag expr)))
  (cond
    [(symbol? tag) (add-variable! tag (current-pc))]
    [(number? tag) (current-pc tag)]))

(define (advance-pc! amount)
  (current-pc (+ (current-pc) amount)))

(define (expression->binary expr)
  (if (data? expr)
      (data->binary expr)
      (instruction->binary expr)))

(define (data->binary data)
  (for/fold ([result '()])
            ([value (data-values data)])
    (append result
            (cond
              [(string? value)
               (bytes->list (string->bytes/utf-8 value))]
              [else
               (split-into-bytes '() value)]))))

(define (instruction->binary instr)
  (define-values (mnemonic operands) (resolve-operands instr))
  (define var (try-get-variable operands))
  (define value (and var (resolve-value (variable-value var))))
  (when (and value
             (symbol? value)
             (not (relative-op? mnemonic)))
    (error 'variable-not-found "The given variable was not declared: ~a" value))
  (define mode
    (if var
        (cond
          [(relative-op? mnemonic) 'rel]
          [(variable-indexed? var) 'idx]
          [(variable-immediate? var) 'imm]
          [(> value #xFF) 'ext]
          [else 'dir])
        'inh))
  (define opcode (and (not (pseudo-op? mnemonic))
                      (find-opcode mnemonic mode)))
  (define final-values (and value (format-value value mnemonic mode)))
  (when final-values
    (awhen (findf (>/c #xFF) final-values)
      (error 'value-too-large
             "The given value is larger than a byte: ~a"
             (format-hex it))))
  (cond
    [(not opcode) final-values]
    [(not final-values) (list opcode)]
    [else (cons opcode final-values)]))

(define (resolve-operands instr)
  (define operands (instruction-operands instr))
  (define mnemonic (instruction-mnemonic instr))
  (when (and (pair? operands)
             (register? (car operands)))
    (set! mnemonic
          (symbol-append mnemonic
                         (register-value (car operands))))
    (set! operands (drop operands 1)))
  (values mnemonic operands))

(define (try-get-variable operands)
  (and (pair? operands)
       (variable? (car operands))
       (car operands)))

(define (relative-op? mnemonic)
  (find-mnemonic mnemonic 'rel))

(define (pseudo-op? mnemonic)
  (memq mnemonic '(db dw)))

(define (16bit-opcode? mnemonic)
  (memq mnemonic '(lds ldx cpx)))

(define (format-value value mnemonic mode)
  (if (eq? mode 'rel)
      (list (cons 'relative value)) ; delay the value for second pass processing
      (split-into-bytes '() value (get-value-size mnemonic mode))))

(define (split-into-bytes result value [size #f])
    (if (or (<= value #xFF)
            (and size (<= size 1)))
        (cons value result)
        (split-into-bytes (cons (bitwise-and value #xFF) result)
                          (arithmetic-shift value -8)
                          (and size (- size 1)))))

(define (format-7bit-signed value)
  (if (or (> value 127) (< value -128))
      (error 'relative-value "outside of range (-128, 127): ~a" value)
      (if (>= value 0) value (+ 256 value))))

(define (get-value-size mnemonic mode)
  (cond
    [(pseudo-op? mnemonic)
     (if (eq? mnemonic 'dw) 2 1)]
    [(eq? mode 'inh) 0]
    [(eq? mode 'dir) 1]
    [(eq? mode 'idx) 1]
    [(eq? mode 'rel) 1]
    [(eq? mode 'ext) 2]
    [(and (eq? mode 'imm)
          (16bit-opcode? mnemonic)) 2]
    [(eq? mode 'imm) 1]))

(define (find-mnemonic mnemonic mode)
  (for/or ([line code-table])
    (findf (mnemonic-predicate mnemonic mode) line)))

(define (find-opcode mnemonic mode)
  (define cell&line
    (for/or ([current-line code-table])
      (let ([found (findf (mnemonic-predicate mnemonic mode) current-line)])
        (and found (cons found current-line)))))
  (if cell&line
      (let* ([cell (car cell&line)]
             [line (cdr cell&line)]
             [lsb (index-of code-table line)]
             [msb (index-of line cell)])
        (+ (arithmetic-shift msb 4) lsb))
      (error 'opcode-not-found
             "The given operation was not found: ~a in ~a mode"
             mnemonic (format-mode mode))))

(define (mnemonic-predicate mnemonic mode)
  (lambda (elt)
    (and (pair? elt)
         (eq? (car elt) mnemonic)
         (eq? (cadr elt) mode))))

(define (resolve-relative-branches pos&bytes)
  (for/list ([pos&byte pos&bytes])
    (define pos (car pos&byte))
    (define bytes (cdr pos&byte))
    (cons pos
          (map (match-lambda
                 [(cons 'relative value)
                  (format-7bit-signed
                     (- (resolve-value value) (+ pos 2)))]
                 [byte byte])
               bytes))))

(define (build-bytestring pos&bytes)
  (for/fold ([bytestring (make-bytes (current-generated-block-size))])
            ([pos&byte pos&bytes])
    (define pos (car pos&byte))
    (define bytes (cdr pos&byte))
    (for ([byte bytes] [i (in-naturals)])
      (when (<= (bytes-length bytestring) pos)
        (set! bytestring
              (bytes-append bytestring
                            (make-bytes (current-generated-block-size)))))
      (bytes-set! bytestring (+ pos i) byte))
    bytestring))

(define (format-mode mode)
  (match mode
    ['inh "inherent"]
    ['rel "relative"]
    ['imm "immediate"]
    ['dir "direct"]
    ['ext "extended"]
    ['idx "indexed"]))

(define (number str)
  (if (string-prefix? str "$")
      (string->number (substring str 1) 16)
      (string->number str)))
