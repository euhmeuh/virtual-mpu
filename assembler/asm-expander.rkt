#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         assembly
         line
         instruction
         assignment
         data-decl
         operand
         (rename-out [asm:number number]))

(require
  syntax/parse
  (for-syntax
    racket/base
    syntax/parse)
  (prefix-in asm: "assembler.rkt"))

(begin-for-syntax
  (define-splicing-syntax-class maybe-immediate
    #:datum-literals (immediate)
    (pattern (~seq (immediate)) #:with immediate? #'#t)
    (pattern (~seq) #:with immediate? #'#f))

  (define-splicing-syntax-class maybe-indexed
    #:datum-literals (indexed)
    (pattern (~seq (indexed)) #:with indexed? #'#t)
    (pattern (~seq) #:with indexed? #'#f))

  (define-syntax-class maybe-number
    #:datum-literals (number)
    (pattern (number val) #:with value #'(asm:number val))
    (pattern val #:with value #''val))

  (define-splicing-syntax-class maybe-modified-value
    #:datum-literals (modifier)
    (pattern (~seq var:maybe-number (modifier func:id amount:maybe-number))
             #:with value #'(asm:modifier func var.value amount.value))
    (pattern (~seq var:maybe-number)
             #:with value #'var.value)))

(define-syntax-rule (module-begin asm)
  (#%module-begin
    (provide program)
    (define program asm)))

(define-syntax-rule (assembly line ...)
  (asm:program (list 'line ...)
               (filter values (map asm:line-expression
                                   (filter values (list line ...))))))

(define-syntax line
  (syntax-rules (comment)
    [(_) #f]
    [(_ (comment a-comment))
     (asm:line #f a-comment)]
    [(_ expression)
     (asm:line expression #f)]
    [(_ expression (comment a-comment))
     (asm:line expression a-comment)]))

(define-syntax (instruction stx)
  (syntax-parse stx
    #:datum-literals (tag mnemonic)
    [(_ (tag a-tag:maybe-number) (mnemonic a-mnemonic) operand ...)
     #'(asm:instruction a-tag.value 'a-mnemonic (list operand ...))]
    [(_ (mnemonic a-mnemonic) operand ...)
     #'(asm:instruction #f 'a-mnemonic (list operand ...))]))

(define-syntax (data-decl stx)
  (syntax-parse stx
    #:datum-literals (tag)
    [(_ (tag a-tag:maybe-number) datum ...)
     #'(asm:data a-tag.value (list datum ...))]
    [(_ datum ...)
     #'(asm:data #f (list datum ...))]))

(define-syntax assignment
  (syntax-rules ()
    [(_ name val)
     (asm:assignment 'name val)]))

(define-syntax (operand stx)
  (syntax-parse stx
    #:datum-literals (register)
    [(_ (register reg)) #'(asm:register 'reg)]
    [(_ mi:maybe-immediate mv:maybe-modified-value)
     #'(asm:variable mv.value mi.immediate? #f)]
    [(_ mv:maybe-modified-value mx:maybe-indexed)
     #'(asm:variable mv.value #f mx.indexed?)]))

(module+ test
  (require rackunit)

  (define-binary-check (asm-variable-equal? a b)
    (and (equal? (asm:resolve-value (asm:variable-value a))
                 (asm:resolve-value (asm:variable-value b)))
         (equal? (asm:variable-immediate? a)
                 (asm:variable-immediate? b))
         (equal? (asm:variable-indexed? a)
                 (asm:variable-indexed? b))))

  (asm:current-value-table #hash([banana . 42]))

  (check-equal? (operand (register a))
                (asm:register 'a))

  (asm-variable-equal?
    (operand (number "$80"))
    (asm:variable #x80 #f #f))

  (asm-variable-equal?
    (operand (number "$80") (indexed))
    (asm:variable #x80 #f #t))

  (asm-variable-equal?
    (operand (number "$80") (modifier + (number "1")))
    (asm:variable (asm:modifier + #x80 1) #f #f))

  (asm-variable-equal?
    (operand (number "$80") (modifier + (number "1")) (indexed))
    (asm:variable (asm:modifier + #x80 1) #f #t))

  (asm-variable-equal?
    (operand (number "$80") (modifier + banana))
    (asm:variable (asm:modifier + #x80 'banana) #f #f))

  (asm-variable-equal?
    (operand (number "$80") (modifier + banana) (indexed))
    (asm:variable (asm:modifier + #x80 'banana) #f #t))

  (asm-variable-equal?
    (operand (immediate) (number "$80"))
    (asm:variable #x80 #t #f))

  (asm-variable-equal?
    (operand banana)
    (asm:variable 'banana #f #f))

  (asm-variable-equal?
    (operand banana (indexed))
    (asm:variable 'banana #f #t))

  (asm-variable-equal?
    (operand banana (modifier + (number "1")))
    (asm:variable (asm:modifier + 'banana 1) #f #f))

  (asm-variable-equal?
    (operand banana (modifier + (number "1")) (indexed))
    (asm:variable (asm:modifier + 'banana 1) #f #t))

  (asm-variable-equal?
    (operand banana (modifier + banana))
    (asm:variable (asm:modifier + 'banana 'banana) #f #f))

  (asm-variable-equal?
    (operand banana (modifier + banana) (indexed))
    (asm:variable (asm:modifier + 'banana 'banana) #f #t))

  (asm-variable-equal?
    (operand (immediate) banana)
    (asm:variable 'banana #t #f)))
