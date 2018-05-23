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
         value
         (rename-out [asm:number number]))

(require
  (prefix-in asm: "assembler.rkt"))

(define-syntax-rule (module-begin asm)
  (#%module-begin
    (provide program)
    (define program asm)))

(define-syntax-rule (assembly line ...)
  (asm:program (list 'line ...)
               (map asm:line-expression
                    (filter values (list line ...)))))

(define-syntax line
  (syntax-rules (comment)
    [(_) #f]
    [(_ instruction)
     (asm:line instruction #f)]
    [(_ instruction (comment a-comment))
     (asm:line instruction a-comment)]))

(define-syntax instruction
  (syntax-rules (tag mnemonic)
    [(_ (tag a-tag) (mnemonic a-mnemonic) operand ...)
     (asm:instruction (value a-tag) 'a-mnemonic (list operand ...))]
    [(_ (mnemonic a-mnemonic) operand ...)
     (asm:instruction #f 'a-mnemonic (list operand ...))]))

(define-syntax data-decl
  (syntax-rules (tag)
    [(_ (tag a-tag) datum ...)
     (asm:data (value a-tag) (list datum ...))]
    [(_ datum ...)
     (asm:data #f (list datum ...))]))

(define-syntax assignment
  (syntax-rules ()
    [(_ name val)
     (asm:assignment 'name val)]))

(define-syntax operand
  (syntax-rules (register immediate modifier)
    [(_ (register reg)) (asm:register 'reg)]
    [(_ val) (asm:variable (value val) #f #f)]
    [(_ val (indexed)) (asm:variable (value val) #f #t)]
    [(_ val (modifier func amount)) (asm:variable (asm:modifier func (value val) (value amount)) #f #f)]
    [(_ val (modifier func amount) (indexed)) (asm:variable (asm:modifier func (value val) (value amount)) #f #t)]
    [(_ (immediate) val) (asm:variable (value val) #t #f)]))

(define-syntax value
  (syntax-rules ()
    [(_ (number val)) (asm:number val)]
    [(_ val) 'val]))

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
