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
               (map asm:line-expression (list line ...))))

(define-syntax line
  (syntax-rules (comment)
    [(_ instruction)
     (asm:line instruction #f)]
    [(_ instruction (comment a-comment))
     (asm:line instruction a-comment)]))

(define-syntax instruction
  (syntax-rules (tag mnemonic)
    [(_) #f]
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
    [(_ val (modifier func amount)) (asm:variable (asm:modifier func amount (value val)) #f #f)]
    [(_ val (modifier func amount) (indexed)) (asm:variable (asm:modifier func amount (value val)) #f #t)]
    [(_ (immediate) val) (asm:variable (value val) #t #f)]))

(define-syntax value
  (syntax-rules ()
    [(_ (number val)) (asm:number val)]
    [(_ val) 'val]))
