#lang brag
assembly: [line] (/NEWLINE line)*
line: [instruction | assignment | data-decl] /SPACE* [comment]

instruction: [tag] [/SPACE+ mnemonic (/SPACE+ operand)*]
tag: value
@value: ID | number
mnemonic: ID
operand: register | ([immediate] modified-value) | (modified-value [indexed])
register: "a" | "b"
immediate: /"#"
@modified-value: value [modifier]
modifier: ("+" | "-" | "*" | "/") value
indexed: /",x"

assignment: ID /SPACE* /"=" /SPACE* number

data-decl: [tag] /SPACE+ /".data" (/SPACE+ data)*
@data: STRING | number

comment: COMMENT
number: NUMBER
