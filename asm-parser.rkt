#lang brag
program: [line] (/NEWLINE line)*
line: (instruction | assignment | data-decl | /SPACE* | comment) /SPACE* [comment]

instruction: [tag] [/SPACE+ mnemonic (/SPACE+ operand)*]
tag: ID | number
mnemonic: ID
operand: register | (value [indexed])
register: "a" | "b"
@value: ([immediate] number) | (ID [modifier])
immediate: /"#"
modifier: ("+" | "-" | "*" | "/") number
indexed: /",x"

assignment: ID /SPACE* /"=" /SPACE* number

data-decl: [tag] /SPACE+ /".data" (/SPACE+ data)*
@data: STRING | number

comment: COMMENT
number: NUMBER
