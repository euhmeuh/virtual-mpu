#lang brag
assembly: [line] (/NEWLINE line)*
line: (instruction | assignment | data-decl | /SPACE* | comment) /SPACE* [comment]

instruction: [tag] [/SPACE+ mnemonic (/SPACE+ operand)*]
tag: ID | number
mnemonic: ID
operand: register | ([immediate] number) | (number [indexed]) | (ID [modifier] [indexed])
register: "a" | "b"
immediate: /"#"
modifier: ("+" | "-" | "*" | "/") number
indexed: /",x"

assignment: ID /SPACE* /"=" /SPACE* number

data-decl: [tag] /SPACE+ /".data" (/SPACE+ data)*
@data: STRING | number

comment: COMMENT
number: NUMBER
