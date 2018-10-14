# language-dlg

DLG is a scripting language bundled with expression evaluation & code interactivity for narratives in games.

## What is it for ?

## Building

This project depends on the following modules !
  * menhir
  * sexplib
  * ppx_sexp_conv
  * ANSITerminal

You can install them by typing :
```bash
$$ opam install menhir sexplib ppx_sexp_conv ANSITerminal
```

## DLG language

### Semantics

### Syntax & grammar

The grammar for the DLG language implemented in the compiler in is the following :

```dlg
; a program is a sequential list of instructions
program ::= { instruction }

; one action. usually occupies one line
instruction ::=
              ; text content senders
                | message
                | ? INDENT choice [{, choice}] OUTDENT
              ; text display options
                | speed expr
              ; flow control
                | ? expression INDENT { branches } OUTDENT
                | wait expression
                | label const_id
                | goto const_id
                | nop
              ; variable definitions
                | set variable expr
                | ifnset variable
              ; game program interactivity
                | invoke extern_id ([expr{, expr}])
                | send extern_id   [expr]
                | wait const_id then expr

; an expression
expr ::=
       ; a literal
         | literal
       ; a variable
         | variable
       ; bracketing
         | (expr)
       ; a function call
         | const_id([expr{, expr}])
       ; an operator call
         | expr infixop expr

; an infix operator
infixop ::= + | - | * | / | && | || | = | != | <= | >= | < | >

; a variable
variable ::=
          ; a local variable
            | [local] const_id
          ; a global variable
            | [global|#] const_id
          ; an extern variable
            | [extern] extern_id


; a choice displayed
choice ::= ->message INDENT program OUTDENT
; a branch
branch ::= ->pattern INDENT program OUTDENT
; a pattern
pattern ::=
          ; a wildcard token
            | _
          ; a simple value
            | expression
          ; a binding pattern with a condition
            | const_id when expr

; a literal
literal ::=
          | int32[%]
          | float
          | 'string'
          | true | false
          | vec2(expr, expr)
          | vec3(expr, expr, expr)
          | enum(const_id, const_id)

; a message and the options
message ::= "string" [{message_opt }]
message_opt ::= norush | noack

; a string
string ::= [{ stringtoken }]
stringtoken ::=
              | char*
              | $expr$
              | \c{ (#color)? }
color ::= #[0-9A-Fa-f]{3} | #[0-9A-Fa-f]{6}

; identifiers
extern_id ::= const_id.const_id(.const_id)*
const_id ::= [a-zA-Z_][a-zA-Z0-9_]*
```

## DLG bytecode
