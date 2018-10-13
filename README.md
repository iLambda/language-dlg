# language-dlg

DLG is a scripting language bundled with expression evaluation & code interactivity for narratives in games.

## What is it for ?

## The DLG language

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
              ; variable related
                | set [global|local] const_id
                | set [object] obj_id
                | ifnset [global|local] const_id
                | ifnset [object] obj_id
              ; game program interactivity
                | invoke obj_id ([expr{, expr}])
                | send obj_id [expr]
                | wait const_id expr

; an expression
expr ::=
       ; a literal
         | literal
       ; a variable identifier
         | const_id
       ; bracketing
         | (expr)
       ; a function call
         | const_id([expr{, expr}])
       ; an operator call
         | expr infixop expr

; an infix operator
infixop ::= + | - | * | / | && | || | = | != | <= | >= | < | >


; a choice displayed
choice ::= -message INDENT program OUTDENT
; a branch
branch ::= -pattern INDENT program OUTDENT
; a pattern
pattern ::=
          ; a wildcard token
            | _
          ; a simple value
            | expression
          ; a binding pattern with a condition
            |  const_id when expr

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
object_id ::= const_id.const_id(.const_id)*
const_id ::= [a-zA-Z_][a-zA-Z0-9_]*

```
