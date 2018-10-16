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

### Syntax



### Grammar & tokens

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
                | send extern_id [expr]
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
       ; a ternary condiction
         | expr ?= expr : expr
       ; an access over a constructed type
         | expr.[const_id]

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

## Type system

In the DLG language, there are two type checking procedures at play when you compile and run a script.
* **static type checking**, which takes place at compilation
* **dynamic type checking**, which takes place at runtime

### Static type checking

In a DLG program, you can have two kinds of variables :
* *script variables* that have a scope limited to the DLG interpreter :
  ```dlg
  set local var x
  set global var x
  ```
* *extern variables* that are accessing directly objects from your game :
  ```dlg
  set extern var x
  ```

Because of that, the compiler cannot know for sure at runtime that a variable is of a given type. For instance, in the expression `f() * 3`, the compiler cannot know which type the resulting expression will be, because `f` is not defined in the context of a DLG script.

The goal of type checking is to avoid errors that might be hard for the programmer to notice, so the static type checking of the DLG parser uses a little trick :
  * a *literal* has an **actual type** that the compiler can recognize for sure
  * a *local* variable have to be defined before it is used, so the compiler can infer the type of the variable : it will also have an **actual type**
  * *global* and *external* variables cannot have a known type at compilation, so the compiler will give them an **expected type**
  * a *function invocation* will also have an **expected type** because of its internal nature

In composing types to ensure type correctness, the compiler will use the following rules to compose types when they interact together :
  * two *actual types* will form an *actual type*
  * an *expected type* and an *actual type* will form an *expected type*
  * two *expected types* will form an *expected type*
  * an *expected type* can carry no assumption regarding the type it will produce. in that case, it will be ignored by the computer and considered as valid in all cases

This ensures a few things :
  * operations that only use identifiers known by the compiler will be fully typed
  * operations that includes external identifiers will have a type inferred from their use when possible, else it won't be caught by the static compiler

Whenever an expression is left unchecked by the static compiler, it will be typed by the dynamic type checking system at runtime to ensure type correctness. The data computed by the static compiler will be transferred to the dynamic type compiler to avoid recomputations.

In order to assert that a global variable or an extern symbol (a function, a variable, ..) is of a given type, you can use the following instruction to tell the compiler the type of the symbol.
```dlg
declare extern My.extern.variable type
declare extern My.extern.function type (arg1 arg2 arg3)

declare global myvariable type
```

### Dynamic type checking



## Bytecode

## Interpreter

The **interpreter** is an interface in a given programming language that implements all semantic functions of the language and a bytecode interpreter, which allows a programmer to virtually run the DLG language everywhere.
