# language-dlg

DLG is a scripting language bundled with expression evaluation & code interactivity for narratives in games.

## What is it for ?

## Building

This project depends on the following modules :
  * menhir
  * dune
  * ANSITerminal

You can install them by typing :
```bash
$$ opam install menhir dune ANSITerminal
```

You can build the project by going to the root folder and using :
```bash
$$ make
```

## DLG language

### Syntax

## DLG

### Compiler optimisations

The compiler yields optimisations that do not change the meaning of the program, but reduce the size of the bytecode (by reducing the size of the AST).

#### Inside programs

The followings optimisations are done by the compiler in a program :
* a group of `nop` instructions will be collapsed into one
* `wait` instructions with no event specified and a duration of 0 will be removed

In conditions (`? expr` blocks) :
* Branches appearing after a wildcard pattern will be removed
  ```dlg
  ? Player.Stats.Charisma
    -> n when n > 10
      nop
    -> _
      nop
    -> n when n = 0 ; this branch will be unused, hence removed
      nop
  ```
* Unreachable branches will be removed
* If there is only one branch in a condition corresponding to a wildcard pattern or an all-capturing expression (`->n when true`), the condition testing will be ignored


#### Inside expressions

When possible, expressions will be evaluated by the compiler.
For instance, the expressions `vec2(2*0, 3*4)` and `2*(3+3)` will be respectively replaced by `vec2(0, 12)` and `12`.
Other optimisations include :
* Boolean operators with one static absorbing argument can be reduced;
  * `true||expr` and `expr||true` => `true`
  * `false&&expr` and `expr&&false` => `false`
* Trivial conditions will be reduced to the concerned branch (̀`true?=a:b` => `a`)
* Property access will be reduced in trivial cases (`vec2(1,-1).[x]` => `1.0`)
* Typecasts will be reduced in trivial cases (`(int)3.0` => `3`)

### Type system

In the DLG language, there are two type checking procedures at play when you compile and run a script.
* **static type checking**, which takes place at compilation
* **dynamic type checking**, which takes place at runtime

#### Static type checking

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

Because of that, the compiler cannot always know for sure that a variable is of a given type. For instance, in the expression `f() * 3`, the compiler cannot know which type the resulting expression will be, because `f` is not defined in the context of a DLG script. It follows :
  * a **local** variable will always have a type, because it must have been declared at some point in the program
  * a **global** variable will not always have a type, because it can be set in another script. However, if it is set in the current script, the type will be assumed by the compiler.
  * an **extern** valiable will never have a type, unless declared. If not, the compiler gives it a "no assumption type", and type checking is done at runtime.


In order to assert that a global variable or an extern symbol (a function, a variable, ..) is of a given type, you can use the following instruction to tell the compiler the type of the symbol.
```dlg
declare extern My.extern.variable type
declare extern My.extern.function type (arg1 arg2 arg3)
declare extern myfunc (arg1 arg2 arg3)

declare global myvariable type
  ```


The goal of type checking is to avoid errors that might be hard for the programmer to notice, so the static type checking of the DLG parser uses a little trick :
  * a *literal* has an **actual type** that the compiler can recognize for sure
  * a *local* variable have to be defined before it is used, so the compiler can infer the type of the variable : it will also have an **actual type**
  * *global* and *external* variables cannot have a known type at compilation, so the compiler will give them an **expected type**, unless they're explicitely declared. Then they will have an **actual type**.
  * a *function invocation* will also have an **expected type** because of its external nature, unless it's explicitely declared. Then it will have an **actual type**.

This ensures a few things :
  * operations that only use identifiers known by the compiler will be fully typed
  * operations that includes external identifiers can be declared and be typed statically by the compiler; else, they will have a type inferred from their use when possible, and if not misuse won't be caught by the static compiler

Whenever an expression is left unchecked by the static compiler, it will be typed by the dynamic type checking system at runtime to ensure type correctness. The data computed by the static compiler will be transferred to the dynamic type compiler to avoid recomputation.

##### Type inference

#### Dynamic type checking


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
              ; type checking
                | declare global const_id
                | declare extern const_id|extern_id

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

; a type
type ::= void | int | float | bool | string | vec2 | vec3 | enum(const_id)

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

## Bytecode

## Interpreter

The **interpreter** is an interface in a given programming language that implements all semantic functions of the language and a bytecode interpreter, which allows a programmer to virtually run the DLG language everywhere.
