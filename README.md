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

## DLG compiler

### Compiler optimisations

The compiler yields **optimisations** that *do not change the meaning of the program*, but reduce the size of the bytecode (by reducing the size of the AST).

#### Inside programs

The followings optimisations are done by the compiler in a program :
* a group of `nop` instructions will be collapsed into one
* `wait` instructions with no event specified and a duration of 0 will be removed

The followings optimisations are done by the compiler in conditions (`? expr` blocks) :
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
* **script variables** that have a scope limited to the DLG interpreter :
  ```dlg
  set local var x
  set global var x
  ```
* **extern variables** that are accessing directly objects from your game :
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

## Interpreter

DLG files are then compiled into bytecode. A DLG interpreter has three big parts :
* a **stack**, where data, instructions and commands will be pushed
* an **environment**, used to store all the variables and their scope, be they global or local
* a **memory case** containing one value that can be memorized (MEM) from the top of the stack or duplicated and pushed on top of the stack (DUPL)
* a **program buffer**, which is usually a stream or an array containing the compiled bytecode that can do the following operations :
  * get the **next** byte (returns a byte) and moves the cursor forward
  * **seek** a position in a file and set the cursor on it
  * **peek** the next character from a file; that is, get the next byte without moving the cursor
  * **pos** returns the posiiton of the cursor in the file
  * check if we're at the **eof** (end of file)

### Bytecode

The opcode is the first byte read from an instruction ; the additional data is a buffer found right after the opcode.

Endianness of additional data is stored in little-endian.

#### Stack & environment management


| name | opcode  | additional data | effect | category |
| --- | --- | --- | --- | --- |
| EOD | 0x00 | None | Indicates the end of some data (usually a string) | Special markers |
| MEM | 0x01 | None | Copies the token from the top of the stack in memory  | Stack management |
| DUPL  | 0x02 | None  | Duplicate the token in memory and push it on the stack | Stack management |
| Deepen Scope  | 0x03 | None | Deepens the scope. The variables declared after this statement will be only accessible in this scope level block | Stack management |
| Raise Scope  | 0x04 | None | Raises the scope. The variables created at the previous scope level will be destroyed | Stack management |

#### Control flow

| name | opcode  | additional data | effect |
|---|---|---|---|
| Skip if not | 0x05 | n=**int64** (8 bytes) | Pulls a token from the stack. If this is a boolean valued at `false`, skip *n* bytes of program |
| Skip | 0x06 | n=**int64** (8 bytes) | Skips *n* bytes of program |

#### Instructions

| name | opcode  | additional data | effect |
|---|---|---|---|
| Set | 0x20 | None | Pulls an identifier, and a value from the stack, and bind the value to the identifier in the environment |
| Ifnset | 0x21 | None | Pulls an identifier, and a value from the stack, and bind the value to the identifier in the environment if and only if the variable was never bound |
| Init | 0x22 | None | Pulls an identifier, and a value from the stack, and bind the value to the identifier in the environment if and only if the variable was never bound. Fail if identifier was already bound |
| Message | 0x23 | f=**flags** (1 byte) | Pulls a string value from the stack and display it |
| Wait | 0x24 | None |  |
| Speed | 0x25 | None |  |
| Invoke | 0x26 | None |  |
| Send | 0x27 | None |  |
| Choice | 0x28 | None | Pulls an integer n from the stack representing the number of choices, pull n string values, and waits for user to push on the stack an integer value 0<=i<n representing the choice taken |
| Nop | 0x80 | None | Does nothing |

When an instruction is reached, the data in the stack will be used as the parameters for it.
For instance, with the *set* instruction :

```
                  PROGBUF                                  STACK
_______________________________________________   __________________________
[0x90 0x06 0x00 0x00 0x00] 0x62 0x6E 0x00 0x20  ⊢  ∅
[0x62 0x6E 0x00] 0x20                           ⊢  Int(6)
[0x20]                                          ⊢  Id(local x) Int(6)
```

When `0x20` is reached, two tokens are pulled from the stack. If they are respectively an identifier and an int, they're used as parameters for the *set* instruction evaluated by the interpreter.

#### Identifiers

| name | opcode  | additional data | effect |
|---|---|---|---|
| Extern identifier | 0x60 | Null-terminated string representing identifier's name | An extern identifier |
| Global identifier | 0x61 | Null-terminated string representing identifier's name | A global identifier |
| Local identifier | 0x62 | Null-terminated string representing identifier's name | A local identifier |

When an identifier is reached, the bytes in the progbuf must be read until a 0x00 is encountered in order to have a string representing its name.
For instance, with a local identifier :

```
                  PROGBUF                                  STACK
_______________________________________________   __________________________
[0x62] 0x6E 0x00                                ⊢  ∅
[0x62] [0x6E 0x00]                              ⊢  ∅
∅                                               ⊢  Id(local {0x68})
∅                                               ⊢  Id(local "x")
```

#### Expressions

| name | opcode  | additional data | effect |
|---|---|---|---|
| Extern variable | 0x81 | Null-terminated string representing identifier's name | Access to an extern (not contained in the environment) variable |
| Global variable | 0x82 | Null-terminated string representing identifier's name | Access to a global (shared between all scripts) variable |
| Local variable | 0x83 | Null-terminated string representing identifier's name | Access to a local (accessible only in this script) variable |
|---|---|---|---|---|---|
| Int literal | 0x90 | n=**int32** (4 bytes) | An int value |
| Float literal | 0x91 | f=**float** (4 bytes, IEEE-754 floating-point) | An float value |
| Bool literal | 0x92 | 0x00 if *false*, 0xFF if *true* | A boolean value |
| String literal | 0x93 | Null-terminated string | A string value |
| Enum literal | 0x94 | None | ? |
| 2D vector literal | 0x95 | None | Pulls two float x,y values from the stack, and build a 2D vector from them |
| 3D vector literal | 0x96 | None | Pulls three float values x,y,z from the stack, and build a 3D vector from them |
|---|---|---|---|---|---|
| Inline  | 0x9F | None | Pulls a value from the stack, turns it into a string, and pushes the result in the stack |
| Operator + | 0xA0 | 0x00 | Pulls two values from the stack, add them, and pushes the result in the stack |
| Operator - | 0xA0 | 0x01 | Pulls two values from the stack, subtract them, and pushes the result in the stack |
| Operator * | 0xA0 | 0x02 | Pulls two values from the stack, multiply them, and pushes the result in the stack |
| Operator / | 0xA0 | 0x03 | Pulls two values from the stack, divide them, and pushes the result in the stack |
| Operator && | 0xA0 | 0x04 | Pulls two boolean values from the stack, and them, and pushes the result in the stack |
| Operator \|\| | 0xA0 | 0x05 | Pulls two boolean values from the stack, or them, and pushes the result in the stack |
| Operator == | 0xA0 | 0x06 | Pulls two values from the stack, check for their equality, and pushes the boolean result in the stack |
| Operator != | 0xA0 | 0x07 | Pulls two values from the stack, check for their non-equality, and pushes the boolean result in the stack |
| Operator <= | 0xA0 | 0x08 | Pulls two numbers from the stack, compare them, and pushes the boolean result in the stack |
| Operator >= | 0xA0 | 0x09 | Pulls two numbers from the stack, compare them, and pushes the boolean result in the stack |
| Operator < | 0xA0 | 0x0A | Pulls two numbers from the stack, compare them, and pushes the boolean result in the stack |
| Operator > | 0xA0 | 0x0B | Pulls two numbers from the stack, compare them, and pushes the boolean result in the stack |
| Ternary condition | 0xA1 | None | Pulls a boolean and two values of the same type from the stack, checks the boolean value, pushes the first one on the stack if true, else the second one  |
| Function call | 0xA2 | None | For a function with n arguments, pull an external identifier, n values from the stack, and pushes the result value |
| Cast | 0xA3 | 1 byte representing the type to cast to (equal to the corresponding literal instruction between 0x90 and 0x96) | Pulls a value from the stack, converts it to type, and pushes it on the stack |
| Access | 0xA4 | None | Pulls a value from the stack, and try to access one of its properties (usually x, y or z in case of vectors) |

### Dynamic type checking


The **interpreter** is an interface in a given programming language that implements all semantic functions of the language and a bytecode interpreter, which allows a programmer to virtually run the DLG language everywhere.
