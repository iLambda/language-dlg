(* The abstract syntax tree for hopix programs. *)
open Utils.Position

(* A value type *)
type type_const =
  (* Literal types *)
  | TInt             (* an int32 *)
  | TFloat           (* a floating point number *)
  | TBool            (* a boolean *)
  | TString          (* a string *)
  | TEnum of string  (* an enumeration *)
  | TVec2            (* a 2d-vector *)
  | TVec3            (* a 3d-vector *)
  (* Special types *)
  | TUnknown         (* a polymorphic type, for extern scoped_identifiers *)
  | TVoid            (* a void type, for functions that return nothing *)
(* A function type *)
type type_func = type_const * type_const list
(* A type *)
type type_container =
  | TCValue of type_const
  | TCFunc of type_func

(* A program is a list of instructions. *)
type program = instruction located list
(* An instruction *)
and instruction =
  (* A label *)
  | ILabel of identifier located
  (* A goto statement *)
  | IGoto of identifier located
  (* A set variable statement *)
  | ISet of scoped_identifier located * expression located
  (* A ifnset variable statement (if not set, do) *)
  | IIfnset of scoped_identifier located * expression located
  (* A wait n seconds statement *)
  | IWait of identifier located option * expression located
  (* A nop instruction (do nothing) *)
  | INop
  (* A 'print message' instruction *)
  | IMessage of message located
  (* A choice *)
  | IChoice of (message located * program located) list
  (* A condition *)
  | ICondition of expression located * ((pattern located * program located) list)
  (* A function invoke *)
  | IInvoke of identifier located * arglist located
  (* A speed command *)
  | ISpeed of expression located
  (* A send command *)
  | ISend of identifier located * (expression located option)
  (* A declare command *)
  | IDeclare of scoped_identifier located * type_const located * type_const list located option
(* An expression *)
and expression =
  (* A literal (const value) *)
  | ELiteral of literal located
  (* A variable  *)
  | EVar of scoped_identifier located
  (* An operation *)
  | EOperation of operation located * expression located * expression located
  (* A ternary *)
  | ECondition of expression located * expression located * expression located
  (* A function call *)
  | EFunc of identifier located * arglist located
  (* Access to a constructed type *)
  | EAccess of expression located * identifier located
  (* Cast from one type to another *)
  | ETypeCast of type_const located * expression located

and pattern =
  (* A wildcard *)
  | PWildcard
  (* A simple value *)
  | PValue of expression located
  (* A capturing value*)
  | PBinding of identifier located * expression located

and scope =
  | SGlobal
  | SLocal
  | SExtern

and literal =
  | LInt    of int32
  | LFloat  of float
  | LBool   of bool
  | LString of fstring
  | LEnum of (identifier located) * (identifier located)
  | LVec2 of (expression located) * (expression located)
  | LVec3 of (expression located) * (expression located) * (expression located)

and scoped_identifier = scope * identifier

and arglist = expression located list

and message = fstring * messageopt located list
and messageopt =
  | MsgNoRush
  | MsgNoAcknowledge

and fstring = fstringtok located list

and fstringtok =
  | StrConst of string
  | StrInline of expression located
  | StrColor of string option

and identifier =
  | Id of string

and operation =
  | OpPlus
  | OpMinus
  | OpStar
  | OpDivide
  | OpAnd
  | OpOr
  | OpEqual
  | OpNotEqual
  | OpLeq
  | OpGeq
  | OpLess
  | OpMore

and t = program
