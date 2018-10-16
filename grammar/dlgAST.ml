(** The abstract syntax tree for hopix programs. *)

open Sexplib.Std
open Position

(* A constant type *)
type typeconst =
  | TInt
  | TFloat
  | TBool
  | TString
  | TEnum of string
  | TVec2
  | TVec3
  | TAll
[@@deriving sexp]

(** A program is a list of definitions. *)
type program = instruction located list
(* [@@deriving sexp] *)

and instruction =
  (** A label **)
  | ILabel of identifier located
  (** A goto statement **)
  | IGoto of identifier located
  (** A set variable statement **)
  | ISet of variable located * expression located
  (** A ifnset variable statement (if not set, do) **)
  | IIfnset of variable located * expression located
  (** A wait n seconds statement **)
  | IWait of identifier located option * expression located
  (** A nop instruction (do nothing) **)
  | INop
  (** A 'print message' instruction **)
  | IMessage of message located
  (** A choice **)
  | IChoice of (message located * program located) list
  (** A condition **)
  | ICondition of expression located * ((pattern located * program located) list)
  (** A function invoke **)
  | IInvoke of identifier located * arglist located
  (** A speed command **)
  | ISpeed of expression located
  (** A send command **)
  | ISend of identifier located * (expression located option)
  (** A declare command **)
  | IDeclare of variable located * typeconst located * typearglist option

and expression =
  (** A literal (const value) **)
  | ELiteral of literal located
  (** A variable **)
  | EVar of variable located
  (** An operation **)
  | EOperation of operation located * expression located * expression located(*expression located list*)
  (** A ternary **)
  | ECondition of expression located * expression located * expression located
  (** A function call **)
  | EFunc of identifier located * arglist located
  (** Access to a constructed type **)
  | EAccess of expression located * identifier located

and pattern =
  (** A wildcard **)
  | PWildcard
  (** A simple value **)
  | PValue of expression located
  (** A capturing value**)
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

and variable = scope * identifier

and arglist = expression located list
and typearglist = typeconst located list

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
[@@deriving sexp]
