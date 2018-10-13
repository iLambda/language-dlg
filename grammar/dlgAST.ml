(** The abstract syntax tree for hopix programs. *)

open Sexplib.Std
open Position

(** A program is a list of definitions. *)
type program = instruction located list
(* [@@deriving sexp] *)

and instruction =
  (** A label **)
  | ILabel of identifier located
  (** A goto statement **)
  | IGoto of identifier located
  (** A set variable statement **)
  | ISet of scope located * identifier located * expression located
  (** A ifnset variable statement (if not set, do) **)
  | IIfnset of scope located * identifier located * expression located
  (** A wait n seconds statement **)
  | IWait of expression located
  (** A nop instruction (do nothing) **)
  | INop
  (** A 'print message' instruction **)
  | IMessage of message located
  (** A choice **)
  | IChoice of (message located * program located) list
  (** A condition **)
  | ICondition of expression located * ((pattern located * program located) list)
  (** A speed command **)
  | ISpeed of expression located
  (** A send command **)
  | ISend of identifier located * (expression located option)

and expression =
  (** A literal (const value) **)
  | ELiteral of literal located
  (** A variable **)
  | EVar of identifier located
  (** An operation **)
  | EOperation of string located * expression located list
  (** A function call **)
  | EFunc of identifier located * arglist located

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
  | SObject

and literal =
  | LInt    of int32
  | LFloat  of float
  | LBool   of bool
  | LString of fstring
  | LEnum of (identifier located) * (identifier located)
  | LVec2 of (expression located) * (expression located)
  | LVec3 of (expression located) * (expression located) * (expression located)

and message = fstring located * messageopt located list
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

and t = program
[@@deriving sexp]
