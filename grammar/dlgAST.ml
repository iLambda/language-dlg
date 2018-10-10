(** The abstract syntax tree for hopix programs. *)

open Sexplib.Std
open Position

(** A program is a list of definitions. *)
type program = instruction located list

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
  | IWait of literal located
  (** A nop instruction (do nothing) **)
  | INop
  (** A 'print message' instruction **)
  | IMessage of string
  (** A choice **)
  | IChoice of (string located * program located) list

and expression =
  (** A literal (const value) **)
  | ELiteral of literal located

and scope =
  | SGlobal
  | SLocal
  | SObject

and literal =
  | LInt    of int32
  | LFloat  of float
  | LBool   of bool

and identifier =
  | Id of string

and t = program

[@@deriving sexp]
