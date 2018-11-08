open Data

type alu_error_reason =
  | AluCallInvalidArgType of int * value * value
  | AluCallInvalidArgNumber of int * int
  | AluCallNoSuchFunction of string

type alu_error = {
  reason: alu_error_reason;
}

exception Alu_error of alu_error


(* An operation *)
type operation =
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

(* Compute an operation *)
val alu_compute : value -> operation -> value -> value
(* Cast a value *)
val alu_copy_type : value -> value -> value
(* Apply a function from the VM's library *)
val alu_call : string -> value list -> value
