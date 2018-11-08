open Data

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
