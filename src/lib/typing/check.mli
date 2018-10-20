open DLG.Ast
open Env

(* Returns the type of an expression, given an environment and a branch
  -> Can raise Type_error *)
val get_expr_type : env -> env_branch -> expression -> type_const
(* Returns the type of an arglist *)
val get_arglist_type : env -> env_branch -> arglist -> type_const list

(* Checks the type of a formatted string
  -> Can raise type Type_error *)
val check_fstring_type : env -> env_branch -> fstring -> unit
(* Checks the type of an instruction
  -> Can raise Type_error *)
val check_instruction_type : env -> env_branch -> instruction -> unit
(* Checks the type subprogram
  -> Can raise Type_error *)
val check_subprogram_type : env -> env_branch -> program -> unit
(* Checks the type of a program
  -> Can raise Type_error *)
val check_program_type : program -> unit
