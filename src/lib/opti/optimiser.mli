open DLG.Ast

(* Raised when there was a failure in optimising something *)
exception Optimisation_failure
(* Raised in order to take a shortcut a always used branch in the optimisation process *)
exception Optimisation_always_used_branch of program
(* Raised when a wildcard pattern is matched so it cuts all the branches after *)
exception Optimisation_unused_branches_after of pattern

(* Optimizes a type-correct program. WARNING: after optimisation, locations are not valid anymore (unknown_pos) *)
val optimise_program : program -> program
(* Optimizes a type-correct instruction *)
val optimise_instr : instruction -> instruction list
(* Optimizes a type-correct expression *)
val optimise_expr : expression -> expression
(* Optimizes a type-correct fstring *)
val optimise_fstring : fstring -> fstring

(* Optimizes a computation between literals *)
val optimise_operation : literal -> operation -> literal -> literal
(* Optimizes a computation between literals, but the evaluation of the lhs and lhs as literals can be carried after *)
val optimise_operation_lazy : expression -> operation -> expression -> literal
