open DLG.Ast
open Pcode

(* Returns the p-code of a program *)
val compile : program -> pcode
(* Build the jump table of the program *)
val add_jump_table : pcode -> pcode
