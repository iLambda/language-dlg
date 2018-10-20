open DLG.Ast
open Error

(* A type environment representing all the bound identifiers *)
type env
(* A branch of the type environment representing where we are in the program *)
type env_branch
(* A declaration in the type environment *)
type env_declaration =
  | DeclareVariable of type_const
  | DeclareFunction of type_func
(* A list of declarations *)
type env_declaration_list
(* A scoped identifier *)
type scoped_identifier = DLG.Ast.scoped_identifier

(* Returns true iff the base is a prefix on the branch (see tree lexicographic order) *)
val env_branch_is_previous : env_branch -> env_branch -> bool
(* Retuns the depth at which the two branches start to diverge *)
val env_branch_diverge_depth : env_branch -> env_branch -> int
(* Expands a branch, exploring the ith choice after it *)
val env_branch_child : env_branch -> int -> env_branch
(* Returns the branch *)
val env_branch_root : unit -> env_branch

(* Creates an empty type environment *)
val env_make : unit -> env
(* Return all the declarations related to an identifier *)
val env_get_all : env -> scoped_identifier -> env_declaration_list
(* Checks if the definition we add at this point is coherent with the rest of the env
-> If no conflict, returns [Ok()]
-> If conflict, returns [Error conflictualdecl] *)
val env_is_conflictual : env -> env_branch -> scoped_identifier -> env_declaration -> (unit, env_declaration) result
(* Returns an option containing the definition for the identifier in the environment at a given branch *)
val env_get : env -> env_branch -> scoped_identifier -> env_declaration option
(* Sets the type container definition for a variable at a given branch
    -> Can raise Type_error*)
val env_bind : env -> env_branch -> scoped_identifier -> env_declaration -> unit
(* Returns true iff the identifier was bound *)
val env_is_bound : env -> env_branch -> scoped_identifier -> bool

(* Try to get the definition for the identifier in the environment at a given branch
    -> If ok, returns [Ok (option decl)]
    -> If not, returns [Error errtype] ready to be decorated *)
val env_try_get : env -> env_branch -> scoped_identifier -> (env_declaration option, type_error) result
(* Try to set the type container definition for a variable at a given branch
    -> If ok, returns [Ok ()]
    -> If not, returns [Error errtype] ready to be decorated *)
val env_try_bind : env -> env_branch -> scoped_identifier -> env_declaration -> (unit, type_error) result

(* Returns true iff the scoped identifier is extern *)
val scoped_id_is_extern : scoped_identifier -> bool
