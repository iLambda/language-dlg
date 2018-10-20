open DLG.Ast
open Type
open Error

(* A scoped identifier *)
type scoped_identifier = DLG.Ast.scoped_identifier
(* A branch of the type environment representing where we are in the program *)
type env_branch = int list
(* A declaration in the type environment *)
type env_declaration =
  | DeclareVariable of type_const
  | DeclareFunction of type_func
type env_declaration_list = (env_branch * env_declaration) list
(* A type environment representing all the bound identifiers *)
type env = (scoped_identifier, env_declaration_list) Hashtbl.t

(* Converts a declaration to its type container *)
let decl_to_type_container = function
  | DeclareVariable v -> TCValue v
  | DeclareFunction f -> TCFunc f

(* Returns true iff the base is a prefix on the branch (see tree lexicographic order) *)
let rec env_branch_is_previous base branch = match base, branch with
  | [], [] -> true
  | [], _ -> true
  | _, [] -> false
  | h1::t1, h2::t2 -> h1 == h2 && env_branch_is_previous t1 t2
(* Retuns the depth at which the two branches start to diverge *)
let rec env_branch_diverge_depth branch0 branch1 = match branch0, branch1 with
  | [], [] -> 0
  | [], _ -> 0
  | _, [] -> 0
  | h1::t1, h2::t2 -> if h1 == h2 then (1 + (env_branch_diverge_depth t1 t2)) else 0
(* Expands a branch, exploring the ith choice after it *)
let env_branch_child branch i = branch @ [i]
(* Returns the root *)
let env_branch_root () = []

(* Creates an empty type environment *)
let env_make () = ((Hashtbl.create 30) :> env)
(* Return all the declarations related to an identifier *)
let env_get_all env id = match Hashtbl.find_opt env id with
  | None -> []
  | Some decls -> decls
(* Returns an option containing the definition for the identifier in the environment at a given branch *)
(* val env_get : env -> env_branch -> scoped_identifier -> env_declaration option *)
let env_get env branch id =
  (* try find branch*)
  match Hashtbl.find_opt env id with
    (* not found. no definition *)
    | None -> None
    (* found. append *)
    | Some d ->
      (* find the most advanced declaration available from current branch *)
      let rec find_max_branch decls curbranch maxdepth acc = match decls with
        (* no declarations *)
        | [] -> acc
        (* there are declarations, try to match them *)
        | (base, decl)::tail ->
          (* if the declaration is not available from branch *)
          if not (env_branch_is_previous base curbranch) then
            (* keep crawling *)
            find_max_branch tail curbranch maxdepth acc
          (* it is avaialble *)
          else
            (* compute depth of current declaration *)
            let depth = env_branch_diverge_depth base curbranch in
            (* check if larger *)
            if maxdepth >= depth then
              (* not maximal keep crawling in declarations*)
              find_max_branch tail curbranch maxdepth acc
            else
              (* maximal. replace *)
              find_max_branch tail curbranch maxdepth (Some decl)
      (* actually find the max branch*)
      in find_max_branch d branch (-1) None

(* Checks if two given declarations are conflictual *)
let env_decl_conflictual olddecl decl = match olddecl, decl with
  (* We have two variables with the same type of declaration *)
  | DeclareVariable t0, DeclareVariable t1 -> type_is_same t0 t1
  | DeclareFunction (r0, at0), DeclareFunction (r1, at1) -> type_function_is_same (r0, at0) (r1, at1)
  (* Else, it's not compatible *)
  | _, _ -> false


(* Checks if the definition we add at this point is coherent with the rest of the env
    -> If no conflict, returns [Ok()]
    -> If conflict, returns [Error conflictualdecl] *)
(* val env_is_conflictual : env -> env_branch -> scoped_identifier -> env_declaration -> (unit, env_declaration) result *)
let env_is_conflictual env branch id decl =
  (* try find branch *)
  match env_get env branch id with
    (* no declaration found. no conflict *)
    | None -> Ok ()
    (* a decl is found previously in the branch. check if works *)
    | Some olddecl ->
      (* check if declarations are compatible and return *)
      if env_decl_conflictual olddecl decl
      then Ok()
      else Error olddecl


(* Sets the type container definition for a variable at a given branch
    -> Can raise Type_error*)
(* val env_bind : env -> env_branch -> scoped_identifier -> env_declaration -> unit *)
let env_bind env branch id decl =
  (* try find branch*)
  match Hashtbl.find_opt env id with
    (* not found, means no definition. define *)
    | None -> Hashtbl.add env id [branch, decl]
    (* found. append *)
    | Some d ->
      (* try see if there is a conflictual declaration *)
      let () = match env_is_conflictual env branch id decl with
        (* There is. raise an error *)
        | Error olddecl -> raise_type_error ReasonIncompatibleDeclaration (Some (decl_to_type_container decl)) [decl_to_type_container olddecl]
        (* There isn't. Go on *)
        | Ok _ -> ()
      in
      (* replace the definition at the right branch*)
      let rec replace_branch decls =  match decls with
        (* list of declarations is empty. add blindly*)
        | [] -> [branch, decl]
        (* crawl list, check if we found the right branch yet*)
        | (base, decl)::tail when base = branch -> (base, decl)::tail
        | _::tail -> replace_branch tail
      (* actually do it. set the new definition list *)
      in Hashtbl.add env id (replace_branch d)

(* Returns true iff the scoped identifier is extern *)
let scoped_id_is_extern = function
  | SLocal, _ -> false
  | _ -> true


(* Returns true iff the identifier was bound *)
let env_is_bound env branch id = match env_get env branch id with
  | None -> false
  | Some _ -> true

(* Try to get the definition for the identifier in the environment at a given branch
    -> If ok, returns [Ok (option decl)]
    -> If not, returns [Error errtype] ready to be decorated *)
let env_try_get env branch id =
  (* get the declaration *)
  let binding = env_get env branch id in
  (* deconstruct the result *)
  match binding with
    (* No previous declaration *)
    | None ->
      (* Check if identifier is extern*)
      if scoped_id_is_extern id then Ok (None)
      (* It's not ; this is a problem *)
      else Error (make_type_error ReasonUnbound None [])
    (* There is a previous declaration. Return it *)
    | Some _ -> Ok binding

(* Try to set the type container definition for a variable at a given branch
    -> If ok, returns [Ok ()]
    -> If not, returns [Error errtype] ready to be decorated *)
let env_try_bind env branch id decl = match env_bind env branch id decl with
  (* Error occured. *)
  | exception (Type_error payload) -> Error (payload)
  (* No error. The variable has been bound *)
  | _ -> Ok ()
