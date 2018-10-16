open Position
open DlgAST

(*
* TYPING SYSTEM TYPES
*)

(* A type container, representing an expectation of some constant type *)
type typecontainer =
  | TCActual of typeconst
  | TCExpected of typeconst option

(* A branch in the program *)
type branchid = int list
(* A type environment containing all type declarations for variables&funcs in a given branch *)
type typeenv = (variable, (branchid * typecontainer) list) Hashtbl.t

(*
*  TYPE CONSTANTS HELPERS
*)

(* Returns the type of a given literal *)
let type_of_literal = function
  | LInt _ -> TInt
  | LFloat _ -> TFloat
  | LBool _ -> TBool
  | LString _ -> TString
  | LEnum (t, _) -> begin match (Position.value t) with Id tstr -> TEnum tstr end
  | LVec2 _ -> TVec2
  | LVec3 _ -> TVec3

(* Checks if a given type is a number type *)
let type_is_number = function
  | LInt _ -> true
  | LFloat _ -> true
  | _ -> false

(* Casts the number literal to float *)
let float_of_number = function
  | LFloat f -> LFloat f
  | LInt i -> LFloat (Int32.to_float i)
  | _ -> failwith "Type could not be cast to a number"

let float_of_located_number n = float_of_number (Position.value n)

(*
  BRANCH HELPERS
*)
(* Returns true iff the base is a prefix on the branch (see tree lexicographic order) *)
let rec branch_is_previous (base : branchid) (branch : branchid) = match base, branch with
  | [], [] -> true
  | [], _ -> true
  | _, [] -> false
  | h1::t1, h2::t2 -> h1 == h2 && branch_is_previous t1 t2
(* Retuns the depth at which the two branches start to diverge *)
let rec branch_common_depth (base : branchid) (branch : branchid) = match base, branch with
  | [], [] -> 0
  | [], _ -> 0
  | _, [] -> 0
  | h1::t1, h2::t2 -> if h1 == h2 then (1 + branch_common_depth t1 t2) else 0
(* Expand a branch, exploring the ith choice after it *)
let branch_child branch i =
  branch @ [i]

(*
  TYPE ENVIRONMENT HELPER FUNCTIONS
*)
(* Returns the an option containing the type container definition for the variable at a given branch *)
let type_env_get (env : typeenv) var (b : branchid) =
  (* try find branch*)
  match Hashtbl.find_opt env var with
    (* not found. no definition *)
    | None -> None
    (* found. append *)
    | Some d ->
      (* find the most advanced declaration available from current branch *)
      let rec find_max_branch decls branch maxdepth acc = match decls with
        (* no declarations *)
        | [] -> acc
        (* there are declarations, try to match them *)
        | (base, decl)::tail ->
          (* if the declaration is not available from branch *)
          if not (branch_is_previous base branch) then
            (* keep crawling *)
            find_max_branch tail branch maxdepth acc
          (* it is avaialble *)
          else
            (* compute depth of current declaration *)
            let depth = branch_common_depth base branch in
            (* check if larger *)
            if maxdepth >= depth then
              (* not maximal keep crawling in declarations*)
              find_max_branch tail branch maxdepth acc
            else
              (* maximal. replace *)
              find_max_branch tail branch maxdepth (Some decl)
      (* actually find the max branch*)
      in find_max_branch d b (-1) None

(* Sets the type container definition for a variable at a given branch *)
let type_env_bind (env : typeenv) var (branch : branchid) tc =
  (* try find branch*)
  match Hashtbl.find_opt env var with
    (* not found *)
    | None -> Hashtbl.add env var [branch, tc]
    (* found. append *)
    | Some d ->
      (* replace the definition at the right branch*)
      let rec replace_branch decls =  match decls with
        (* list of declarations is empty. add blindly*)
        | [] -> [branch, tc]
        (* crawl list *)
        | (base, decl)::tail -> if base = branch then (base, tc)::tail
                                else replace_branch tail
      (* actually do it *)
      in Hashtbl.add env var (replace_branch d)

(*
  TYPE CHECKING METHODS
*)
type type_error =
{
  position: Position.position
}

(*
* TYPE CONTAINERS HELPERS
*)
(* Returns true if the two type constants are the same *)
let type_is_same t0 t1 = match t0, t1 with
  | TAll, t -> true
  | t, TAll -> true
  | _, _ -> t0 = t1

(* Returns an option representing the type constant assumed to be produced by the type container *)
let type_assumed_from tc = match tc with
  | TCActual t -> Some t
  | TCExpected expectation -> begin match expectation with
    | None -> None
    | Some t -> Some t
    end

(* Returns true iff the type container represents a type constant t0 *)
let type_container_is_type tc t0 = match tc with
  | TCActual t -> type_is_same t t0
  | TCExpected expectation -> begin match expectation with
    | None -> true
    | Some t -> type_is_same t t0
    end

(* Returns true iff the type container represents a number *)
let type_container_is_number tc =
  (type_container_is_type tc TInt)
  || (type_container_is_type tc TFloat)

(* Returns true iff the type containers are compatible *)
let type_container_is_same tc1 tc2 = match tc1, tc2 with
  (* actual types. check equality *)
  | TCActual t0, TCActual t1 -> type_is_same t0 t1
  | TCActual t0, TCExpected (Some t1) -> type_is_same t0 t1
  | TCExpected (Some t0), TCActual t1 -> type_is_same t0 t1
  | TCExpected (Some t0), TCExpected (Some t1) -> type_is_same t0 t1
  (* in the other cases, we have a null expectation somewhere. give in*)
  | _ -> true

(* Returns the type container carrying the most expectation info about tc1(t) AND tc2(t) representing t *)
let least_type_container tc1 tc2 =
  (* if they are not representing the same type *)
  if not (type_container_is_same tc1 tc2) then None
  (* they represent the same type. return the least one*)
  else match tc1, tc2 with
    (* actual types. return an actual type *)
    | TCActual t, TCActual _ -> Some (TCActual t)
    (* if we have a typed expectation or an actual type, return an expectation *)
    | TCActual t, TCExpected _ -> Some (TCExpected (Some t))
    | TCExpected _, TCActual t -> Some (TCExpected (Some t))
    | TCExpected (Some t), _ -> Some (TCExpected (Some t))
    | _, TCExpected (Some t) -> Some (TCExpected (Some t))

    (* in the other case, we have two null expectations. can't say more*)
    | TCExpected None, TCExpected None -> Some (TCExpected None)

(* Returns the type container carrying the most expectation info about tc1 AND tc2, but representing a different const type t *)
let least_type_container_of tc1 tc2 t = match tc1, tc2 with
    (* actual types. return an actual type *)
    | TCActual _, TCActual _ -> TCActual t
    (* if we have a typed expectation or an actual type, return an expectation *)
    | TCActual _, TCExpected _ -> TCExpected (Some t)
    | TCExpected _, TCActual _ -> TCExpected (Some t)
    | TCExpected (Some _), _ -> TCExpected (Some t)
    | _, TCExpected (Some _) -> TCExpected (Some t)

    (* in the other case, we have two null expectations. can't say more*)
    | TCExpected None, TCExpected None -> TCExpected None

(*
* TYPE CHECKER METHODS
*)

(* Returns the type of an expression given a branch *)
let rec expr_type expr (runtime:typeenv) (branch:branchid) =
  (* shorthand to unlocate *)
  let unlocate x = Position.value x in
  (* match subexpression *)
  let rec subexpr_type = function

    (** A literal has an actual type **)
    | ELiteral lit -> begin match unlocate lit with
      (* a vector. check the constructor *)
      | LVec2 (x, y) ->
        (* compute types of x and y *)
        let tcx = subexpr_type (unlocate x) in
        let tcy = subexpr_type (unlocate y) in
        (* check if args are numbers *)
        if not (type_container_is_number tcx)
        then failwith "Type error : vec2.x must be a number";
        if not (type_container_is_number tcy)
        then failwith "Type error : vec2.y must be a number";
        (* return *)
        TCActual TVec2
      (* a vector. check the constructor *)
      | LVec3 (x, y, z) ->
        (* compute types of x and y *)
        let tcx = subexpr_type (unlocate x) in
        let tcy = subexpr_type (unlocate y) in
        let tcz = subexpr_type (unlocate z) in
        (* check if args are numbers *)
        if not (type_container_is_number tcx)
        then failwith "Type error : vec3.y must be a number";
        if not (type_container_is_number tcy)
        then failwith "Type error : vec3.y must be a number";
        if not (type_container_is_number tcz)
        then failwith "Type error : vec3.z must be a number";
        (* return *)
        TCActual TVec3
      (* a formatted string *)
      | LString fstr ->
        (* verify if underlying fstring if ok *)
        if check_fstring_type runtime branch fstr then TCActual TString
        else failwith "Type error : fstring has unsound inline tokens"
      (* a literal without a constuctor. this is straightforward *)
      | _ -> TCActual (type_of_literal (unlocate lit))
    end

    (** A variable **)
    | EVar var ->
      (* check if it is in type environment *)
      begin match type_env_get runtime (unlocate var) branch with
        (* it is not. check its scope *)
        | None -> begin match (unlocate var) with
            (* local it must be defined before *)
            | SLocal, id -> failwith ("Type error : local variable was not defined before use " ^ (match id with Id s -> s))
            (* not local. can't assume anything *)
            | _ -> TCExpected None
          end
        (* it's been found. use the saved type container *)
        | Some tc -> tc
      end

    (** An operation **)
    | EOperation (op, lhs, rhs) ->
      (* Returns an identity tuple matching the signature t, t -> t*)
      let make_id_tuple t = (t, t, t) in
      (* Returns the resolved return type given arguments *)
      let rec resolve_args_type l tcl tcr = begin match l with
        (* No rule to match lhs and rhs. No return *)
        | [] -> None
        (* Try to match one rule *)
        | (l, r, ret)::t ->
          (* If rules matches *)
          if (type_container_is_type tcl l) && (type_container_is_type tcr r)
          (* Return the output type, with as less assumption as possible *)
          then Some (least_type_container_of tcl tcr ret)
          (* Did not match. Try the next rule *)
          else resolve_args_type t tcl tcr
        end in
        let oplist = begin match unlocate op with
          | OpPlus -> [
              (* numbers *)
              make_id_tuple TInt;
              TInt, TFloat, TFloat;
              TFloat, TInt, TFloat;
              make_id_tuple TFloat;
              (* string concat *)
              make_id_tuple TString;
              (* vector *)
              make_id_tuple TVec2;
              make_id_tuple TVec3;
            ]
          | OpMinus -> [
              (* numbers *)
              make_id_tuple TInt;
              TInt, TFloat, TFloat;
              TFloat, TInt, TFloat;
              make_id_tuple TFloat;
              (* vectors *)
              make_id_tuple TVec2;
              make_id_tuple TVec3;
            ]
          | OpStar -> [
              (* number *)
              make_id_tuple TInt;
              TInt, TFloat, TFloat;
              TFloat, TInt, TFloat;
              make_id_tuple TFloat;
              (* string *)
              TInt, TString, TString;
              TString, TInt, TString;
              (* vector 3 *)
              TVec2, TInt, TVec2;
              TInt, TVec2, TVec2;
              TVec2, TFloat, TVec2;
              TFloat, TVec2, TVec2;
              (* vector 3 *)
              TVec3, TInt, TVec3;
              TInt, TVec3, TVec3;
              TVec3, TFloat, TVec3;
              TFloat, TVec3, TVec3;
            ]
          | OpDivide -> [
              (* number *)
              make_id_tuple TInt;
              TInt, TFloat, TFloat;
              TFloat, TInt, TFloat;
              make_id_tuple TFloat;
              (* vector 3 *)
              TVec2, TInt, TVec2;
              TInt, TVec2, TVec2;
              TVec2, TFloat, TVec2;
              TFloat, TVec2, TVec2;
              (* vector 3 *)
              TVec3, TInt, TVec3;
              TInt, TVec3, TVec3;
              TVec3, TFloat, TVec3;
              TFloat, TVec3, TVec3;
            ]
          | OpAnd -> [ make_id_tuple TBool ]
          | OpOr -> [ make_id_tuple TBool ]
          | OpEqual -> [ TAll, TAll, TBool ]
          | OpNotEqual -> [ TAll, TAll, TBool ]
          | OpLeq -> [ TAll, TAll, TBool ]
          | OpGeq -> [ TAll, TAll, TBool ]
          | OpLess -> [ TAll, TAll, TBool ]
          | OpMore -> [ TAll, TAll, TBool ]
        end in begin match resolve_args_type oplist (subexpr_type (unlocate lhs)) (subexpr_type (unlocate rhs)) with
          | None -> failwith "Type error : operator does not support given type"
          | Some tc -> tc
        end

    (** A ternary **)
    | ECondition (cond, thendo, elsedo) ->
      (* check if condition type is bool *)
      if not (type_container_is_type (subexpr_type (unlocate cond)) TBool)
      then failwith "Type error : ternary condition needs bool";
      (* compute type containers of branches *)
      let then_tc = (subexpr_type (unlocate thendo))
      and else_tc = (subexpr_type (unlocate elsedo)) in
      (* compute least type container of both *)
      begin match least_type_container then_tc else_tc with
        | None -> failwith "Type error : ternary branches must be of same type"
        | Some t -> t
      end
    (** A function call has no expected type since it is always extern **)
    | EFunc (_, _) -> TCExpected None
    (* Access to a constructed type *)
    | EAccess (constr, id) ->
      (* get the type of the constructed type *)
      let constr_tc = (subexpr_type (unlocate constr)) in
      (* get the string represented by id*)
      let accessed = match (unlocate id) with Id s -> s in
      (* match it *)
      begin match type_assumed_from constr_tc with
        (* No assumption. Work anyways, no info on type*)
        | None -> TCExpected None
        (* Assumption exists. Match the type const it assumes *)
        | Some t -> begin match t with
          (* A vector *)
          | TVec2 ->
            (* check if accessed is x or y. else , throw*)
            if List.mem accessed ["x"; "y"] then least_type_container_of constr_tc constr_tc TFloat
            else failwith ("Type error : accessed property '" ^ accessed ^ "' does not belong to type")
          (* A vector *)
          | TVec3 ->
            (* check if accessed is x or y. else , throw*)
            if List.mem accessed ["x"; "y"; "z"] then least_type_container_of constr_tc constr_tc TFloat
            else failwith ("Type error : accessed property '" ^ accessed ^ "' does not belong to type")
          (* Anything else fails *)
          | _ -> failwith "Type error : accessed type is not a constructed type "

        end
      end

  in subexpr_type expr

(* Checks the type of a fstring *)
and check_fstring_type (env:typeenv) branch =
  (* shorthand to unlocate *)
  let unlocate x = Position.value x in function
  (* Empty fstring. Checks out *)
  | [] -> true
  (* Crawl through tokens *)
  | tok::tail -> begin match unlocate tok with
    (* An expression *)
    | StrInline expr ->
      (* compute type container of subexpression. for the moment, do nothing with it*)
      let () = ignore (expr_type (unlocate expr) env branch)
      (* check next tokens *)
      in check_fstring_type env branch tail
    (* We only check expressions. Pass *)
    | _ -> check_fstring_type env branch tail
  end

(* Checks the type of a program *)
let rec check_program_type p =
  (* the type context*)
  let mainenv = ((Hashtbl.create 30) :> typeenv) in
  (* checks the type of a subprogram*)
  let rec check_subprogram_type program env branch =
    (* shorthand to unlocate *)
    let unlocate x = Position.value x in
    (* check instruction *)
    let check_instruction_type instr = match instr with
      (* an ifnset expr *)
      | IIfnset (var, expr) ->
        (* check if variable was already bound in the typeenvflat *)
        begin match type_env_get env (unlocate var) branch with
          (* not bound. do bind. this line is valid *)
          | None -> type_env_bind env (unlocate var) branch (expr_type (unlocate expr) env branch); true
          (* bound. check if valid *)
          | Some tc ->
            let isvalid = tc = (expr_type (unlocate expr) env branch) in
            if not isvalid then failwith "Type error: variable was already set with a different type";
            isvalid
        end

      (* a set expression *)
      | ISet (var, expr) ->
        (* check if variable was already bound in the typeenvflat *)
        begin match type_env_get env (unlocate var) branch with
          (* not bound. do bind. this line is valid *)
          | None -> type_env_bind env (unlocate var) branch (expr_type (unlocate expr) env branch); true
          (* bound. check if valid *)
          | Some tc ->
            let isvalid = tc = (expr_type (unlocate expr) env branch) in
            if not isvalid then failwith "Type error: variable was already set with a different type";
            isvalid
        end

      (* a wait expression *)
      | IWait (_, expr) ->
        (* evaluate type of expression *)
        let tc = expr_type (unlocate expr) env branch in
        (* check if expression is a number type *)
        let isvalid = type_container_is_number tc in
        if not isvalid then failwith "Type error: speed needs a number";
        isvalid

      (* a send instruction. since it has external effect, we don't expect nothing *)
      | ISend (_, Some expr) -> true

      (* a speed instruction *)
      | ISpeed expr ->
        (* evaluate type of expression *)
        let tc = expr_type (unlocate expr) env branch in
        (* check if expression is a number type *)
        let isvalid = type_container_is_number tc in
        if not isvalid then failwith "Type error: speed needs a number";
        isvalid

      (* a choice possibility*)
      | IChoice choices ->
        (* go through choices *)
        let rec check_choices_type i = function
          (* empty. it is valid *)
          | [] -> true
          (* check type of all expressions *)
          | (msg, prog)::tail ->
            (* check the message *)
            let fstr, _ = unlocate msg in
            (* verify if underlying fstring is ok *)
            (if check_fstring_type env branch fstr
            then true
            else failwith "Type error : message has unsound inline tokens")
            (* check the subprogram *)
            && (check_subprogram_type (unlocate prog) env (branch_child branch i))
            (* check the next choice *)
            && (check_choices_type (i+1) tail)
        (* actually check *)
        in check_choices_type 0 choices

      (* a condition *)
      | ICondition (expr, branches) ->
        (* go through pattern *)
        let rec check_pattern_type etype b = function
          (* Wildcard. Always ok *)
          | PWildcard -> true
          (* A value. Okay iff the same type as etype *)
          | PValue e ->
            (* get type equality between e and etype *)
            let sametype = type_container_is_same etype (expr_type (unlocate e) env branch) in
            (* check if passed *)
            if not sametype then failwith "Type error : value pattern must have same type as the matched expression"
            else sametype
          (* A binding expression *)
          | PBinding (id, e) ->
            (* get a type declaration for identifier (always local) *)
            let idtype = type_env_get env (SLocal, unlocate id) branch in
            (* check if variable is already bound *)
            if idtype <> None then failwith "Type error : identifier is already bound";
            (* it wasn't. bind it *)
            type_env_bind env (SLocal, unlocate id) b etype;
            (* check if expr is boolean*)
            if not (type_container_is_type (expr_type (unlocate e) env b) TBool)
            (* it's not *)
            then failwith "Type error : binding pattern condition must be a boolean"
            (* it is *)
            else true
        in
        (* go through branches *)
        let rec check_branches_type etype i = function
          (* empty. it is valid *)
          | [] -> true
          (* check type of all expressions *)
          | (patt, prog)::tail ->
            (* check the pattern *)
            (check_pattern_type etype (branch_child branch i) (unlocate patt))
            (* check the subprogram *)
            && (check_subprogram_type (unlocate prog) env (branch_child branch i))
            (* check the next choice *)
            && (check_branches_type etype (i+1) tail)
        (* actually check *)
        in
        (* evaluate type of condition *)
        let etype = (expr_type (unlocate expr) env branch)
        (* check the type of the branches *)
        in check_branches_type etype 0 branches

      (* a message *)
      | IMessage msg ->
        (* get the formatted string *)
        let fstr, _ = unlocate msg in
          (* verify if underlying fstring is ok *)
          if check_fstring_type env branch fstr then true
          else failwith "Type error : message has unsound inline tokens"

      (* A declare type *)
      | IDeclare (var, ret, args) ->
        (* check if args is none*)
        begin match args with
        (* it's none. we got a variable*)
        | None ->
        (* check if variable was already bound in the typeenv where we are *)
        (** BIND GLOBALLY OR LOCALLY ? branch or [] ? **)
          begin match type_env_get env (unlocate var) branch with
            (* not bound. do bind. this line is valid *)
            | None -> type_env_bind env (unlocate var) branch (TCActual (unlocate ret)); true
            (* bound. check if valid *)
            | Some tc ->
              let isvalid = type_container_is_type tc (unlocate ret) in
              if not isvalid then failwith "Type error: variable was declared two times with different types";
              (* else throw warning *)
              isvalid
          end
        (* it's not none. we got a func*)
        | Some _ -> failwith "UNIMPLEMENTED"
        end

      | _ -> true
    in
    (* is the program valid ? *)
    let is_valid = ref true in
    (* the current program insctruction and the rest of the program *)
    let program_instr = ref INop in
    let program_left = ref program in
    while !program_left <> [] do
      (* get the current instruction *)
      begin match !program_left with
        | [] -> failwith "Can't be empty."
        | i::rest -> program_instr := (unlocate i); program_left := rest;
      end;
      (* check if instruction valid *)
      is_valid := ((!is_valid) && (check_instruction_type !program_instr))
    done;
    (* return *)
    !is_valid
  in check_subprogram_type p mainenv []
