open Position
open DlgAST
open Lexing

(*
* TYPING SYSTEM TYPES
*)

(* A type container, representing an expectation of some constant type *)
type typecontainer =
  | TCActual of typeconst
  | TCExpected of typeconst option
(* A type declaration *)
type typedeclaration =
  | TDVariable of typecontainer
  | TDFunction of typecontainer * (typecontainer list)

(* A branch in the program *)
type branchid = int list
(* A type environment containing all type declarations for variables&funcs in a given branch *)
type typeenv = (variable, (branchid * typedeclaration) list) Hashtbl.t

(*
*  TYPE CONSTANTS HELPERS
*)

(* Returns the type of argument list *)
let rec type_container_list_of_args = function
  | [] -> []
  | h::tail -> (TCActual (Position.value h))::(type_container_list_of_args tail)

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

(* Check if a list of type containers represent a list of types *)
let rec type_container_list_is_types tclist tlist = match tclist, tlist with
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | tc1::r1, t1::r2 -> (type_container_is_type tc1 t1) && (type_container_list_is_types r1 r2)

(* Check if a list of type containers represent a list of types *)
let rec type_container_list_same tclist1 tclist2 = match tclist1, tclist2 with
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | tc1::r1, tc2::r2 -> (type_container_is_same tc1 tc2) && (type_container_list_same r1 r2)


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
  TYPE CHECKING ERROR HELPERS
*)
(* Returns a string representing a given type t*)
let string_of_type t = match t with
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TEnum s -> "enum(" ^ s ^ ")"
  | TVec2 -> "vec2"
  | TVec3 -> "vec3"
  | TAll -> "'t"
  | TVoid -> "void"

(* Returns a string representing the underlying type in a type container*)
let string_of_type_container tc = match (type_assumed_from tc) with
  (* We never show no-assumption types to the user *)
  | None -> assert false
  (* Return the matched type *)
  | Some t -> string_of_type t

let rec string_of_error_type t =
  let rec string_of_args a = match a with
    | [] -> "void"
    | [t] -> string_of_type t
    | h::t -> (string_of_type h) ^ " * " ^ (string_of_args t)
  in match t with
    | [] -> ""
    | [a] -> string_of_args a
    | h::t -> (string_of_args h) ^ " or " ^ (string_of_error_type t)


(* The payload of a type error*)
type type_error =
{
  position: Position.position;
  reason: string;
  expected: typeconst list list;
  given: typeconst list list;
}
(* An type error exception*)
exception Type_error of type_error

(*Print a type error *)
let print_type_error_at file err =
  (* deconstruct error types *)
  let pos = err.position in
  let startpos = pos.start_p in
  let endpos = pos.end_p in

  let at_line p = p.pos_lnum in
  let line_pos p = p.pos_cnum - p.pos_bol in
  let get_line file p =
    let pos_start_line = p.pos_bol in
      (* set at beginning of line *)
      seek_in file pos_start_line;
      (* read line *)
      input_line file
  in let underline line start e =
    if start == e then line, "", ""
    else  (String.sub line 0 start), (String.sub line start (e-start)), (String.sub line e ((String.length line) - e))
  in let header =
    "Type error in line " ^ (string_of_int (at_line startpos)) ^
    ", characters " ^ (string_of_int (line_pos startpos)) ^ ":" ^  (string_of_int (line_pos endpos)) ^
    " : " ^ err.reason in
  let m_s, m_u, m_e = underline (get_line file startpos) (line_pos startpos) (line_pos endpos) in
  let expectedt = string_of_error_type err.expected in
  let givent = string_of_error_type err.given in
  let expected = if expectedt <> "" then ("\texpected : " ^ expectedt ^ "\n") else "" in
  let given = if givent <> "" then ("\tgiven : " ^ givent ^ "\n") else "" in

  ANSITerminal.print_string [] ("\n" ^ header ^ "\n" ^ m_s);
  ANSITerminal.print_string [ANSITerminal.red; ANSITerminal.Underlined] m_u;
  ANSITerminal.print_string [] (m_e ^ "\n" ^ expected ^ given)

(* Throw type error with no type info *)
let type_error_at msg tok =
  (* construct the data *)
  let payload = {
    position = (Position.position tok);
    reason = msg;
    expected = [];
    given = [];
  } in raise (Type_error payload)

(* Throw error : tc is not the expected type t*)
let type_error_at_type msg tok tcgiven texp =
  (* get the given type *)
  let tgiven = match type_assumed_from tcgiven with
    | None -> []
    | Some t -> [[t]]
  in
  (* construct the data *)
  let payload = {
    position = (Position.position tok);
    reason = msg;
    expected = [[texp]];
    given = tgiven;
  } in raise (Type_error payload)

(* Throw error : tc is not the expected types t*)
let type_error_at_types msg tok tcgiven texps =
  (* get the given type *)
  let tgiven = match type_assumed_from tcgiven with
    | None -> []
    | Some t -> [[t]]
  in
  (* construct the data *)
  let payload = {
    position = (Position.position tok);
    reason = msg;
    expected = List.map (fun t -> [t]) texps;
    given = tgiven;
  } in raise (Type_error payload)

(* Throw error : tc is not the same type tc*)
let type_error_at_type_container msg tok tcgiven tcexp =
  (* get the given types *)
  let tgiven = match type_assumed_from tcgiven with
    | None -> []
    | Some t -> [[t]]
  in let texp = match type_assumed_from tcexp with
    | None -> []
    | Some t -> [[t]]
  in
  (* construct the data *)
  let payload = {
    position = (Position.position tok);
    reason = msg;
    expected = texp;
    given = tgiven;
  } in raise (Type_error payload)


(* Throw error : arglist type container does not represent type list*)
let type_error_at_args msg tok argsgiven argsexpected =
  let args = List.map (fun tc -> match type_assumed_from tc with | None -> assert false | Some t -> t) argsgiven in
  (* construct the data *)
  let payload = {
    position = (Position.position tok);
    reason = msg;
    expected = argsexpected;
    given = [args];
  } in raise (Type_error payload)

(* Throw error : tc is not the same type tc*)
let type_error_at_args_container msg tok argsgiven argsexpected =
  let args = List.map (fun tc -> match type_assumed_from tc with | None -> assert false | Some t -> t) argsgiven in
  let argse = List.map (fun tc -> match type_assumed_from tc with | None -> assert false | Some t -> t) argsexpected in
  (* construct the data *)
  let payload = {
    position = (Position.position tok);
    reason = msg;
    expected = [argse];
    given = [args];
  } in raise (Type_error payload)


(*
* TYPE CHECKER METHODS
*)

(* Returns the type of an expression given a branch *)
let rec expr_type expr (env:typeenv) (branch:branchid) =
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
        then type_error_at_types "vec2.x must be a number" x tcx [TFloat; TInt];
        if not (type_container_is_number tcy)
        then type_error_at_types "vec2.y must be a number" y tcy [TFloat; TInt];
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
        then type_error_at_types "vec3.x must be a number" x tcx [TFloat; TInt];
        if not (type_container_is_number tcy)
        then type_error_at_types "vec3.y must be a number" y tcy [TFloat; TInt];
        if not (type_container_is_number tcz)
        then type_error_at_types "vec3.z must be a number" z tcz [TFloat; TInt];
        (* return *)
        TCActual TVec3
      (* a formatted string *)
      | LString fstr ->
        (* verify if underlying fstring if ok *)
        if check_fstring_type env branch fstr then TCActual TString
        (* we never are here. we always throw in a specific token*)
        else assert false
      (* a literal without a constuctor. this is straightforward *)
      | _ -> TCActual (type_of_literal (unlocate lit))
    end

    (** A variable **)
    | EVar var ->
      (* check if it is in type environment *)
      begin match type_env_get env (unlocate var) branch with
        (* it is not. check its scope *)
        | None -> begin match (unlocate var) with
            (* local it must be defined before *)
            | SLocal, id -> type_error_at "local variable was not defined before use " var
            (* not local nor explicitely typed. can't assume anything *)
            | _ -> TCExpected None
          end
        (* it's been found. use the saved type container if the declaration one of a variable *)
        | Some td -> begin match td with
          | TDVariable c -> c
          | TDFunction _ -> type_error_at "the identifier is a function and must be called with (..)" var
        end
      end

    (** An operation **)
    (** TODO : TYPE INFERENCE **)
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
          | OpLeq -> [
              TInt, TInt, TBool;
              TFloat, TInt, TBool;
              TInt, TFloat, TBool;
              TFloat, TFloat, TBool
            ]
          | OpGeq -> [
              TInt, TInt, TBool;
              TFloat, TInt, TBool;
              TInt, TFloat, TBool;
              TFloat, TFloat, TBool
            ]
          | OpLess -> [
              TInt, TInt, TBool;
              TFloat, TInt, TBool;
              TInt, TFloat, TBool;
              TFloat, TFloat, TBool
            ]
          | OpMore -> [
              TInt, TInt, TBool;
              TFloat, TInt, TBool;
              TInt, TFloat, TBool;
              TFloat, TFloat, TBool
            ]
        end in
        let tclhs = (subexpr_type (unlocate lhs)) in
        let tcrhs = (subexpr_type (unlocate rhs)) in
        begin match resolve_args_type oplist tclhs tcrhs with
          | None -> type_error_at_args "operator does not support given types" op [tclhs; tcrhs] []
          | Some tc -> tc
        end

    (** A ternary **)
    (** TODO : TYPE INFERENCE **)
    | ECondition (cond, thendo, elsedo) ->
      (* check if condition type is bool *)
      let tccond = subexpr_type (unlocate cond) in
      if not (type_container_is_type tccond TBool)
      then type_error_at_type "ternary condition needs bool" cond tccond TBool;
      (* compute type containers of branches *)
      let then_tc = (subexpr_type (unlocate thendo))
      and else_tc = (subexpr_type (unlocate elsedo)) in
      (* compute least type container of both *)
      begin match least_type_container then_tc else_tc with
        | None -> type_error_at_type_container "ternary branches must be of same type" elsedo then_tc else_tc;
        | Some t -> t
      end
    (** A function call has no expected type (because extern), except if it was bound before **)
    | EFunc (id, arglist) -> (*TCExpected None*)
        let var = (SExtern, (unlocate id)) in
        begin match type_env_get env var branch with
          (* not bound. hence, there is no assumption *)
          | None -> TCExpected None
          (* bound. check if valid *)
          | Some td -> begin match td with
            (* Already bound to function. Type error*)
            | TDVariable _ -> type_error_at "identifier has been defined as a variable" id
            | TDFunction (tcret, tcargs) ->
              (* check if registered type declaration is the same as arglist*)
              let targlist = (arglist_type (unlocate arglist) env branch) in
              let argsisvalid = type_container_list_same tcargs targlist in
              (* check if type checking worked *)
              if not argsisvalid
              then type_error_at_args_container "function was declared two times with different arguments" id targlist tcargs;
              (* else throw warning : declared twice, redundant *)
              (* check if return type is void *)
              if type_container_is_type tcret TVoid then
              type_error_at "a void returning function cannot be used in an expression (can't produce a type void)" id;
              tcret
          end
        end

    (* Access to a constructed type *)
    (** TODO : TYPE INFERENCE **)
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
            else type_error_at ("accessed property '" ^ accessed ^ "' does not belong to type") id
          (* A vector *)
          | TVec3 ->
            (* check if accessed is x or y. else , throw*)
            if List.mem accessed ["x"; "y"; "z"] then least_type_container_of constr_tc constr_tc TFloat
            else type_error_at ("accessed property '" ^ accessed ^ "' does not belong to type") id
          (* Anything else fails *)
          | _ -> type_error_at "accessed type is not a constructed type " constr

        end
      end

  in subexpr_type expr

(* Returns the type of an arglist *)
and arglist_type arglist (env:typeenv) (branch:branchid) =
  (* shorthand to unlocate *)
  let unlocate x = Position.value x in
  (* deconstruct *)
  match arglist with
    (* void function *)
    | [] -> []
    (* an argument. evaluate and then go to other *)
    | e::t -> (expr_type (unlocate e) env branch)::(arglist_type t env branch)

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
          | None -> type_env_bind env (unlocate var) branch (TDVariable (expr_type (unlocate expr) env branch)); true
          (* bound. check if valid *)
          | Some tc ->
            let expectedtype = match tc with
              | TDVariable c -> c
              | TDFunction _ -> type_error_at "the identifier can't be a variable; it was already defined to be a function" var
            in let curtc = (expr_type (unlocate expr) env branch)
            in let isvalid = expectedtype = curtc in
            if not isvalid then type_error_at_type_container "variable was already set with a different type" expr curtc expectedtype;
            isvalid
        end

      (* a set expression *)
      | ISet (var, expr) ->
        (* check if variable was already bound in the typeenvflat *)
        begin match type_env_get env (unlocate var) branch with
          (* not bound. do bind. this line is valid *)
          (* TODO : if using a global variable, use assumption or actual type ?*)
          | None -> type_env_bind env (unlocate var) branch (TDVariable (expr_type (unlocate expr) env branch)); true
          (* bound. check if valid *)
          | Some tc ->
            let expectedtype = match tc with
              | TDVariable c -> c
              | TDFunction _ -> type_error_at "the identifier can't be a variable; it was already defined to be a function" var
            in let curtc = (expr_type (unlocate expr) env branch)
            in let isvalid = expectedtype = curtc in
            if not isvalid then type_error_at_type_container "variable was already set with a different type" expr curtc expectedtype;
            isvalid
        end

      (* a wait expression *)
      | IWait (_, expr) ->
        (* evaluate type of expression *)
        let tc = expr_type (unlocate expr) env branch in
        (* check if expression is a number type *)
        let isvalid = type_container_is_number tc in
        if not isvalid then type_error_at_types "speed needs a number" expr tc [TFloat; TInt];
        isvalid

      (* a send instruction. since it has external effect, we don't expect nothing *)
      | ISend (_, Some expr) -> true

      (* a speed instruction *)
      | ISpeed expr ->
        (* evaluate type of expression *)
        let tc = expr_type (unlocate expr) env branch in
        (* check if expression is a number type *)
        let isvalid = type_container_is_number tc in
        if not isvalid then type_error_at_types "speed needs a number" expr tc [TFloat; TInt];
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
            else assert false) (* we can never be here, errors will be thrown by check_fstring_type *)
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
            let curetc = (expr_type (unlocate e) env branch) in
            let sametype = type_container_is_same etype curetc in
            (* check if passed *)
            if not sametype
            then type_error_at_type_container "value pattern must have same type as the matched expression" e curetc etype
            else sametype
          (* A binding expression *)
          | PBinding (id, e) ->
            (* get a type declaration for identifier (always local) *)
            let idtype = type_env_get env (SLocal, unlocate id) branch in
            (* check if variable is already bound *)
            if idtype <> None then type_error_at "identifier is already bound" id;
            (* it wasn't. bind it *)
            type_env_bind env (SLocal, unlocate id) b (TDVariable etype);
            (* check if expr is boolean*)
            let tccond = (expr_type (unlocate e) env b) in
            if not (type_container_is_type tccond TBool)
            (* it's not *)
            then type_error_at_type "binding pattern condition must be a boolean" e tccond TBool
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
          else assert false (* errors are thrown by check_fstring_type *)

      (* A declare type *)
      | IDeclare (var, ret, a) ->
        (* check args is none*)
        begin match a with
          (* it's none. we got a variable*)
          | None ->
            (* check if variable was already bound in the typeenv where we are *)
            (** BIND GLOBALLY OR LOCALLY ? branch or [] ? **)
            begin match type_env_get env (unlocate var) branch with
              (* not bound. do bind. this line is valid *)
              | None -> type_env_bind env (unlocate var) branch (TDVariable (TCActual (unlocate ret))); true
              (* bound. check if valid *)
              | Some td -> begin match td with
                (* Already bound to function. Type error*)
                | TDFunction _ -> type_error_at "the identifier can't be a variable; it was already defined to be a function" var
                | TDVariable tc ->
                  let isvalid = type_container_is_type tc (unlocate ret) in
                  if not isvalid then type_error_at_type "variable was declared two times with different types" var tc (unlocate ret);
                  (* else throw warning *)
                  isvalid
              end
            end
          (* it's not none. we got a func*)
          | Some args ->
            (* check if function was already bound in the typeenv where we are *)
            (** BIND GLOBALLY OR LOCALLY ? branch or [] ? **)
            begin match type_env_get env (unlocate var) branch with
              (* not bound. do bind. this line is valid *)
              | None -> type_env_bind env (unlocate var) branch (TDFunction ((TCActual (unlocate ret)), (type_container_list_of_args (unlocate args)))); true
              (* bound. check if valid *)
              | Some td -> begin match td with
                (* Already bound to function. Type error*)
                | TDVariable _ -> type_error_at "the identifier can't be a variable; it was already defined to be a function" var
                | TDFunction (tcret, tcargs) ->
                  let retisvalid = type_container_is_type tcret (unlocate ret) in
                  let argsisvalid = type_container_list_same tcargs (type_container_list_of_args (unlocate args)) in
                  (* check if type checking worked *)
                  if not retisvalid then type_error_at_type "function was declared two times with different return types" ret tcret (unlocate ret);
                  if not argsisvalid then type_error_at_args_container "function was declared two times with different arguments" args tcargs (type_container_list_of_args (unlocate args));
                  (* else throw warning *)
                  retisvalid && argsisvalid
              end
            end
        end

      (* A function invoke *)
      | IInvoke (id, args) ->
        (* make the function variable name *)
        let var = (SExtern, (Position.value id)) in
        (* check if function was already bound in the typeenv where we are *)
        begin match type_env_get env var branch with
          (* not bound. we can't know if type is void nor if arguments are correct. it's ok *)
          | None -> true
          (* bound. check if valid *)
          | Some td -> begin match td with
            (* Bound to variable. Type error*)
            | TDVariable _ -> type_error_at "can't use invoke on a variable" id
            (* Bound to function. Check args *)
            | TDFunction (tcret, tcargs) ->
              let tcarglist = (arglist_type (unlocate args) env branch) in
              let argsisvalid = type_container_list_same tcargs tcarglist in
              let retisvalid = type_container_is_type tcret TVoid in
              (* check if type checking  on arguments worked *)
              if not argsisvalid then type_error_at_args_container "wrong type of arguments in invoke" args tcargs tcarglist;
              (* TODO: do we keep this ? we use a lot of funcs w/ side-effects in game programming. maybe warning ?*)
              if not retisvalid then type_error_at_type "a function called with the invoke instr must return void" id tcret TVoid;
              (* return *)
              retisvalid && argsisvalid
          end
        end

      (* any other declaration is of sound type. *)
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
