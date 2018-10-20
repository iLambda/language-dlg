open DLG.Ast
open Utils.Position
open Env
open Error
open Type

(* The expected type list when we wait for a number *)
let expected_types_number = [ TCValue (TFloat); TCValue (TInt) ]
(* Return the given argument from a type for a type_error*)
let given t = Some (TCValue t)


(* Returns the type of an expression, given an environment and a branch
  -> Can raise Type_error *)
let rec get_expr_type env branch = function
  (* A literal *)
  | ELiteral lit ->
    (* Check the constructors if needed *)
    begin match value lit with
      (* A 2D vec; check the constructor *)
      | LVec2 (x, y) ->
        (* compute the arguments *)
        let tx = get_expr_type env branch (value x) in
        let ty = get_expr_type env branch (value y) in
        (* check if expression is a number *)
        if not (type_is_number tx)
        then raise_located_type_error ReasonNotANumber x (given tx) expected_types_number;
        if not (type_is_number ty)
        then raise_located_type_error ReasonNotANumber y (given ty) expected_types_number;

      (* A 3D vec; check the constructor *)
      | LVec3 (x, y, z) ->
        (* compute the arguments *)
        let tx = get_expr_type env branch (value x) in
        let ty = get_expr_type env branch (value y) in
        let tz = get_expr_type env branch (value z) in
        (* check if expression is a number *)
        if not (type_is_number tx)
        then raise_located_type_error ReasonNotANumber x (given tx) expected_types_number;
        if not (type_is_number ty)
        then raise_located_type_error ReasonNotANumber y (given ty) expected_types_number;
        if not (type_is_number tz)
        then raise_located_type_error ReasonNotANumber z (given tz) expected_types_number;

      (* A formatted string ; check the tokens *)
      | LString fstr ->
        (* Check it *)
        check_fstring_type env branch fstr;

      (* Any other literal needs no checking*)
      | _ -> ()
    end;
    (* Return the type *)
    type_of_literal (value lit)

  (* A variable *)
  | EVar var ->
    (* try to get the binding and check for any error *)
    let binding = check_for_type_error (env_try_get env branch (value var)) var in
    (* check what we got in our binding*)
    begin match binding with
      (* No binding, but no error in finding it. Type is polymorphic*)
      | None -> TUnknown
      (* There is a binding. *)
      | Some decl -> begin match decl with
        (* It's a function. Abort *)
        | DeclareFunction tf -> raise (make_located_type_error ReasonNotAValue var None [ TCFunc tf ])
        (* The right definition *)
        | DeclareVariable tv -> tv
      end
    end

  (* A function *)
  | EFunc (nameid, arglist) ->
    (* make the actual identifier *)
    let id = (SExtern, (value nameid)) in
    (* try to get the binding and check for any error *)
    let binding = check_for_type_error (env_try_get env branch id) nameid in
    (* get type of arglist *)
    let targs = get_arglist_type env branch (value arglist) in
    (* check what we got in our binding*)
    begin match binding with
      (* No binding, but no error in finding it. Type is polymorphic*)
      | None -> TUnknown
      (* There is a binding. *)
      | Some decl -> begin match decl with
        (* It's a variable. Abort *)
        | DeclareVariable tv -> raise (make_located_type_error ReasonNotAValue nameid None [ TCValue tv ])
        (* It's a function. Check if args match *)
        | DeclareFunction (tret, targs0) ->
          (* Check arguments *)
          if not (type_list_is_same targs0 targs)
          then raise (make_located_type_error ReasonIncompatibleDeclaration arglist None [ TCFunc (tret, targs0) ]);
          (* Check if type is void *)
          if type_is_same tret TVoid
          then raise (make_located_type_error ReasonNoProduceVoid nameid (Some (TCFunc (tret, targs0))) []);
          (* Return the type *)
          tret
      end
    end

  (* A ternary *)
  | ECondition (condition, thendo, elsedo) ->
    (* Checks if condition is bool *)
    let tcondition = get_expr_type env branch (value condition) in
    if not (type_is_same tcondition TBool)
    then raise (make_located_type_error ReasonConditionWrongType condition (given tcondition) [ TCValue TBool ]);
    (* Check if subexpressions have same type *)
    let tthen = get_expr_type env branch (value thendo) in
    let telse = get_expr_type env branch (value elsedo) in
    (* Check if they are not the same type *)
    if not (type_is_same tthen telse)
    then raise (make_located_type_error ReasonBranchesDifferentType elsedo (given telse) [ TCValue tthen ]);
    (* Return the type of the branch *)
    tthen

  (* Access to a constructed type *)
  | EAccess (constr, id) ->
    (* Gets the string represented by the ID *)
    let accessed = match (value id) with Id s -> s in
    (* Gets the type of the constructed type *)
    let tconstr = get_expr_type env branch (value constr) in
    (* Match the type *)
    begin match tconstr with
      (* A polymorphic type *)
      | TUnknown -> TUnknown
      (* A 2D vector *)
      | TVec2 ->
        (* check if accessed x or y. else, errors *)
        if List.mem accessed ["x"; "y"] then TFloat
        else raise (make_located_type_error ReasonNoProperty id None [])
      (* A 3D vector *)
      | TVec3 ->
        (* check if accessed x or y. else, errors *)
        if List.mem accessed ["x"; "y"; "z"] then TFloat
        else raise (make_located_type_error ReasonNoProperty id None [])
      (* No other literal has any property *)
      | _ -> raise (make_located_type_error ReasonNoProperty id None [])
    end

  (* A cast *)
  | ETypeCast (totype, expr) ->
    (* Get the target type *)
    let ttarget = value totype in
    (* Compute the type of the expression *)
    let texpr = get_expr_type env branch (value expr) in
    (* Check if type cast valid *)
    if not (type_is_valid_cast texpr ttarget)
    then raise (make_located_type_error ReasonInvalidTypeCast totype (given ttarget) (List.map (fun t -> TCValue t) (type_valid_casts_from texpr)));
    (* Return the cast destination type *)
    ttarget

  (* An operation *)
  | EOperation (op, lhs, rhs) ->
    (* Compute the type of the rhs and get the operator *)
    let trhs = get_expr_type env branch (value rhs) in
    let tlhs = get_expr_type env branch (value lhs) in
    let top = value op in
    (* Check if the operation is valid *)
    if not (type_is_valid_op tlhs top trhs)
    then raise (make_located_type_error ReasonInvalidTypeOp lhs (given tlhs) []);
    (* If any of them is unknown, return unknown*)
    if trhs = TUnknown || tlhs = TUnknown then TUnknown
    else
    (* Get the return type *)
    type_return_op tlhs top trhs

(* Returns the type of an arglist *)
and get_arglist_type env branch = function
  (* function takes no arguments : void *)
  | [] -> []
  (* an argument. evaluate the expressio and keep crawling*)
  | expr::tail -> (get_expr_type env branch (value expr))::(get_arglist_type env branch tail)

(* Checks the type of a formatted string
  -> Can raise type Type_error *)
and check_fstring_type env branch = function
  (* Empty fstring. Ok*)
  | [] -> ()
  (* Crawl through tokens *)
  | tok::tail -> begin match value tok with
    (* Only check expressions*)
    | StrInline expr ->
      (* Compute the type of the expression, ensuring it is sound *)
      let _ = get_expr_type env branch (value expr) in
      (* Check next tokens *)
      check_fstring_type env branch tail
    (* All other tokens are ok *)
    | _ -> check_fstring_type env branch tail
  end

(* Checks the type of an instruction
  -> Can raise Type_error *)
let rec check_instruction_type env branch = function
  (* An ifnset/set expr *)
  | IIfnset (var, expr) | ISet (var, expr) ->
    (* evaluate the type of the subexpression *)
    let texpr = get_expr_type env branch (value expr) in
    (* try to bind the varialbe*)
    let tentative = env_try_bind env branch (value var) (DeclareVariable texpr) in
    (* check if there was an error *)
    check_for_type_error tentative expr

  (* A wait/speed expression *)
  | IWait (_, expr) | ISpeed expr ->
    (* Evaluate expression *)
    let texpr = get_expr_type env branch (value expr) in
    (* Check if number *)
    if not (type_is_number texpr)
    then raise_located_type_error ReasonNotANumber expr (given texpr) expected_types_number

  (* A choice possibility *)
  | IChoice choices ->
    (* check the list of choices *)
    let rec check_choices_type i = function
      (* Empty, it's valid *)
      | [] -> ()
      (* Not empty *)
      | (msg, prog)::tail ->
        (* get and check the fstring *)
        let fstr, _ = (value msg) in
        check_fstring_type env branch fstr;
        (* check the subprogram*)
        check_subprogram_type env (env_branch_child branch i) (value prog);
        (* check the next choice *)
        check_choices_type (i+1) tail
    (* start the subfunction on choices list*)
    in check_choices_type 0 choices

  (* A condition *)
  | ICondition (matched, branches) ->
    (* check a pattern *)
    let check_pattern_type env branch tmatch = function
      (* Wildcard is ok*)
      | PWildcard -> ()
      (* A value. Okay iff type is the same as matched *)
      | PValue v ->
        (* Evaluate value type *)
        let tvalue = get_expr_type env branch (value v) in
        (* Check *)
        if not (type_is_same tvalue tmatch)
        then raise_located_type_error ReasonInvalidValuePattern v (given tvalue) [TCValue tmatch]
      (* A binding expression *)
      | PBinding (id, cond) ->
        (* Create the scoped identifier for the local branch variable *)
        let scopedid = (SLocal, (value id)) in
        (* Check if bound *)
        if env_is_bound env branch scopedid
        then raise_located_type_error ReasonAlreadyBound id None [];
        (* Bind the local variable with type same as matched value *)
        env_bind env branch scopedid (DeclareVariable tmatch);
        (* Check if the condition is indeed a boolean *)
        let tcond = get_expr_type env branch (value cond) in
        if not (type_is_same tcond TBool) then
        raise_located_type_error ReasonConditionWrongType cond (given tcond) [TCValue TBool];
      in
      (* check all branches *)
      let rec check_branches_type env branch tmatch i = function
        (* Empty branches. No check *)
        | [] -> ()
        (* Crawl through branches *)
        | (pattern, prog)::tail ->
          (* Check the pattern *)
          check_pattern_type env (env_branch_child branch i) tmatch (value pattern);
          (* Check the subprogram *)
          check_subprogram_type env (env_branch_child branch i) (value prog);
          (* Check the next choice *)
          check_branches_type env branch tmatch (i+1) tail
      in
      (*Evaluate the type of the matched value *)
      let tmatch = get_expr_type env branch (value matched) in
      (* Start checking all branches *)
      check_branches_type env branch tmatch 0 branches

  (* A message *)
  | IMessage msg ->
    (* Get the underlying formatted string *)
    let fstr, _ = value msg in
    (* Verify the fstring *)
    check_fstring_type env branch fstr

  (*A function invocation*)
  | IInvoke (funcid, args) ->
    (* create the function scoped identifier *)
    let scopedid = (SExtern, (value funcid)) in
    (* check if the function was bound *)
    let tentative = env_try_get env branch scopedid in
    (* try raise any error and match over result *)
    begin match check_for_type_error tentative funcid with
      (* Function unbound ; since it is extern, it is ok *)
      | None -> ()
      (* Function bound. Check if valid declaration *)
      | Some decl -> begin match decl with
        (* Bound to a variable. Error *)
        | DeclareVariable _ ->
          (* Evaluate type of arglist *)
          let targlist = get_arglist_type env branch (value args) in
          (* Create the expected function type *)
          let tfexpected = TVoid, targlist in
          (* Raise the error *)
          raise_located_type_error ReasonNotAFunction funcid None [ TCFunc tfexpected ]
        (* Bound to a function. Check if types match *)
        | DeclareFunction (tret, targs) ->
          (* Evaluate type of arglist *)
          let targlist = get_arglist_type env branch (value args) in
          (* Check if argument types match *)
          if not (type_list_is_same targlist targs)
          then raise_located_type_error ReasonInvalidCall args (Some (TCFunc (TVoid, targlist))) [ TCFunc (TVoid, targs) ];
          (* Check if return type is indeed void *)
          if not (type_is_same TVoid tret)
          then raise_located_type_error ReasonNotVoid funcid  (given tret) [ TCValue TVoid ]
      end
    end

  (* An explicit declaration *)
  | IDeclare (scopedid, return, possibleargs) ->
    (* Check if there is an arglist*)
    begin match possibleargs with
      (* None. This is a variable declaration *)
      | None ->
        (* Check if return is void *)
        if type_is_same (value return) TVoid
        then raise_located_type_error ReasonNoProduceVoid return (given (value return)) [];
        (* try to get the binding *)
        let tentative = env_try_get env branch (value scopedid) in
        (* try raise any error and match over result *)
        begin match check_for_type_error tentative scopedid with
          (* No previous binding. Bind *)
          | None -> env_bind env branch (value scopedid) (DeclareVariable (value return))
          (* Already bound. Check if this one interferes *)
          | Some decl -> begin match decl with
            (* Bound to a function. Error *)
            | DeclareFunction tf -> raise_located_type_error ReasonIncompatibleDeclaration return (Some (TCFunc tf)) [TCValue (value return)]
            (* Bound to a variable *)
            | DeclareVariable tvar ->
              (* Check if compatible *)
              if not (type_is_same tvar (value return))
              then raise_located_type_error ReasonIncompatibleDeclaration return (given (value return)) [ TCValue tvar ]
          end
        end
      (* Some arglist has been provided. This is a function declaration *)
      | Some args ->
        (* construct the type *)
        let twanted = ((value return), (value args)) in
        (* try to get the binding *)
        let tentative = env_try_get env branch (value scopedid) in
        (* try raise any error and match over result *)
        begin match check_for_type_error tentative scopedid with
          (* No previous binding. Bind *)
          | None -> env_bind env branch (value scopedid) (DeclareFunction twanted)
          (* Already bound. Check if this one interferes *)
          | Some decl -> begin match decl with
            (* Bound to a function. Error *)
            | DeclareVariable tvar -> raise_located_type_error ReasonIncompatibleDeclaration return (Some (TCFunc twanted)) [TCValue tvar]
            (* Bound to a variable *)
            | DeclareFunction (tret, targs) ->
              (* the function declaration type *)
              let tfunc = tret, targs in
              (* Check if return types compatible *)
              if not (type_is_same tret (value return))
              then raise_located_type_error ReasonIncompatibleDeclaration return (Some (TCFunc twanted)) [ TCFunc tfunc ];
              (* Check if arguments types compatible *)
              if not (type_list_is_same targs (value args))
              then raise_located_type_error ReasonIncompatibleDeclaration args (Some (TCFunc twanted)) [ TCFunc tfunc ]
          end
        end
    end
  (* Any other instruction needs no checking *)
  | _ -> ()

(* Checks the type subprogram
  -> Can raise Type_error *)
and check_subprogram_type env branch subprogram =
  (* the current program insctruction and the rest of the program *)
  let program_instr = ref INop in
  let program_left = ref subprogram in
  (* while there are instructions *)
  while !program_left <> [] do
    (* get the current instruction *)
    begin match !program_left with
      (* Can't reach *)
      | [] -> assert false
      (* Check instruction *)
      | i::rest ->
        (* get current instruction *)
        program_instr := (value i);
        (* get rest of program *)
        program_left := rest;
    end;
    (* check if instruction valid *)
    check_instruction_type env branch !program_instr
  done

(* Checks the type of a program
  -> Can raise Type_error *)
let check_program_type program =
  (* run with blank environment *)
  check_subprogram_type (env_make ()) (env_branch_root ()) program
