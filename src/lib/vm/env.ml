open Data
open Error

(* A scoped environment *)
type unscoped_env = (identifier, value) Hashtbl.t
type scoped_env = ((identifier, value) Hashtbl.t) Stack.t
(* An environment *)
type env = {
  (* local environment *)
  local: scoped_env;
  (* global environment *)
  global: unscoped_env;
}

(* make an empty environment *)
let env_make () =
  (* Make env *)
  let env = {
    local = Stack.create ();
    global = Hashtbl.create 64;
  } in
  (* Create the first scope *)
  Stack.push (Hashtbl.create 64) env.local;
  (* Return env *)
  env

(* clear script local values *)
let env_clear env =
  (* clear stack *)
  Stack.clear env.local;
  (* Recreate the first scope *)
  Stack.push (Hashtbl.create 64) env.local

(* wipe all data *)
let env_wipe env =
  env_clear env;
  Hashtbl.clear env.global

(* compute the env depth *)
let env_get_depth env = Int32.of_int((Stack.length env.local) - 1)

(* Raises the scope *)
let env_raise_scope env =
  (* pop the top stack *)
  ignore(Stack.pop env.local)

(* Deepends the scope *)
let env_deepen_scope env =
  (* make a stack and push*)
  Stack.push (Hashtbl.create 64) env.local

(* set scope depth  *)
let env_set_scope_depth env depth =
  (* compute env depth *)
  let env_depth = env_get_depth env in
  (* env depth is same as set depth *)
  if depth = env_depth then ()
  (* set depth is lesser than env depth *)
  else if env_depth > depth then
    (* while there are stacks to pop *)
    while (env_get_depth env) <> depth do
      (* raise the scope *)
      env_raise_scope env;
    done
  (* set depth is deeper than env depth *)
  else if env_depth < depth then
    (* while there are stacks to push *)
    while (env_get_depth env) <> depth do
      (* pop a stack at the top *)
      env_deepen_scope env
    done

(* get binding in scoped *)
let env_get_scoped_binding_opt env id =
  (* get actual binding *)
  let fold_binding (acc, scope) table =
    (* get this table's bindign *)
    let binding = Hashtbl.find_opt table id in
    (* if acc is none, set. else, acc*)
    if acc = None then (binding, (Int32.pred scope))
                  else (acc, (Int32.pred scope))
  in
  (* fold *)
  Stack.fold fold_binding (None, env_get_depth env) env.local

(* get nth depth scope of local table *)
let env_get_nth_scope_table env depth =
  (* get actual table *)
  let fold_table (acc, _) table =
    (* check if acc = idx. if yes, return *)
    if acc = depth then (acc, Some table)
    (* else, increment acc *)
    else (Int32.pred acc, None)
  in
  (* fold *)
  let _, sc = Stack.fold fold_table ((Int32.pred (env_get_depth env)), None) env.local in sc

(* get type of declaration *)
let env_same_type env scope id value =
  (* check scope *)
  match scope with
    | Global ->
      (* get it *)
      begin match Hashtbl.find_opt env.global id with
        (* no binding : can do *)
        | None -> true
        (* check if type same *)
        | Some v -> same_type v value
      end
    | Local ->
      (* get it *)
      begin match env_get_scoped_binding_opt env id with
        (* no binding : can do *)
        | (None, _) -> true
        (* check if there is a stack *)
        | (Some def_val, _) ->
          (* check if variable is accessible at cur depth *)
          same_type value def_val
      end
    | Extern -> assert false


(* set a value *)
let env_set env scope id value =
  (* check the scope *)
  match scope with
    (* local *)
    | Local ->
      (* Check if stack already present *)
      begin match env_get_scoped_binding_opt env id with
        (* no binding. set blindly in top stack *)
        | (None, _) ->
          (* add to table *)
          (ignore (LTerm.print id));
          Hashtbl.replace (Stack.top (env.local)) id value
        (* there is a binding already *)
        | (Some def_value, def_depth) ->
          (* it IS accessible. *)
          (* get the table containing it *)
          match env_get_nth_scope_table env def_depth with
            (* None ? This can't be. *)
            | None -> assert false
            (* Some ! Replace binding. *)
            | Some scope ->
              (* check if type is same  *)
              if same_type value def_value
              then Hashtbl.replace scope id value
              else raise (make_variable_error value def_value)
      end
    (* global *)
    | Global ->
      (* check if types compatible *)
      if env_same_type env scope id value
      (* just set it *)
      then Hashtbl.replace env.global id value
      (* else *)
      else
        (* error *)
        let def_value = Hashtbl.find env.global id in
        raise (make_variable_error value def_value)
    (* extern : dead code *)
    | Extern -> assert false

let env_init env scope id value =
  (* check the scope *)
  match scope with
    (* local *)
    | Local ->
      (* Check if stack already present *)
      begin match env_get_scoped_binding_opt env id with
        (* no binding. set blindly in top stack *)
        | (None, _) ->
          (* add to table *)
          Hashtbl.replace (Stack.top (env.local)) id value
        (* there is a binding already *)
        | (Some def_value, _) ->
          (* check type just to ensure type corectness *)
          if not (same_type value def_value) then raise (make_variable_error value def_value);
          (* we don't bind. *)
          raise (Vm_error { reason = VmAlreadyBoundVariable })
      end
    (* global *)
    | Global ->
      (* check if set *)
      begin match Hashtbl.find_opt env.global id with
        (* not set ; set *)
        | None ->
          (* bind *)
          Hashtbl.replace env.global id value;
        (* something; don't bind  *)
        | Some _ -> raise (Vm_error { reason = VmAlreadyBoundVariable })
      end

    (* extern : dead code *)
    | Extern -> assert false

(* set if the value is unset *)
let env_ifnset env scope id value =
  (* try to init *)
  try env_init env scope id value
  (* if error : variable already bound, nothing is done *)
  with
    | Vm_error { reason = VmAlreadyBoundVariable } -> ()


(* get a value *)
let env_get env scope id =
  (* check the scope *)
  match scope with
    (* local *)
    | Local ->
      (* Check if stack already present *)
      begin match env_get_scoped_binding_opt env id with
        (* no binding. error *)
        | (None, _) -> raise (Vm_error { reason= VmUnboundVariable })
        (* there is a binding already *)
        | (Some def_value, _) ->
          (* it IS accessible. return value *)
          def_value
      end
    (* global *)
    | Global ->
      (* check if in environment *)
      begin match Hashtbl.find_opt env.global id with
        (* none *)
        | None -> raise (Vm_error { reason= VmUnboundVariable })
        (* a binding. return *)
        | Some v -> v
      end
    (* extern : dead code *)
    | Extern -> assert false
