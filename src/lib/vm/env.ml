open Data
open Error

(* A scoped environment *)
type unscoped_env = (identifier, value) Hashtbl.t
type scoped_env = (identifier, (value * int32)) Hashtbl.t
(* An environment *)
type env = {
  (* local environment *)
  local: scoped_env;
  (* global environment *)
  global: unscoped_env;
  (* depth *)
  mutable depth: int32;
}

(* make an empty environment *)
let env_make () = {
  local = Hashtbl.create 64;
  global = Hashtbl.create 64;
  depth = 0l;
}

(* clear script local values *)
let env_clear env =
  Hashtbl.clear env.local

(* wipe all data *)
let env_wipe env =
  Hashtbl.clear env.local;
  Hashtbl.clear env.global

(* set scope depth  *)
let env_set_scope_depth env depth =
  env.depth <- depth
(* Raises the scope *)
let env_raise_scope env =
  env.depth <- (Int32.pred env.depth);
  (* remove dead binding  *)
  let remove_dead _ (def_value, def_depth) =
    (* If accessible, keep *)
    if env.depth >= def_depth then (Some (def_value, def_depth))
    (* Else, remove *)
                              else None
    in
  (* remove all dead bindings *)
  Hashtbl.filter_map_inplace remove_dead env.local

(* Deepends the scope *)
let env_deepen_scope env =
  env.depth <- (Int32.succ env.depth)

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
      begin match Hashtbl.find_opt env.local id with
        (* no binding : can do *)
        | None -> true
        (* check if there is a stack *)
        | Some (def_val, def_depth) ->
          (* check if variable is accessible at cur depth *)
          if env.depth >= def_depth then same_type value def_val
          else true
      end
    | Extern -> assert false


(* set a value *)
let env_set env scope id value =
  (* check the scope *)
  match scope with
    (* local *)
    | Local ->
      (* Check if stack already present *)
      begin match Hashtbl.find_opt env.local id with
        (* no binding. set blindly *)
        | None ->
          (* add to table *)
          Hashtbl.replace env.local id (value, env.depth)
        (* there is a binding already *)
        | Some (def_value, def_depth) ->
          (* check top depth s*)
          if env.depth >= def_depth then
            (* it is accessible. check if type is same  *)
            if same_type value def_value
            then Hashtbl.replace env.local id (value, def_depth)
            else raise (make_variable_error value def_value)
          (* it is not accessible *)
          else
            (* old binding should have been deleted, cuz variable is not accessible. *)
            (* override *)
            Hashtbl.replace env.local id (value, env.depth)
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
      (* Check if value already present *)
      begin match Hashtbl.find_opt env.local id with
        (* not set ; can be set *)
        | None ->
          (* bind *)
          Hashtbl.replace env.local id (value, env.depth)
        (* set but destroyed (out of scope)*)
        | Some (_, def_depth) when env.depth < def_depth ->
          (* bind *)
          Hashtbl.replace env.local id (value, env.depth)
        (* can't be set *)
        | _ -> raise (Vm_error { reason = VmAlreadyBoundVariable })
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
      (* Check if binding already present *)
      begin match Hashtbl.find_opt env.local id with
        (* no binding. error *)
        | None -> raise (Vm_error { reason= VmUnboundVariable })
        (* there is a binding already *)
        | Some (def_value, def_depth) ->
          (* if it is accessible *)
          if env.depth >= def_depth then
            (* return value *)
            def_value
          (* it is not accessible *)
          else begin
            (* not accessible *)
            (* destroy binding *)
            Hashtbl.remove env.local id;
            (* raise *)
            raise (Vm_error { reason=VmUnboundVariable })
          end
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
