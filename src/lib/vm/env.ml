open Data
open Error

(* An identifier *)
type identifier = string
(* A scope *)
type scope = Local | Global
(* A scoped environment *)
type scoped_env = (identifier, value) Hashtbl.t
(* An environment *)
type env = {
  local: scoped_env;
  global: scoped_env
}

(* make an empty environment *)
let make () = {
  local = Hashtbl.create 64;
  global = Hashtbl.create 64
}


(* clear script local values *)
let env_clear env =
  Hashtbl.clear env.local

(* wipe all data *)
let env_wipe env =
  Hashtbl.clear env.local;
  Hashtbl.clear env.global

(* returns the hashtable given a scope *)
let env_scoped_hashtable env scope = match scope with
  | Local -> env.local
  | Global -> env.global

(* set a value *)
let env_set env scope id value =
  (* gets the hashtbl *)
  let hashtbl = env_scoped_hashtable env scope in
  (* set the value *)
  Hashtbl.add hashtbl id value

(* set if the value is unset *)
let env_ifnset env scope id value =
  (* get the hashtbl *)
  let hashtbl = env_scoped_hashtable env scope in
  (* try to find *)
  match Hashtbl.find_opt hashtbl id with
    (* Not bound, set *)
    | None -> env_set env scope id value
    (* Bound. Do nothing *)
    | Some _ -> ()

(* get a value *)
let env_get env scope id =
  (* get the hashtbl *)
  let hashtbl = env_scoped_hashtable env scope in
  (* try to find *)
  match Hashtbl.find_opt hashtbl id with
    (* Not bound, error *)
    | None -> raise (Vm_error { reason=VmUnboundVariable })
    (* Bound. Return value *)
    | Some v -> v