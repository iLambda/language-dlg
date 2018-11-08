open Data

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
val env_make : unit -> env
(* set a value *)
val env_set : env -> scope -> identifier -> value -> unit
(* set if the value is unset *)
val env_ifnset : env -> scope -> identifier -> value -> unit
(* set if the value is unset; else, fail *)
val env_init : env -> scope -> identifier -> value -> unit
(* get a value *)
val env_get : env -> scope -> identifier -> value
(* clear script local values *)
val env_clear : env -> unit
(* wipe all data *)
val env_wipe : env -> unit
(* get type of declaration *)
val env_same_type : env -> scope -> identifier -> value -> bool

(* raise/deepen scope *)
val env_raise_scope : env -> unit
val env_deepen_scope : env -> unit
val env_set_scope_depth : env -> int32 -> unit
