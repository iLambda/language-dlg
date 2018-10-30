open Data

(* A scoped environment *)
type scoped_env = (identifier, value) Hashtbl.t
(* An environment *)
type env = {
  local: scoped_env;
  global: scoped_env;
  mutable scope: int
}

(* make an empty environment *)
val env_make : unit -> env
(* set a value *)
val env_set : env -> scope -> identifier -> value -> unit
(* set if the value is unset *)
val env_ifnset : env -> scope -> identifier -> value -> unit
(* get a value *)
val env_get : env -> scope -> identifier -> value
(* clear script local values *)
val env_clear : env -> unit
(* wipe all data *)
val env_wipe : env -> unit
(* raise/deepen scope *)
val env_raise_scope : env -> unit
val env_deepen_scope : env -> unit
