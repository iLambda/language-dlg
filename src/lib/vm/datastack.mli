open Data

(* A vm stack *)
type datastack = data Stack.t

(* Create an empty stack *)
val datastack_make : unit -> datastack
