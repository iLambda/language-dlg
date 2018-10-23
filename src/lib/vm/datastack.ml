open Data

(* A vm stack *)
type datastack = data Stack.t

(* Create an empty stack *)
let datastack_make () = Stack.create ()
