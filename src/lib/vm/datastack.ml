open Data

(* An element in the stack *)
type datastack_element =
  | Value of value

(* A vm stack *)
type datastack = datastack_element Stack.t

(* Create an empty stack *)
let datastack_make () = Stack.create ()
