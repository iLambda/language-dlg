open Data

(* An element in the stack *)
type data_stack_element =
  | Value of value

(* A vm stack *)
type data_stack = data_stack_element Stack.t

(* Create an empty stack *)
let make () = Stack.create ()
