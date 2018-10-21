open DLG.Ast


(* Returns true iff types are the same *)
val type_is_same : type_const -> type_const -> bool
(* Returns true iff type list is same *)
val type_list_is_same : type_const list -> type_const list -> bool
(* Returns true iff types are the same *)
val type_function_is_same : type_func -> type_func -> bool
(* Returns true iff types represent a number *)
val type_is_number : type_const -> bool
(* Returns the type with least assumption carried with it *)
val type_least_assumption : type_const -> type_const -> type_const

(* Get a list of valid casts from type *)
val type_valid_casts_from : type_const -> type_const list
(* Check if a typecast is valid*)
val type_is_valid_cast : type_const -> type_const -> bool

(* Check if an operation is valid*)
val type_is_valid_op : type_const -> operation -> type_const -> bool
(* Returns the return type of an operation given two valid lhs and rhs types *)
val type_return_op : type_const -> operation -> type_const -> type_const
(* Returns the signatures allowed for an operator *)
val type_op_signature : operation -> type_func list

(* Returns a function signature type *)
val type_of_function : type_const -> type_const list -> type_func
(* Returns the type of a literal *)
val type_of_literal : literal -> type_const
(* Casts a literal *)
val type_cast_literal : type_const -> literal -> literal
