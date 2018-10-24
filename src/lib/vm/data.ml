(* A value *)
type value =
  | VInt of int32
  | VFloat of float
  | VBool of bool
  | VString of string
  (* VEnum of string ??? *)
  | VVec2 of float * float
  | VVec3 of float * float * float

(* string of value *)
let string_of_value = function
  | VInt i -> Int32.to_string i
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VString s -> s
  | VVec2 (x, y) -> "(x=" ^ (string_of_float x) ^ " ;y="  ^ (string_of_float y) ^ ")"
  | VVec3 (x, y, z) -> "(x=" ^ (string_of_float x) ^ " ;y="  ^ (string_of_float y) ^ " ;z="  ^ (string_of_float z) ^ ")"


(* A scope *)
type scope = Local | Global
(* An identifier *)
type identifier = string
(* A scoped identifier *)
type scoped_identifier = scope * identifier

(* Some data *)
type data =
  | Value of value
  | Identifier of scoped_identifier
