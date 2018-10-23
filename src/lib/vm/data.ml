(* A value *)
type value =
  | VInt of int32
  | VFloat of float
  | VBool of bool
  | VString of string
  (* VEnum of string ??? *)
  | VVec2 of float * float
  | VVec3 of float * float * float


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
