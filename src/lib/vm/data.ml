type value =
  | VInt of int32
  | VFloat of float
  | VBool of bool
  | VString of string
  (* VEnum of string ??? *)
  | VVec2 of float * float
  | VVec3 of float * float * float
