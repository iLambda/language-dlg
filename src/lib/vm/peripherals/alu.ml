open Data

(* An operation *)
type operation =
  | OpPlus
  | OpMinus
  | OpStar
  | OpDivide
  | OpAnd
  | OpOr
  | OpEqual
  | OpNotEqual
  | OpLeq
  | OpGeq
  | OpLess
  | OpMore

(* Compute an operation *)
let alu_compute lhs op rhs = match op with
  (* + *)
  | OpPlus -> begin match lhs, rhs with
    (* String concat *)
    | VString s1, VString s2 -> VString (s1 ^ s2)
    (* Not implemented yet *)
    | _ -> failwith "Not implemented"
  end
  (* Equality  *)
  | OpEqual -> VBool (lhs = rhs)
  (* Difference *)
  | OpNotEqual -> VBool (lhs <> rhs)
  (* Not implemented yet *)
  | _ -> failwith "Not implemented"
