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

(* Copy type *)
let alu_copy_type dummy dest = match dummy, dest with
  (* Identity cast *)
  | src, dest when same_type src dest -> dest
  (* Cast numbers *)
  | (VInt _), VFloat f -> VInt (Int32.of_float f)
  | (VFloat _), VInt i -> VFloat (Int32.to_float i)
  (* Vectors *)
  | (VVec2 _), VVec3 (x, y, _) -> VVec2 (x, y)
  | (VVec3 _), VVec2 (x, y) -> VVec3 (x, y, 0.)
  (* Anything to string *)
  | (VString _), _ -> VString (inline_string_of_data (Value dest))
  (* Other casts don't work *)
  | _ -> raise (make_type_error "" (Value dest))


(* Compute an operation *)
let alu_compute lhs op rhs = match op with
  (* plus operator *)
  | OpPlus -> begin match lhs, rhs with
    (* String concat *)
    | VString s1, VString s2 -> VString (s1 ^ s2)
    (* Vector add *)
    | VVec2 (x1, y1), VVec2(x2, y2) -> VVec2(x1 +. x2, y1 +. y2)
    | VVec3 (x1, y1, z1), VVec3(x2, y2, z2) -> VVec3(x1 +. x2, y1 +. y2, z1 +. z2)
    (* Numbers *)
    | VInt i1, VInt i2 -> VInt (Int32.add i1 i2)
    | _ ->
      (* try get as numbers *)
      let x = number_of_data (Value lhs) in
      let y = number_of_data (Value rhs) in
      VFloat (x +. y)
  end
  (* minus operator *)
  | OpMinus -> begin match lhs, rhs with
    (* Vector add *)
    | VVec2 (x1, y1), VVec2(x2, y2) -> VVec2(x1 -. x2, y1 -. y2)
    | VVec3 (x1, y1, z1), VVec3(x2, y2, z2) -> VVec3(x1 -. x2, y1 -. y2, z1 -. z2)
    (* Numbers *)
    | VInt i1, VInt i2 -> VInt (Int32.sub i1 i2)
    | _ ->
      (* try get as numbers *)
      let x = number_of_data (Value lhs) in
      let y = number_of_data (Value rhs) in
      VFloat (x -. y)
  end
  (* star operator *)
  | OpStar -> begin match lhs, rhs with
    (* Numbers *)
    | VInt i1, VInt i2 -> VInt (Int32.mul i1 i2)
    | VInt _, VFloat _
    | VFloat _, VInt _
    | VFloat _, VFloat  _ ->
      (* coerce as floats *)
      let x = number_of_data (Value lhs) in
      let y = number_of_data (Value rhs) in
      VFloat(x *. y)

    (* 2D vectors *)
    | VVec2 (x, y), n
    | n, VVec2(x, y) ->
      (* coerce as floats *)
      let f = number_of_data (Value n) in
      VVec2(f *. x, f *. y)
    (* 3D vectors *)
    | VVec3 (x, y, z), n
    | n, VVec3(x, y, z) ->
      (* coerce as floats *)
      let f = number_of_data (Value n) in
      VVec3(f *. x, f *. y, f *. z)

    (* String repetition *)
    | VInt i1, VString s1
    | VString s1, VInt i1 ->
      (* repeat string *)
      let rec stringn s n = match n with
        | 1l -> s
        | _ -> s ^ stringn s (Int32.pred n) in
      (* repeat *)
      VString (stringn s1 i1)

    (* Error *)
    | _ -> raise (make_type_error "" (Value lhs))
  end


  | OpDivide -> begin match lhs, rhs with
    (* Numbers *)
    | VInt i1, VInt i2 -> VInt (Int32.div i1 i2)
    | VInt _, VFloat _
    | VFloat _, VInt _
    | VFloat _, VFloat  _ ->
      (* coerce as floats *)
      let x = number_of_data (Value lhs) in
      let y = number_of_data (Value rhs) in
      VFloat(x /. y)

    (* 2D vectors *)
    | VVec2 (x, y), n ->
      (* coerce as floats *)
      let f = number_of_data (Value n) in
      VVec2(x /. f, y  /. f)
    (* 3D vectors *)
    | VVec3 (x, y, z), n ->
      (* coerce as floats *)
      let f = number_of_data (Value n) in
      VVec3(x /. f, y /. f, z /. f)

    (* Error *)
    | _ -> raise (make_type_error "" (Value lhs))
  end
  (* Comparison operators *)
  | OpMore -> begin match lhs, rhs with
    | VInt s1, VInt s2 -> VBool (s1 > s2)
    | _ ->
      (* try get as numbers *)
      let x = number_of_data (Value lhs) in
      let y = number_of_data (Value rhs) in
      VBool (x > y)
  end
  | OpLess -> begin match lhs, rhs with
    | VInt s1, VInt s2 -> VBool (s1 < s2)
    | _ ->
      (* try get as numbers *)
      let x = number_of_data (Value lhs) in
      let y = number_of_data (Value rhs) in
      VBool (x < y)
  end
  | OpLeq -> begin match lhs, rhs with
    | VInt s1, VInt s2 -> VBool (s1 <= s2)
    | _ ->
      (* try get as numbers *)
      let x = number_of_data (Value lhs) in
      let y = number_of_data (Value rhs) in
      VBool (x <= y)
  end
  | OpGeq -> begin match lhs, rhs with
    | VInt s1, VInt s2 -> VBool (s1 >= s2)
    | _ ->
      (* try get as numbers *)
      let x = number_of_data (Value lhs) in
      let y = number_of_data (Value rhs) in
      VBool (x >= y)
  end
  (* Boolean operators *)
  | OpAnd -> VBool ((bool_of_data (Value lhs)) && (bool_of_data (Value rhs)))
  | OpOr -> VBool ((bool_of_data (Value lhs)) || (bool_of_data (Value rhs)))
  (* Equality  *)
  | OpEqual -> VBool (lhs = rhs)
  (* Difference *)
  | OpNotEqual -> VBool (lhs <> rhs)
