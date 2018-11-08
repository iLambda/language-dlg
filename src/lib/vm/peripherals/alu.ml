open Data

type alu_error_reason =
  | AluCallInvalidArgType of int * value * value
  | AluCallInvalidArgNumber of int * int
  | AluCallNoSuchFunction of string

type alu_error = {
  reason: alu_error_reason;
}

exception Alu_error of alu_error

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

(* Apply a function from the VM's library *)
let alu_call name args =
  (* helper to ensure type *)
  let rec ensure_type_args model args = match model, args with
    (* nothing left in the two lists : ok*)
    | [], [] -> ()
    (* something is wrong in the nubmer of args *)
    | [], _
    | _, []  -> raise (Alu_error { reason = (AluCallInvalidArgNumber ((List.length model),(List.length args)))  })
    (* two args : check types *)
    | model_arg::model_tl, arg::tl when same_type model_arg arg -> ensure_type_args model_tl tl
    | model_arg::model_tl, arg::_ ->
      (* get id of current arg failure *)
      let arg_id = (List.length model) - (List.length model_tl) - 1 in
      (* raise *)
      raise (Alu_error { reason = (AluCallInvalidArgType (arg_id, model_arg, arg)) })
  in
  (* match over name of function *)
  match name with
    (* random function *)
    | "rand" ->
        (* ensure there are the right number of args *)
        ensure_type_args [] args;
        (* call *)
        (fun _ -> VFloat (Random.float 1.0))(args)
    (* cos function *)
    | "cos" ->
        (* ensure there are the right number of args *)
        ensure_type_args [ VFloat 0. ] args;
        (* call *)
        (fun args ->
          (* get first token as float value *)
          let x = float_of_value (List.nth args 0)
          in VFloat (cos x))(args)
    (* sin function *)
    | "sin" ->
        (* ensure there are the right number of args *)
        ensure_type_args [ VFloat 0. ] args;
        (* call *)
        (fun args ->
          (* get first token as float value *)
          let x = float_of_value (List.nth args 0)
          in VFloat (sin x))(args)
    (* tan function *)
    | "tan" ->
        (* ensure there are the right number of args *)
        ensure_type_args [ VFloat 0. ] args;
        (* call *)
        (fun args ->
          (* get first token as float value *)
          let x = float_of_value (List.nth args 0)
          in VFloat (tan x))(args)

    (* no such function.*)
    | _ -> raise (Alu_error { reason = (AluCallNoSuchFunction name) })


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
