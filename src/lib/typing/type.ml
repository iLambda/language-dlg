open DLG.Ast
open Utils.Position

(* Returns a function signature type *)
let type_of_function treturn targs = (treturn, targs)
(* Returns the type of a literal *)
let type_of_literal = function
  | LInt _ -> TInt
  | LFloat _ -> TFloat
  | LBool _ -> TBool
  | LString _ -> TString
  | LEnum (t, _) -> begin match value t with Id tstr -> TEnum tstr end
  | LVec2 _ -> TVec2
  | LVec3 _ -> TVec3

(* Returns true iff types are the same *)
let type_is_same t0 t1 = match t0, t1 with
  | TUnknown, _ -> true
  | _, TUnknown -> true
  | _, _ -> t0 = t1

(* Returns true iff type list is same *)
let type_list_is_same tl0 tl1 = begin match List.for_all2 type_is_same tl0 tl1 with
  (* not the same size. different *)
  | exception Invalid_argument _ -> false
  (* return the result *)
  | same -> same
end

(* Returns true iff function types are the same *)
let type_function_is_same ft0 ft1 =
  (* deconstruct and get return types *)
  let r0, a0 = ft0 in
  let r1, a1 = ft1 in
  (* compare return types *)
  (r0 == r1) &&
  (* compare arguments *)
  begin match List.for_all2 type_is_same a0 a1 with
    (* not the same size. different *)
    | exception Invalid_argument _ -> false
    (* return the result *)
    | same -> same
  end

(* Returns true iff types represent a number *)
let type_is_number t = match t with
  | TFloat | TInt -> true
  | _-> false

(* Returns the type with least assumption carried with it *)
let type_least_assumption t0 t1 = match t0, t1 with
  (* If one type is unknown, the least assumption one is the unknown one*)
  | TUnknown, _ -> TUnknown
  | _, TUnknown -> TUnknown
  (* If the types are equal, and none are unknown, return it *)
  | _ when type_is_same t0 t1  -> t0
  (* Can't work on different types *)
  | _ -> raise (Invalid_argument "t0 and t1 in type_least_assumption must be equivalent")

(* Get a list of valid casts from type *)
let type_valid_casts_from t =
  (* if type is unknown, return nothing *)
  if t = TUnknown then []
  else
  (* add the nonident casts*)
  let nonident_casts = match t with
    (* An int can be converted to a float *)
    | TInt -> [ TFloat ]
    (* A float can be converted to an int *)
    | TFloat -> [ TInt ]
    (* Other types can't be casted yet *)
    | _ -> [] in
  (* now, add the identity cast, that is t -> t*)
  t::nonident_casts

(* Check if a typecast is valid*)
let type_is_valid_cast fromtype totype =
  (* If type is unknown, it is ok *)
  if fromtype = TUnknown then true
  else
  (* Get the list of valid casts *)
  let valid_casts = type_valid_casts_from fromtype in
  (* Checs if dest type is in the list *)
  List.mem totype valid_casts

(* Returns the signatures allowed for an operator *)
let type_op_signature = function
  (* the + operator *)
  | OpPlus -> [
      (* numbers *)
      type_of_function TInt [TInt; TInt];
      type_of_function TFloat [TFloat; TInt];
      type_of_function TFloat [TInt; TFloat];
      type_of_function TFloat [TFloat; TFloat];
      (* string concatenation *)
      type_of_function TString [TString; TString];
      (* vector addition *)
      type_of_function TVec2 [TVec2; TVec2];
      type_of_function TVec3 [TVec3; TVec3];
    ]
  (* the - operator *)
  | OpMinus -> [
      (* numbers *)
      type_of_function TInt [TInt; TInt];
      type_of_function TFloat [TFloat; TInt];
      type_of_function TFloat [TInt; TFloat];
      type_of_function TFloat [TFloat; TFloat];
      (* vector addition *)
      type_of_function TVec2 [TVec2; TVec2];
      type_of_function TVec3 [TVec3; TVec3];
    ]
  (* the * operator *)
  | OpStar -> [
      (* numbers *)
      type_of_function TInt [TInt; TInt];
      type_of_function TFloat [TFloat; TInt];
      type_of_function TFloat [TInt; TFloat];
      type_of_function TFloat [TFloat; TFloat];
      (* string repetition *)
      type_of_function TString [TInt; TString];
      (* 2D vector scaling *)
      type_of_function TVec2 [TInt; TVec2];
      type_of_function TVec2 [TFloat; TVec2];
      type_of_function TVec2 [TVec2; TInt];
      type_of_function TVec2 [TVec2; TFloat];
      (* 3D vector scaling *)
      type_of_function TVec3 [TInt; TVec3];
      type_of_function TVec3 [TFloat; TVec3];
      type_of_function TVec3 [TVec3; TInt];
      type_of_function TVec3 [TVec3; TFloat];
    ]
  (* the / operator *)
  | OpDivide -> [
      (* numbers *)
      type_of_function TInt [TInt; TInt];
      type_of_function TFloat [TFloat; TInt];
      type_of_function TFloat [TInt; TFloat];
      type_of_function TFloat [TFloat; TFloat];
      (* 2D vector scaling *)
      type_of_function TVec2 [TVec2; TInt];
      type_of_function TVec2 [TVec2; TFloat];
      (* 3D vector scaling *)
      type_of_function TVec3 [TVec3; TInt];
      type_of_function TVec3 [TVec3; TFloat];
    ]
  (* the and, or operators *)
  | OpAnd | OpOr -> [ type_of_function TBool [TBool; TBool] ]
  (* the = and != operators *)
  | OpEqual | OpNotEqual -> [ type_of_function TBool [TUnknown; TUnknown] ]
  (* the comparison operators *)
  | OpLeq | OpGeq | OpLess | OpMore -> [
      (* numbers *)
      type_of_function TBool [TInt; TInt];
      type_of_function TBool [TFloat; TInt];
      type_of_function TBool [TInt; TFloat];
      type_of_function TBool [TFloat; TFloat];
    ]

(* Check if an operation is valid*)
let type_is_valid_op lhs op rhs =
  (* Get the list of signatures for current operator *)
  let signatures = type_op_signature op in
  (* The predicate function used *)
  let predicate = (function signature ->
    (* decnstruct the signature *)
    match signature with
      (* if the list of args has two arguments, test *)
      | _, [l; r] -> (type_is_same lhs l) && (type_is_same rhs r)
      (* error *)
      | _ -> false
  ) in
  (* Checks if there is a function that takes [rhs; lhs] as an argument *)
  List.exists predicate signatures

let type_return_op lhs op rhs =
  (* Get the list of signatures for current operator *)
  let signatures = type_op_signature op in
  (* The predicate function used *)
  let predicate = (function signature ->
    (* decnstruct the signature *)
    match signature with
      (* if the list of args has two arguments, test *)
      | _, [l; r] -> (type_is_same lhs l) && (type_is_same rhs r)
      (* error *)
      | _ -> false
  ) in
  (* Checks if there is a function that takes [rhs; lhs] as an argument *)
  match List.find_opt predicate signatures with
    (* no signature found ; invalid lhs and rhs *)
    | None -> raise (Invalid_argument "lhs and rhs types not compatible with operator")
    (* a signature was found ; return the return type *)
    | Some (ret, _) -> ret
