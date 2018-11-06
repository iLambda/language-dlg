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
type scope = Local | Global | Extern
(* An identifier *)
type identifier = string
(* A scoped identifier *)
type scoped_identifier = scope * identifier

(* Some data *)
type data =
  | Value of value
  | Identifier of scoped_identifier

(* A type exception *)
type data_type_exception = {
  expected: string;
  token: data;
}
(* An exception *)
exception Wrong_data_type of data_type_exception
(* Make data type *)
let make_type_error expected token = Wrong_data_type ({
  expected=expected;
  token=token;
})

(* Coercive operators *)
let value_of_data = function Value x -> x | d -> raise (make_type_error "any value" d)
let int_of_data = function Value (VInt x) -> x | d -> raise (make_type_error "int" d)
let float_of_data = function Value (VFloat x) -> x | d -> raise (make_type_error "float" d)
let bool_of_data = function Value (VBool x) -> x | d -> raise (make_type_error "bool" d)
let string_of_data = function Value (VString x) -> x | d -> raise (make_type_error "string" d)
let vec2_of_data = function Value (VVec2 (x, y)) -> (x, y) | d -> raise (make_type_error "vec2" d)
let vec3_of_data = function Value (VVec3 (x, y, z)) -> (x, y, z) | d -> raise (make_type_error "vec3" d)
let id_of_data = function Identifier id -> id | d -> raise (make_type_error "int" d)
let number_of_data = function
  | Value (VFloat x) -> x
  | Value (VInt x) -> Int32.to_float x
  | d -> raise (make_type_error "float or int" d)

let data_of_int i = Value (VInt i)
let data_of_float f = Value (VFloat f)
let data_of_bool b = Value (VBool b)
let data_of_string s = Value (VString s)
let data_of_vec2 x y = Value (VVec2 (x, y))
let data_of_vec3 x y z = Value (VVec3 (x, y, z))
let data_of_value v = Value v

(* inline a value *)
let inline_string_of_data = function
  | Value v -> string_of_value v
  | d -> raise (make_type_error "any value" d)
