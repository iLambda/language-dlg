(* Writes an int32 to a buffer *)
val int32_to_buf : int32 -> Buffer.t
(* Writes an int64 to a buffer *)
val int64_to_buf : int64 -> Buffer.t
(* Writes a float to a buffer *)
val float_to_buf : float -> Buffer.t
(* Writes a boolean to a buffer *)
val bool_to_buf : bool -> Buffer.t
(* Writes a string to a buffer *)
val str_to_buf : string -> Buffer.t

(* Converts an int to a byte list *)
val byte_list_of_int64 : int64 -> int list

(* Create a buffer from bytes *)
val from_bytes : bytes -> Buffer.t
(* Create a buffer from a byte list *)
val from_byte_list : int list -> Buffer.t

(* Deconstruct a flag  *)
val deconstruct_flag : int -> int list
(* Create a byte from a list *)
val byte_from_list : int list -> bytes

(* Converts a buffer to a string *)
val to_string : Buffer.t -> string
