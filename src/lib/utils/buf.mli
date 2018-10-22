(* Writes an int32 to a buffer *)
val int32_to_buf : int32 -> Buffer.t
(* Writes a float to a buffer *)
val float_to_buf : float -> Buffer.t
(* Writes a boolean to a buffer *)
val bool_to_buf : bool -> Buffer.t

(* Create a buffer from bytes *)
val from_bytes : bytes -> Buffer.t
(* Create a buffer from a byte list *)
val from_byte_list : int list -> Buffer.t

(* Create a byte from a list *)
val byte_from_list : int list -> bytes
